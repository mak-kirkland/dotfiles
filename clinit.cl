;;; This file contains examples of potentially interesting user
;;; customizations which can be done via a $HOME/.clinit.cl.

(format *terminal-io* "~% Loading home ~a file.~%"
        *load-pathname*)

;; PRINT STUFF FROM NICK

(tpl:setq-default *print-circle* ())
(tpl:setq-default *print-right-margin* 200)
(setf excl:*trace-print-length* 200
     excl:*trace-print-level* 20
      excl:*trace-print-array* t
      excl:*trace-print-circle* nil)

(setf tpl:*zoom-print-length* 200
      tpl:*zoom-print-level* 20
      tpl:*zoom-print-circle* nil)

(setf excl::*trace-show-effective-method* nil)

;;; Set a few top-level variables.
(tpl:setq-default top-level:*history* 50)
(tpl:setq-default top-level:*print-length* 20)
(tpl:setq-default top-level:*print-level* 5)
(tpl:setq-default top-level:*zoom-print-level* 3)
(tpl:setq-default top-level:*zoom-print-length* 3)
(tpl:setq-default top-level:*exit-on-eof* t)
(tpl:setq-default top-level:*help-page-length* 0)
(tpl:setq-default *print-right-margin* 160)

;;; Display 10 frames on :zoom,
(tpl:setq-default top-level:*zoom-display* 10)
;;; and don't print anything but the current frame on :dn, :up and :find
(tpl:setq-default top-level:*auto-zoom* :current)

;;; Have the garbage collector print interesting stats.
(setf (sys:gsgc-switch :print) nil)
(setf (sys:gsgc-switch :stats) nil)
(setf (sys:gsgc-switch :verbose) nil)
;; make it a lot harder for data to be tenured
(setf (sys:gsgc-parameter :generation-spread) 24)
;; Always GC old space before expanding it
(setf (sys:gc-switch :gc-old-before-expand) t)
;; VDR-403
(setf (sys:gsgc-parameter :free-percent-new) 10)
;; RPA-654
(setf (sys:gc-parameter :expansion-free-percent-old) 70)
(setf (sys:gc-parameter :quantum) 16384)
(tpl:setq-default excl:*tenured-bytes-limit* (expt 2 25))

#+(version>= 10 0)
(setf (sys:gc-parameter :helper-threads-requested)
      (sys:gc-parameter :helper-thread-limit))

;; Dump core automatically on a gc error.
(setf (sys:gsgc-switch :dump-on-error) t)

;; set default dns mode to non allegro lib, as the allegro lib has proven
;; time and time again to be buggy
(setf socket:*dns-mode* :clib)

;; The default value is 2.0, which seems high for modern machines.
;; The value is the number of seconds this process can hold the lisp
;; heap before it will allow other ready processes a chance to run.
(setq mp:*default-process-quantum* 0.1)

;;; To have all advice automatically compiled.
(tpl:setq-default *compile-advice* t)

;;; Have packages print with their shortest nickname instead of the package
;;; name.
(tpl:setq-default *print-nickname* t)
;;; VDR-395
#+(version>= 10 0)
(tpl:setq-default excl:*print-alternate-package-name* t)

;;; Allow concise printing of shared structure.
(tpl:setq-default *print-circle* nil)

;;; Only print "Compiling" messages for files, not for individual functions,
;;; unless there is a warning or error.
(tpl:setq-default *compile-verbose* t)
(tpl:setq-default *compile-print* nil)

#+mswindows
(def-fwrapper console-control-wrap (&rest args &key title &allow-other-keys)
  (when title
    (setf (getf arglist :title)
            (format nil "~d: ~a" (excl.osi:getpid) title)))
  (call-next-fwrapper))

#+mswindows
(fwrap 'excl:console-control 'cc-wrap-1 'console-control-wrap)

(top-level:alias ("load-system" 7) (system)
  ":load-sys will load and compile a system"
  (excl:load-system system :compile t))

(top-level:alias ("clean-system" 8) (system)
  ":clean-sys will load and compile a system"
  (excl:clean-system system))

(top-level:alias ("pkg-alias" 1) (package &rest alias)
  ":pkg-alias adds a nickname to a package"
  (rename-package package (package-name package)
                  (append alias (package-nicknames package))))

(top-level:alias "pid" ()
  ":pid prints and returns the current os process id"
  (let ((pid (excl::getpid)))
    (princ pid)
    pid))


;; Allow emacs to attach to an already running lisp image.
(format *terminal-io* "Starting ACL~a Emacs Lisp Interface~%"
        (subseq (lisp-implementation-version) 0 3))

(defvar *separator* (excl:path-namestring "/"))
(defvar *home*      (string-right-trim *separator*
                     (or (sys:getenv "HOME")
                         (concatenate 'string
                                      (sys:getenv "HOMEDRIVE")
                                      (sys:getenv "HOMEPATH")))))

#+(version>= 9 0)
(progn
  (setf (synchronized-output-stream-p *error-output*) t)
  (setf (synchronized-output-stream-p *trace-output*) t))


;; VDR-305
(setf (sys::thread-control :logging) t)

(warn "alisp PID: ~a, eli pw: ~a"
      (excl::getpid)
      excl::*emacs-daemon-password*)

(warn "current git commit: ~a"
      (first (excl.osi:command-output "git rev-parse HEAD")))

(push :load-app-only *features*)

(let ((clinit (merge-pathnames "workspace/ravenpack/system.lisp" (user-homedir-pathname))))
  (if (probe-file clinit)
      (load clinit)
    (warn "Cannot load ~s: file not found." clinit)))

;; Michael Custom

;; (asdf:clear-configuration)
;; (setf asdf:*central-registry* (list (namestring (user-homedir-pathname)))
;;       asdf:*user-cache*       (merge-pathnames ".cache/common-lisp/acl/"
;;                                                  (user-homedir-pathname)))
(push (concatenate 'string (sys:getenv "HOME") "/git/sly/slynk/")
      asdf:*central-registry*)
(asdf:load-system :slynk)

(defun joaot/find-symbol-safe (name package)
  (and (find-package package) (find-symbol name package)))

(defun joaot/set-symbol-safe (name package value)
  (let ((sym (joaot/find-symbol-safe name package)))
    (cond (sym
           (format *trace-output* "Locally (SETQ ~s ~s)~%"
                   sym value)
           (setf (symbol-value sym) value))
          (t
           (format *trace-output* "Could not set symbol ~a in package ~s~%"
                   name package)))))

(excl:without-package-locks
  (defmethod excl:load-system :after ((system symbol) &rest keys)
    (joaot/set-symbol-safe "*BREAK-ON-ERRORS*" :utils :unless-recoverable)
    (joaot/set-symbol-safe "*TEST-MONITORING-ENVIRONMENT-P*" :mis.utils.monitoring nil)))
    (joaot/set-symbol-safe "*SKIP-VALIDATION*" :ana t)
    (joaot/set-symbol-safe "*GET-FROM-STORY-CACHE*" :ana nil)
    (joaot/set-symbol-safe "*DB-CACHE-FORMAT*" :dbu :fasl)
    ;;(joaot/set-symbol-safe "*DB-CACHE-DIR*" :dbu "~/Data/db-query-results/%Y-%W/")
