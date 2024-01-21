;;; early-init.el --- Pre-GUI and pre-package-initialization config -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The 'early-init.el'
;; ===================
;; Mostly used to:
;; - set critical paths
;; - increase speed by presetting GUI, rather than fixing after load
;; - after it, Emacs inits packs, unless package-enable-at-startup is nil
;; 'https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html'
;;
;; I am strongly inspired by by
;; 'https://raw.githubusercontent.com/doomemacs/doomemacs/master/early-init.el'
;;
;; BloomR specifics
;; ================
;; This file is found as internally bloomr-*.exe '--initi-dir' calls
;;   apps\bremacs\bin\runemacs.exe --initi-dir apps\bremacs\.emacs.d
;;
;; For a more informative CLI based launcher use 'apps\ed\bremacs.ed.cmd'.
;; For a debug version of BloomR/BRemacs use 'apps\ed\bremacs-dbg.ed.cmd'.
;;
;; To make BloomR friendly, standard BR/Emacs dirs are set outside the 'HOME' dir.
;; The HOME dir is set via an env variable by the launcher as '%BLOOMR%/mybloomr'.
;; the variable 'BLOOMR', also set by the launcher, is the setup root dir.
;;

;;; Code:


;;; BloomR specifics
;;; ================


;;; Early stage errors
;; At this stage it seems they can only be detected by 'emacs.exe'.
;; which is used by 'apps\ed\bremacs-dbg.cmd', rather than 'runemacs.exe',
;; To catch them we use:
;;   (message "error")
;;   (kill-emacs 1)


;; TST: Test we got the proper path for `user-emacs-directory'
(let ((user-emacs-directory-wanted (expand-file-name (concat invocation-directory "../.emacs.d/"))))
  (unless (file-equal-p user-emacs-directory-wanted user-emacs-directory)
    (message "BR/Emacs user directory is \n%s, but expected value was \n%s"
	     user-emacs-directory user-emacs-directory-wanted)
    (kill-emacs 1)))
 
;; TST: Test BR/emacs setup dir with respect to received environment variable
(let ((setup-wanted (expand-file-name(concat invocation-directory "../../.."))); BloomR/apps/bremacs/bin
      (setup-env (or (getenv "BLOOMR") "")))
  (unless (file-equal-p setup-wanted setup-env)
    (message "BR/Emacs setup directory riceved is \n%s, but expected value was \n%s"
	     setup-env setup-wanted)
    (kill-emacs 1)))
 
;; Redirect `package-user-dir' to `user-emacs-directory'.
;; It would be otherwise in the HOME:
(setq package-user-dir (concat (file-name-as-directory user-emacs-directory) "elpa"))
 
;; If BloomR debug launcher 'apps\ed\bremacs-dbg.ed.cmd' was used,
;; env var 'BREMACSDBG' is set.
(defconst br-debug (string= "1" (getenv "BREMACSDBG")))

 
;;; General optimisations 
;;; =====================
 
;; TL;DR: Unset this variable in case of startup issues 
(defconst br-file-handler-optimize t)
 
;; PERF: `file-name-handler-alist' is consulted on each call to `require',
;;   `load', or various file/io functions (like `expand-file-name' or
;;   `file-remote-p'). We get a notable startup boost by unsetting its value.
;;   The handler is necessary (also) to read gzipped files.
;;   BloomR has been byte compiled so there should be no gzips to read.
;;   BTW, a package might attempt this, hence see/apply the TL;DR above
;;   The same is true for `load-file-rep-suffixes'
 
 
;; PERF: Don't be verbose unleess we are debugging
(when br-debug
  (setq init-file-debug t
	debug-on-error t))
 
;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but needs to be reset later. Not resetting it can cause stuttering/freezes.
(let ((gc-cons-threshold-old gc-cons-threshold))
  (setq gc-cons-threshold (* 50 1000 1000)) ; about 50mb 
  (add-hook 'after-init-hook (lambda () (setq  gc-cons-threshold gc-cons-threshold-old))))
 
;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
;;   larger than the system font) appears to impact startup time
;;   dramatically. The larger the delta in font size, the greater the delay.
;;   Even trivial deltas can yield a ~1000ms loss, though it varies wildly
;;   depending on font size.
(setq frame-inhibit-implied-resize t)
 
;; PERF: Frame resize and fonts	 affects performace, they are better set here
(set-face-attribute 'default  nil :height 146)
(push '(fullscreen . maximized) initial-frame-alist)
 
 
;; UX: Don't resize the frames in steps a bit slower.
(setq frame-resize-pixelwise t) 
 
;; PERF,UX: Reduce *Message* noise at startup.
(setq inhibit-startup-screen t ;  NB `inhibit-splash-screen' is an alias for this      
      inhibit-startup-echo-area-message user-login-name)
;; PERF,UX: Remove "For information about GNU Emacs..." message at startup.
;;   as it incurs a premature redraw.
(advice-add #'display-startup-echo-area-message :override #'ignore)
;; PERF: Suppress the vanilla startup screen completely. We've disabled it
;;   with `inhibit-startup-screen', but it would still initialize anyway.
;;   This involves some file IO and/or bitmap work (depending on the frame
;;   type) that we can no-op for a free 50-100ms boost in startup time.
(advice-add #'display-startup-screen :override #'ignore)
 
;; PERF: Shave seconds off startup time by starting the scratch buffer in
;;   `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
;;   pull in a ton of packages. `doom/open-scratch-buffer' provides a better
;;   scratch buffer anyway.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
 
 
;; PERF: The mode-line procs a couple dozen times during startup. This is
;;   normally quite fast, but disabling the default mode-line and reducing the
;;   update delay timer seems to stave off ~30-50ms.
(put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
(setq-default mode-line-format nil)
(dolist (buf (buffer-list))
  (with-current-buffer buf (setq mode-line-format nil)))
 
;; PERF,UX: Premature redisplays can substantially affect startup times and
;;   produce ugly flashes of unstyled Emacs.
(setq-default inhibit-redisplay t
	      inhibit-message t)
 
 
;; COMPAT: Reset `mode-line-format', `inhibit-redisplay', `inhibit-message'
;;	   via advice funcs.  `startup--load-user-init-file' doesn't interrupt
;;	   on errors, but these settings can make Emacs frozen or garbled.
(defun br--reset-inhibited-vars-h ()
  "Hook to resore the mode-line after early init.
Also used in advicing `startup--load-user-init-file'."
  (unless br-debug (kill-buffer "*Messages*")); because ugly without modeline
  (setq-default inhibit-redisplay nil
		;; Inhibiting `message' only prevents redraws and
		inhibit-message nil)
  (redraw-frame))
(add-hook 'after-init-hook #'br--reset-inhibited-vars-h)
(define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
  (when init-file-had-error
    (br--reset-inhibited-vars-h))
  (unless (default-toplevel-value 'mode-line-format)
    (setq-default mode-line-format (get 'mode-line-format 'initial-value))))
 
;; Disable MacOS or X Window specific options 
(setq command-line-ns-option-alist nil)
(unless (memq initial-window-system '(x pgtk))
  (setq command-line-x-option-alist nil))
 
;;; Reasonable defaults for interactive sessions
 
;;; Runtime optimizations
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)
 
;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)
 
;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only
 
;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
 
;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)
 
;; Don't ping things that look like domain names.
(setq-default ffap-machine-p-known 'reject)
 
;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5
 
;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)
 
 
;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb
 
;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)
 
;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil    ; decrease file IO workload
	w32-pipe-read-delay 0		    ; faster IPC
	w32-pipe-buffer-size (* 64 1024)))  ; read more at a time (was 4K)
 
 
;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)
;; ...And the clipboard on Windows could be in a wider encoding (UTF-16), so
;; leave Emacs to its own devices.
(when (memq system-type '(cygwin windows-nt ms-dos)) 
  (setq selection-coding-system 'utf-8))
 
 
;; UX: Scroll better
(setopt scroll-conservatively 10000	  ; keyboard scroll: high number causes one line at a time
	scroll-preserve-screen-position t); page up and down, find the original line
 
 
;; PERF: Emacs 27+ calls (package-initialize) before processing the
;;    standard init file unless:
(setq package-enable-at-startup nil)

;;; Bootstrap BRemacs library
(defun br-bremacs-boot-hk ()
  "Bootstrap BRemacs library with `br-init.elc'.
br-init will in turn call own and third party dependecies.
If the launcher set `br-debug', boot with `br-init-dbg.el' instead.
The function is run as a hook for the reason explained below.

GUI customizations related (e.g. ESS toolbar) do not work in `early-init.el'.
That's due to the file being read before the GUI is initialized.
There are two possibilities:
- continue with a standard init file, e.g. `init.el', `site-start.el';
- use a hook run when GUI is initialized.
Here we opt for the second.  See also:
https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
https://www.gnu.org/software/emacs/manual/html_node/elisp/Init-File.html"

  (let (; See `br-file-handler-optimize' docstring for info on settings below
	(file-name-handler-alist (if br-file-handler-optimize nil file-name-handler-alist))
	(load-file-rep-suffixes (if br-file-handler-optimize '("") load-file-rep-suffixes)))
    
    (let* ((bremacs-lib (concat invocation-directory "../share/emacs/site-lisp/bremacs/"))
	   (br-init     (concat bremacs-lib "br-init.elc")) 
	   ;; To simplify debug, BloomR build removes 'br-init-dbg.elc'
	   (br-init-dbg (concat bremacs-lib "br-init-dbg.el")) 
	   (missing (or (unless (file-exists-p br-init)	  br-init)
			(unless (file-exists-p br-init-dbg) br-init-dbg)))
	   what-init)
      
      (when missing (error "Missing init file: \n%s" (expand-file-name missing)))
      (if br-debug (message "Debug flag set!"))
      (setq what-init (if br-debug br-init-dbg br-init))
      (load-file what-init)))) 

(defun br-startup-time-hk ()
  "Measure and prints the overall BloomR boostrasp time."
  (message "Booted in %s with %d gc's."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))


;;; Run hooks at due when GUI is ready
;; Possible hooks are: 1) `before-init-hook', 2) `after-init-hook',
;; 3) `emacs-startup-hook', 4) `window-setup-hook'
;; Hooks are run as LIFO, so this is run second
(if br-debug
    (add-hook 'window-setup-hook
	      #'(lambda()
		 (message "`early-init.el' started \n%s" 
			  (expand-file-name
			   (concat invocation-directory
				   "../share/emacs/site-lisp/bremacs/br-init-dbg.el")))))
  (add-hook 'window-setup-hook 'br-startup-time-hk))

;; ...and this is run first
(add-hook 'window-setup-hook 'br-bremacs-boot-hk)

;;; early-init.el ends here
