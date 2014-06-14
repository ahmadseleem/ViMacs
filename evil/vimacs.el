;; ViMacs
;;
;; My vim keys and more...
;;

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'evil-states)
(require 'evil-ex)
(require 'evil-commands)



;; ===========================================================================
;; -- Reload Emacs Config - without restarting ------------
;;

(defun edit-dot-emacs ()
  "Load the .emacs file into a buffer for editing."
  (interactive)
  (find-file "~/.emacs.d/evil/vimacs.el"))

;; TODO ... doesn't work yet
(defun reload-dot-emacs ()
  "Save .emacs, if it is in a buffer, and reload it."
  (interactive)
  (if (bufferp (get-file-buffer "~/.emacs"))
    (save-buffer (get-buffer "~/.emacs")))
  (load-file "~/.emacs"))

;; TODO ... doesn't work yet
(defun reload-vimacs ()
  "Save .emacs, if it is in a buffer, and reload it."
  (interactive)
  (if (bufferp (get-file-buffer "~/.emacs.d/evil/vimacs.el"))
    (save-buffer (get-buffer "~/.emacs.d/evil/vimacs.el")))
  (load-file "~/.emacs.d/evil/vimacs.el"))


;; ===========================================================================
;; -- Key - Bindings --------------------------
;;

; Keep some universal Emacs/OSX key bindings...
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
;(define-key evil-insert-state-map "\C-n" 'evil-next-line)
;(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-k" 'kill-line)
(define-key evil-insert-state-map "\C-k" 'kill-line)
(define-key evil-visual-state-map "\C-k" 'kill-line)

; some keymaps from ~/.vimrc
(define-key evil-normal-state-map ",w" 'save-buffer) ; save
(define-key evil-normal-state-map ",q" 'kill-buffer) ; quit (current buffer; have to press RETURN)
(define-key evil-normal-state-map ",x" 'save-buffers-kill-emacs) ; save and quit

; Comment Lines ...
;(define-key evil-normal-state-map (kbd "C-c") 'comment-dwim) ; Comment Lines (Visual)
; [ CMD + / ]  ; => works

; allow us to access org-mode keys directly from Evil's Normal mode
(define-key evil-normal-state-map "L" 'org-shiftright)
(define-key evil-normal-state-map "H" 'org-shiftleft)
(define-key evil-normal-state-map "K" 'org-shiftup)
(define-key evil-normal-state-map "J" 'org-shiftdown)
(define-key evil-normal-state-map (kbd "M-l") 'org-metaright)
(define-key evil-normal-state-map (kbd "M-h") 'org-metaleft)
(define-key evil-normal-state-map (kbd "M-k") 'org-metaup)
(define-key evil-normal-state-map (kbd "M-j") 'org-metadown)
(define-key evil-normal-state-map (kbd "M-L") 'org-shiftmetaright)
(define-key evil-normal-state-map (kbd "M-H") 'org-shiftmetaleft)
(define-key evil-normal-state-map (kbd "M-K") 'org-shiftmetaup)
(define-key evil-normal-state-map (kbd "M-J") 'org-shiftmetadown)

;; easy keys to split window. Key based on ErgoEmacs keybinding
;;;(global-set-key (kbd "M-3") 'delete-other-windows)  ; expand current pane
(global-set-key (kbd "M-2") 'delete-window)         ; close current pane
(global-set-key (kbd "M-s") 'other-window)          ; cursor to other pane

(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))


;; ===========================================================================
;; -- General --------------------------
;;

(setq
 tab-always-indent 'complete            ;try to complete before identing
 show-paren-delay 0
 )

(setq evil-shift-width 2)

(put 'narrow-to-region 'disabled nil)   ;narrow to region should be enabled by default
;;

(global-set-key (kbd "C-x f") 'find-file-in-project)

(setq default-directory "~/Documents/" )
(set-default-font "Inconsolata LGC-16") ;(set-default-font "DejaVu Sans Mono-17")
(menu-bar-mode 1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq scroll-step 1)

(set-frame-width (selected-frame) 110)

;; Project find ..........................................
(setq ns-pop-up-frames nil)

;; Vertical ido-mode ..................................
(add-to-list 'load-path "~/.emacs.d/elpa/ido-vertical-mode")
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)

;; New Frame ..........................................
(add-to-list 'default-frame-alist
             '(font . "Inconsolata LGC-16"))

(add-to-list 'default-frame-alist
            '(vertical-scroll-bars . nil))

;; Backup .............................................
;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; Cursor .............................................
(define-key evil-normal-state-map "|" 'org-cycle-list-bullet) ; change bullet style

;; Esc => kj ........... as Vim ..............................
;;(define-key evil-insert-state-map  "jj" 'evil-normal-state)
(define-key evil-insert-state-map "k" #'cofi/maybe-exit)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))
;; --------------------------------------------------------------------------

;; ................................................................
;; Auto-indent...
(add-to-list 'load-path "~/.emacs.d/elpa/auto-indent-mode")
(require 'auto-indent-mode)
(auto-indent-global-mode)

(setq auto-indent-assign-indent-level 2) ; => indent level to 2 spaces.

(electric-indent-mode +1)

;; Tab Settings
;; No tabs! - spaces only .........................................
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq c-basic-indent 2)
(setq css-indent-offset 2)
(setq html-indent-offset 2)
(setq haml-indent-offset 2)
(setq coffee-tab-width 2)
(custom-set-variables '(coffee-tab-width 2))

;; Coffee-mode .........................................................
(add-to-list 'load-path "~/.emacs.d/elpa/coffee-mode")
(require 'coffee-mode)
 

;; indent .........................................................
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-char "∙")

;; ................................................................
;; ;; clipboard ...
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files
  
;; Zoom In/Out ..................................................
(defun zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
      		      :height
		      (+ (face-attribute 'default :height)
		         10)))

(defun zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
      		      :height
		      (- (face-attribute 'default :height)
		         10)))
;; change font size, interactively
(global-set-key (kbd "C-+") 'zoom-in)
(global-set-key (kbd "C--") 'zoom-out)

;; Line numbers ..................................................
(global-linum-mode 1)

(add-hook 'abg-code-modes-hook
          (lambda () (linum-mode 1)))

(add-hook 'ruby-mode-hook
          (lambda () (run-hooks 'abg-code-modes-hook)))
          
(add-hook 'neotree-mode-hook
          (lambda () (linum-mode 0)))

(add-hook 'dired-mode-hook
          (lambda () (linum-mode 0)))
          

;; ................................................................

(defun toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )

;; ................................................................
;; Mode Line ...

(setq-default mode-line-format
  (list
    "  "
    `(vc-mode vc-mode)" "
    ;`(:propertize (vc-mode vc-mode) face mode-line-filename-face)

    `(:propertize  ("  * ") face mode-line-filename-face)
    ;'mode-line-buffer-identification
    ;; directory and buffer/file name
    ;;`(:eval (shorten-directory default-directory 20))
    ;`(:propertize "%b" face mode-line-filename-face)
    `"%b"
    
    ;" *  "
    ;; the current major mode for the buffer.
    "                       "
    `(:propertize  ("  # ") face mode-line-filename-face)
    '"%m"
    `(:propertize  (" # ") face mode-line-filename-face)
    ;" ( " ;; '%02' to set to 2 chars at least; prevents flickering
      ;;"%p" " :" "%02l" ":" "%02c"
      ;;"%02l" ", " "%02c"
    ;" ) "
    ))

        
        
;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 2)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-filename-face)

(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'normal)

;; ................................................................
;; ;; Git ...
(global-auto-revert-mode 1)   ; Auto refresh branchs switching.
(setq auto-revert-check-vc-info t)

;; ................................................................
;; ;; clipboard... OSX

;; Override the default x-select-text function:
;;  -> it doesn't respect x-select-enable-clipboard on OS X.
(defun x-select-text (text))
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary nil)
(setq mouse-drag-copy-region nil)
;;
(setq interprogram-cut-function 'ns-set-pasteboard)
(setq interprogram-paste-function 'ns-get-pasteboard)

;;; ................................................................
;; ;; Clipboard bypass : Support

(defmacro without-evil-mode (&rest do-this)
  ;; Check if evil-mode is on, and disable it temporarily
  `(let ((evil-mode-is-on (evil-mode?)))
     (if evil-mode-is-on
         (disable-evil-mode))
     (ignore-errors
       ,@do-this)
       (if evil-mode-is-on
         (enable-evil-mode))))

(defmacro evil-mode? ()
  "Checks if evil-mode is active. Uses Evil's state to check."
  `evil-state)

(defmacro disable-evil-mode ()
  "Disable evil-mode with visual cues."
  `(progn
     (evil-mode 0)
     (message "Evil mode disabled")))

(defmacro enable-evil-mode ()
  "Enable evil-mode with visual cues."
  `(progn
     (evil-mode 1)
     (message "Evil mode enabled")))

;;;; Clipboard bypass
;; delete: char
(evil-define-operator evil-destroy-char (beg end type register yank-handler)
  :motion evil-forward-char
  (evil-delete-char beg end type ?_))

;; delete: char (backwards)
(evil-define-operator evil-destroy-backward-char (beg end type register yank-handler)
  :motion evil-forward-char
  (evil-delete-backward-char beg end type ?_))

;; delete: text object
(evil-define-operator evil-destroy (beg end type register yank-handler)
  "Vim's 's' without clipboard."
  (evil-delete beg end type ?_ yank-handler))

;; delete: to end of line
(evil-define-operator evil-destroy-line (beg end type register yank-handler)
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (evil-delete-line beg end type ?_ yank-handler))

;; delete: whole line
(evil-define-operator evil-destroy-whole-line (beg end type register yank-handler)
  :motion evil-line
  (interactive "<R><x>")
  (evil-delete-whole-line beg end type ?_ yank-handler))

;; paste: after
(defun evil-destroy-paste-after ()
  (interactive)
  (without-evil-mode
     (delete-region (point) (mark))
     (evil-paste-after 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clipboard bypass key rebindings
(define-key evil-normal-state-map "d" 'evil-destroy)
(define-key evil-normal-state-map "D" 'evil-destroy-line)
(define-key evil-normal-state-map "x" 'evil-destroy-char)
(define-key evil-normal-state-map "X" 'evil-destroy-whole-line)
(define-key evil-normal-state-map "Y" 'evil-copy-to-end-of-line)
(define-key evil-visual-state-map "p" 'evil-destroy-paste-after)
;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 

;; ................................................................
;; SCSS Mode...

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/scss-mode"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

(provide 'vimacs)
