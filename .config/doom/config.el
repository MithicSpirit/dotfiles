;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Basic Configuration
(setq
 user-full-name "MithicSpirit"
 user-mail-address "rpc01234@gmail.com")
(set-language-environment "UTF-8")

;; PATHs
(setq
 org-directory "~/documents/org"
 bookmark-default-file "~/.config/doom/bookmarks"
 auth-sources '("~/.bak/.secrets/authinfo.gpg")
 projectile-project-search-path '("~/documents/coding"
                                  "~/documents/coding/practice"
                                  "~/documents/coding/langs"
                                  "~/src"))
   
 


;;; Appearance
;; Fonts
(setq
 doom-font (font-spec :family "Iosevka Mithic" :size 15)
 doom-variable-pitch-font (font-spec :family "Overpass" :size 16)
 doom-big-font (font-spec :family "Inconsolata" :size 20)
 mode-line-font (font-spec :family "Iosevka Mithic" :size 18))
(setq doom-unicode-font (font-spec :family "Iosevka Mithic"))

;; Theme
(setq doom-nord-brigher-modeline t
      doom-nord-brighter-comments t
      doom-nord-comment-bg nil
      doom-theme 'doom-nord)
(custom-set-faces! ;doom-nord:
  '(fringe :inherit line-number)
  `(line-number-current-line :foreground ,(doom-color 'cyan) :weight bold)
  `(font-lock-comment-face :slant italic))
;(custom-set-faces! ;doom-palenight:
;  '(fringe :inherit line-number)
;  `(font-lock-comment-face :foreground ,(doom-lighten 'base5 .1) :slant italic)
;  `(line-number-current-line :foreground ,(doom-color 'yellow) :weight bold)
;  '(show-paren-match :background nil)
;  )

;; Doom modeline
(setq
 doom-modeline-modal-icon nil
 evil-emacs-state-tag    (propertize "⟨EMACS⟩ ")
 evil-insert-state-tag   (propertize "⟨INSERT⟩")
 evil-motion-state-tag   (propertize "⟨MOTION⟩")
 evil-normal-state-tag   (propertize "⟨NORMAL⟩")
 evil-operator-state-tag (propertize "⟨OPERTR⟩")
 evil-visual-state-tag   (propertize "⟨VISUAL⟩")
 evil-replace-state-tag  (propertize "⟨REPLCE⟩")
 doom-modeline-height 36)
;(custom-set-faces! ;doom-palenight:
;  '(doom-modeline-bar :background nil)
;  `(solaire-mode-line-face :foreground ,(doom-darken 'fg .1)
;                           :background ,(doom-darken 'base3 .16)
;                           )
;  `(solaire-mode-line-inactive-face :background ,(doom-darken 'base2 .1))
;  `(mode-line :foreground ,(doom-darken 'fg .1)
;              :background ,(doom-darken 'base3 .16)
;              :font ,mode-line-font
;              )
;  `(mode-line-inactive :background ,(doom-darken 'base2 .1))
;  )

;; Custom ligatures
(plist-put! +ligatures-extra-symbols
            :map        "→"
            :not        "¬"
            :return     "↦"
            :yield      "↤"
            :union      "∪"
            :tuple      "⊗"
            :pipe       "𝕡")

;; Fancy org-mode
(after! org-superstar
  (setcar (last org-superstar-headline-bullets-list) '(?∙))
  (setcar (cdr org-superstar-item-bullet-alist) '(?+ . ?→))
  (org-superstar-restart))
(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("⣿" "⣶" "⣤" "⣀")))
(setq org-ellipsis " ▼")
(custom-set-faces! `(org-ellipsis :foreground ,(doom-color 'base7))
 `(org-scheduled-today :foreground ,(doom-darken (doom-color 'fg) .1))
 `(org-scheduled-previously :foreground ,(doom-color 'fg)))
 

;; Emojify
(defun +emojify-ignore-emoji (text _beg _end)
  "Prevents `emojify' from displaying certain emojis.

This must be added to `emojify-inhibit-functions' to work."
  ;(message "+emojify-ignore-emoji: Ran with (%s) (%s) (%s)" text _beg _end)
  (if (or
       (string= text "↕")
       (string= text "✔")
       (string= text "✖")
       (string= text "™")
       (string= text "▪")
       (string= text "⚫"))
      t nil))
      
(after! emojify
  (setq
   emojify-point-entered-behaviour 'uncover
   emojify-emoji-set "twemoji-v2-22"
   emojify-program-contexts '(comments string))
  (push #'+emojify-ignore-emoji (cdr (last emojify-inhibit-functions))))
  

;; Misc - Appearance
(setq
 ;; all-the-icons-scale-factor 1.2
 doom-themes-neotree-enable-variable-pitch nil
 evil-motion-state-cursor 'box
 truncate-string-ellipsis "…"
 magit-define-global-key-bindings nil)
(fringe-mode 0)



;;; Utility
;; Misc - Utility
(setq
 display-line-numbers-type t
 fill-column 80
 org-agenda-dim-blocked-tasks nil
 org-agenda-tags-column 0
 delete-by-moving-to-trash t
 lsp-auto-guess-root t
 lsp-enable-suggest-server-download nil
 browse-url-generic-program "dmenu-browser")
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(add-hook! 'text-mode-hook #'auto-fill-mode)
(add-hook! 'prog-mode-hook #'display-fill-column-indicator-mode)
(global-hl-todo-mode)
(add-hook! 'org-agenda-mode-hook #'doom-disable-show-paren-mode-h)
(add-hook! 'rainbow-mode-hook (hl-line-mode (if rainbow-mode -1 +1)))
;(add-hook! 'evil-local-mode-hook 'turn-on-undo-tree-mode)

;; Unbindings
(map! (:map esc-map
       "M-ESC" nil
       ":" nil)
      (:map global-map
       "M-ESC" nil)
      (:after dired :map dired-mode-map
       "q" nil
       :n "q" nil)
      (:after pdf :map pdf-view-mode-map
       "q" nil
       "<normal-state> q" nil)
      (:map mu4e-main-mode-map
       :ne "h" nil)
      (:map mu4e-headers-mode-map
       :vne "l" nil))


;; Autosaving
(setq auto-save-visited-interval 30)
(auto-save-visited-mode)


;; Indentation/Tabs vs. Spaces
(setq highlight-indent-guides-method 'bitmap
      highlight-indent-guides-responsive 'stack)
(setq-default indent-tabs-mode nil)
(add-hook! '(javascript-mode-hook
             typescript-mode-hook
             c++-mode-hook
             sh-mode-hook
             fish-mode-hook)
  (cmd! (setq indent-tabs-mode t)))
  

;; [e]Vi[l] fixes/tweaks
(map! :after evil
      :n "x" "\"_dl"
      :n "X" "\"_dh"
      :n "~" "g~l"
      :nv "gq" #'fill-paragraph)

(setq evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-change-word-to-end nil
      +evil-want-o/O-to-continue-comments nil
      evil-echo-state nil)
      

;; Easier to open org-agenda
(map! :after org
      :leader :desc "Open 10-day agenda" "o a a" #'org-agenda-list

      :map doom-leader-notes-map
      :localleader :desc "Open 10-day agenda"
      "a" #'org-agenda-list)


;; Dired
(after! dired
  (setq dired-listing-switches "-AlhDF --group-directories-first"))
(map! :after dired :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-file)
      

;; LaTeX tweaks and keybinds
(defun +tex-insert-quote (force)
  "Basically `TeX-insert-quote' but closing actually works (it
  skips any quotes present)"
  (interactive "*P")
  (if (and (not force) (string= "''" (buffer-substring (point) (+ 2 (point)))))
      (forward-char 2)
    (TeX-insert-quote force)))
    
(map! :after tex :map LaTeX-mode-map
      (:localleader
       :desc "LaTeX Preview" "v" #'TeX-view
       :desc "Run langtool on current buffer" "l r" #'langtool-check
       :desc "Correct buffer with langtool" "l c" #'langtool-correct-buffer
       :desc "End current langtool session" "l d" #'langtool-check-done
       :desc "Return to langtool" "' e" #'exit-recursive-edit)
      (:i :desc "Insert the appropriate quotation marks for LaTeX"
       "\"" #'+tex-insert-quote))
       
 
(setq
 +latex-indent-level-item-continuation 4
 +latex-viewers '(zathura pdf-tools evince  skim sumatrapdf okular)
 font-latex-script-display '((raise -0.3) raise 0.3)
 font-latex-fontify-script-max-level 2)
 
(setq-default TeX-engine 'luatex)
;(pdf-tools-install 'no-query 'skip-deps)
;(after! tex (setq tex--prettify-symbols-alist
;                  (append tex--prettify-symbols-alist '(("\\implies" . ?⇒))))
(after! (tex flycheck lsp-mode)
 (add-hook! 'latex-mode-local-vars-hook :append
   (add-hook! 'flycheck-mode-hook :local
      (setq-local flycheck-checker 'tex-lacheck)
      (flycheck-add-next-checker 'tex-lacheck 'lsp))))


;; Writeroom tweaks
(after! writeroom-mode (setq writeroom-width 62))
(add-hook! writeroom-mode (cmd! (display-fill-column-indicator-mode -1)))

;; Vterm tweaks (also REPLs)
(map! :map vterm-mode-map :after vterm
      :desc "Send C-u to terminal" "C-u" #'vterm-send-C-u
      :desc "Send Esc to terminal" "C-c <escape>" #'vterm-send-escape
      :desc "Send Esc to terminal" "C-c ESC" #'vterm-send-escape
      :desc "Send C-z to terminal" "C-c C-z" #'vterm-send-C-z
      :desc "Send C-z to terminal" "C-c z" #'vterm-send-C-z
      :desc "Toggle copy mode" "M-<escape>" #'vterm-copy-mode
      :desc "Toggle copy mode" "M-ESC" #'vterm-copy-mode
      :localleader :desc "Toggle copy mode"
      "c" #'vterm-copy-mode)
      
(map! :map vterm-copy-mode-map :after vterm
      :desc "Toggle copy mode" "M-ESC" #'vterm-copy-mode
      :desc "Toggle copy mode" "i" #'vterm-copy-mode
      :localleader :desc "Toggle copy mode"
      "c" #'vterm-copy-mode)
      
(after! vterm
  (add-hook! 'vterm-mode-hook
    (evil-emacs-state)
    (show-paren-mode -1)
    (setq-local evil-emacs-state-cursor '(bar +evil-emacs-cursor-fn)))
    
  (add-hook! 'vterm-copy-mode-hook
    (if vterm-copy-mode (evil-motion-state) (evil-emacs-state))))
    
(setq vterm-always-compile-module t
      vterm-use-vterm-prompt-detection-method t)
      
;; Toggle ligatures
(map! :leader :desc "Ligatures" "t L" #'prettify-symbols-mode

      :map doom-leader-toggle-map
      :localleader :desc "Ligatures"
      "L" #'prettify-symbols-mode)
      

;; Larger popups
(set-popup-rule! "^\\*doom:\\(?:v?term\\|e?shell\\)-popup"
  :vslot -5 :height 0.4 :select t :modeline nil :quit nil :ttl nil)


;; Python stuff
;; (use-package! lsp-jedi)
(setq-hook! 'python-mode-hook
   +format-with-lsp nil
   +format-with 'black
   fill-column 79
   flycheck-checker 'python-flake8)
   
(after! format-all (set-formatter! 'black "black -q -l 79 -"))
(defadvice! +lsp--respect-default-checker-python-mode (orig-fn &rest args)
  "Ensure `flycheck-checker' isn't overwritten by `lsp' in python-mode."
  :around #'lsp-diagnostics-flycheck-enable
  (if (eq major-mode 'python-mode)
      (let ((old-checker flycheck-checker))
        (apply orig-fn args)
        (setq-local flycheck-checker old-checker))
    (apply orig-fn args)))


;; Company/completion
(setq company-idle-delay nil
      company-box-doc-delay 3)
  

;; Map winum window selections to not require "w" prefix
(after! winum
  (map! :map doom-leader-map :leader
        "0" #'winum-select-window-0 ;; don't care about window 10+
        "1" #'winum-select-window-1
        "2" #'winum-select-window-2
        "3" #'winum-select-window-3
        "4" #'winum-select-window-4
        "5" #'winum-select-window-5
        "6" #'winum-select-window-6
        "7" #'winum-select-window-7
        "8" #'winum-select-window-8
        "9" #'winum-select-window-9))
        

;; Email (mu4e)
;(set-email-account!
; "Gmail"
; '((user-mail-address   .  "rpc01234@gmail.com")
;   (smtpmail-smtp-user  .  "rpc01234@gmail.com")
;   (mu4e-sent-folder    .  "/[Gmail].Sent")
;   (mu4e-drafts-folder  .  "/[Gmail].Drafts")
;   (mu4e-trash-folder   .  "/[Gmail].Trash")
;   (mu4e-refile-folder  .  "/[Gmail].All")))

;(setq mu4e-headers-precise-alignment t
;      mu4e-headers-fields
;      '((:account-stripe . 1)
;        (:human-date . 12)
;        (:flags . 6)
;        (:from-or-to . 25)
;        (:subject)))

;(use-package! mu4e-alert
;  :config
;  (setq mu4e-alert-mode-line t
;        doom-modeline-mu4e t
;        mu4e-update-interval nil)

;  (mu4e-alert-enable-mode-line-display))



;;; Heavy customization
;; School agenda
;;(defun school-agenda (&optional kill)
;;  "Open agenda setup for school.

;;Open tasks and agenda with schedule in a sidebar and the
;;calendar in a background buffer."
;;  (interactive
;;   (list (if (string= (buffer-name (current-buffer)) "*doom*") nil t)))

;;  (if kill (call-interactively #'doom/kill-all-buffers))
;;  (cd org-directory)
;;  (evil-edit "schedule.org") (read-only-mode)
;;  (evil-window-vsplit nil "calendar.org")
;;  (evil-edit "tasks.org") (org-shifttab 2)
;;  (evil-window-left 1) (evil-window-set-width 39) ;; schedule
;;  (evil-window-right 1) (evil-window-split) (org-agenda-list) ;; agenda under tasks
;;  (evil-window-set-height 23)
;;  (evil-window-left 1)) ;; schedule

;;(defun =school-agenda (&optional reset)
;;  "Wrapper for `school-agenda' with workspace support.

;;Open workspace for `school-agenda', or switch to one if it
;;already exists. If prefix `reset' is non-`nil', the workspace is
;;reset (all current buffers/windows are killed)."
;;  (interactive "P")
;;  (let ((exists (+workspace-exists-p "*agenda*")))
;;    (+workspace-switch "*agenda*" t)
;;    (when (or (not exists) reset)
;;        (school-agenda reset))))

;;(map!
;; :leader :desc "Open school agenda" "o a s" #'=school-agenda
;; :map doom-leader-notes-map :localleader
;; :desc "Open school agenda" "S" #'=school-agenda

;; :leader :desc "Replace current workspace with agenda" "o a S" #'school-agenda
;; :map doom-leader-notes-map :localleader
;; :desc "Replace current workspace with agenda" "S" #'school-agenda)

;;(map!
;; :map org-mode-map :after org
;; :localleader :desc "Set visibility to 2"
;; "TAB" (cmd! (org-shifttab 2))
;; :localleader :desc "Set visibility to 2"
;; "<tab>" (cmd! (org-shifttab 2)))

;;(setcar (cdr +doom-dashboard-menu-sections)
;;        `("Open school agenda" .
;;          ,(plist-put
;;            (cdr (assoc "Open org-agenda" +doom-dashboard-menu-sections))
;;            :action #'=school-agenda)))
            

;; Org time execute
(defun time-call (time-call &rest args)
  "Time the execution of an org-mode source block"
  (let ((start-time (float-time)))
    (apply time-call args)
    (message "Function call took %f seconds" (- (float-time) start-time))))
    
(defun time-execute ()
  "Execute a source block and time its execution"
  (interactive)
  (advice-add 'org-babel-execute-src-block-maybe :around #'time-call)
  (funcall-interactively 'org-babel-execute-src-block-maybe)
  (advice-remove 'org-babel-execute-src-block-maybe #'time-call))
  
(map!
 :map org-mode-map :after org
 :localleader :desc "Execute code block and time execution"
 "C-c" #'time-execute)
 

;; Custom org links
(defun org-zoom-open (full-id)
  "`zoom' URI handler for org links.

Takes in a `full-id' of the meeting in the format \"<id>\" or
\"<id>&<pwd>\" and opens the join meeting URI with
`browse-url-xdg-open'; corresponds to the
\"x-scheme-handler/zoommtg\" mimetype"
  (browse-url-xdg-open
   (let ((formatted (dired-string-replace-match "&" full-id "&pwd=")))
      (concat "zoommtg://zoom.us/join?action=join&confno="
              (if formatted formatted full-id)))))
      
(after! org
  (org-link-set-parameters "zoom"
                           :follow #'org-zoom-open)
                           
  (org-link-set-parameters "xdg-open"
                           :follow #'browse-url-xdg-open))


;; APL
(use-package! gnu-apl-mode)


;; Lilypond
(use-package! lilypond-mode)
