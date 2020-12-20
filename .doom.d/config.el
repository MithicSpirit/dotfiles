;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Basic Configuration
(setq
 user-full-name "Ricardo Prado Cunha"
 user-mail-address "rpc01234@gmail.com"
 )
(set-language-environment "UTF-8")

;; $PATH
(setq
 org-directory "~/documents/org"
 bookmark-default-file "~/.doom.d/bookmarks"
 auth-sources '("~/.bak/.secrets/authinfo.gpg")
 +mu4e-mu4e-mail-path "~/mbsync"

 projectile-project-search-path
 '("~/documents/coding"
   "~/documents/coding/practice"
   "~/documents/coding/langs"
   "~/src"
   )
 )


;;; Appearance
;; Fonts
(setq
 doom-font (font-spec :family "Iosevka Mithic Book" :size 15)
 doom-variable-pitch-font (font-spec :family "CMU Sans Serif" :size 16)
 doom-big-font (font-spec :family "MesloLGS NF" :size 20)
 mode-line-font (font-spec :family "Iosevka Mithic Book" :size 17)
 )
(setq doom-unicode-font doom-font)

;; Theme
(setq doom-theme 'doom-palenight)
(custom-set-faces!
  '(fringe :inherit line-number)
  `(font-lock-comment-face :foreground ,(doom-lighten 'base6 .1) :slant italic)
  `(line-number-current-line :foreground ,(doom-color 'yellow) :weight bold)
  )

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
 doom-modeline-height 34
 )
(custom-set-faces!
  '(doom-modeline-bar :background nil)
  `(solaire-mode-line-face :foreground ,(doom-darken 'fg .1)
                           :background ,(doom-darken 'base3 .16)
                           )
  `(solaire-mode-line-inactive-face :background ,(doom-darken 'base2 .1))
  `(mode-line :foreground ,(doom-darken 'fg .1)
              :background ,(doom-darken 'base3 .16)
              :font ,mode-line-font
              )
  `(mode-line-inactive :background ,(doom-darken 'base2 .1))
  )

;; Custom ligatures
(plist-put! +ligatures-extra-symbols
            :map        "→"
            :not        "¬"
            :return     "↦"
            :yield      "↤"
            :union      "∪"
            ;:tuple      "⊗"
            :pipe       "𝕡")

;; Fancy org-mode
(after! org-superstar
  (setcar (last org-superstar-headline-bullets-list) '(?∙))
  )
(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("⣿" "⣶" "⣤" "⣀"))
  )
(setq org-ellipsis " ▼")
(custom-set-faces! `(org-ellipsis :foreground ,(doom-color 'base7))
  `(org-scheduled-today :foreground ,(doom-darken (doom-color 'fg) .1))
  `(org-scheduled-previously :foreground ,(doom-color 'fg))
  '(show-paren-match :background nil)
  )

;; Misc - Appearance
(setq
 ;; all-the-icons-scale-factor 1.2
 doom-themes-neotree-enable-variable-pitch nil
 )
(fringe-mode 0)



;;; Utility
;; Misc - Utility
(setq
 display-line-numbers-type t
 fill-column 80
 org-agenda-dim-blocked-tasks nil
 org-agenda-tags-column 0
 delete-by-moving-to-trash t
 ;; browse-url-generic-program "qutebrowser"
 )
(after! dired
  (setq dired-listing-switches "-AlhDF --group-directories-first"))

(add-hook! 'python-mode-hook (cmd! (setq fill-column 79)))

(add-hook! 'text-mode-hook #'auto-fill-mode)
(add-hook! 'prog-mode-hook (cmd! (display-fill-column-indicator-mode)))
(global-hl-todo-mode)
;; (add-hook! 'prog-mode-hook #'hl-todo-mode)
(add-hook! 'org-agenda-mode-hook #'doom-disable-show-paren-mode-h)
(add-hook! 'rainbow-mode-hook (hl-line-mode (if rainbow-mode -1 +1)))

;; Autosaving
(setq auto-save-visited-interval 30)
(auto-save-visited-mode)

;; Tabs vs. Spaces
(setq-default indent-tabs-mode nil)
(add-hook! '(javascript-mode-hook
             typescript-mode-hook
             c++-mode-hook
             sh-mode-hook
             fish-mode-hook
             )
  (cmd! (setq indent-tabs-mode t))
  )

;; [e]Vi[l] fixes/tweaks
(map! :after evil
      :n "x" "\"_dl"
      :n "X" "\"_dh"
      :n "~" "g~l"
      )
(setq evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-change-word-to-end nil
      +evil-want-o/O-to-continue-comments nil
      evil-echo-state nil
      )

;; Formatters
(after! format-all
  (set-formatter! 'black "black -q -l 79 --pyi -")
  (set-formatter! 'clang-format "clang-format -style=GNU")
  )

;; Easier to open org-agenda
(map! :after org
      :leader :desc "Open 10-day agenda" "o a a" #'org-agenda-list

      :map doom-leader-notes-map
      :localleader :desc "Open 10-day agenda"
      "a" #'org-agenda-list
      )

;; LaTeX tweaks and keybinds
(map! :after tex :map LaTeX-mode-map
 :localleader :desc "LaTeX Preview"
 "v" #'TeX-view
 :localleader :desc "Run langtool on current buffer"
 "c r" #'langtool-check
 :localleader :desc "Correct buffer with langtool"
 "c c" #'langtool-correct-buffer
 :localleader :desc "End current langtool session"
 "c d" #'langtool-check-done
 :localleader :desc "Return to langtool"
 "c e" #'exit-recursive-edit
 )
(setq +latex-indent-level-item-continuation 2)
;; (remove-hook! TeX-mode #'TeX-fold-mode)
;; (remove-hook! TeX-mode #'TeX-fold-buffer)

;; Writeroom tweaks
(after! writeroom-mode (setq writeroom-width 62))
(add-hook! writeroom-mode (cmd! (display-fill-column-indicator-mode -1)))
;; (add-hook! writeroom-mode-disable '(display-fill-column-indicator-mode))

;; Vterm tweaks (also REPLs)
(map! :map vterm-mode-map :after vterm
      :localleader :desc "Toggle copy mode"
      "c" #'vterm-copy-mode
      )
(map! :map vterm-copy-mode-map :after vterm
      :localleader :desc "Toggle copy mode"
      "c" #'vterm-copy-mode
      )
(add-hook! '(vterm-mode-hook +eval-repl-mode-hook) #'evil-emacs-state)
(add-hook! 'vterm-copy-mode-hook #'evil-normal-state)

;; Toggle ligatures
(map! :leader :desc "Ligatures" "t L" #'prettify-symbols-mode

      :map doom-leader-toggle-map
      :localleader :desc "Ligatures"
      "L" #'prettify-symbols-mode
      )

;; Larger popups
(set-popup-rule! "^\\*doom:\\(?:v?term\\|e?shell\\)-popup"
  :vslot -5 :height 0.46 :select t :modeline nil :quit nil :ttl nil)

;; Python lsp server
(use-package! lsp-jedi)
(add-hook! 'python-mode-hook (cmd! (setq flycheck-disabled-checkers '(lsp))))
(defadvice! +lsp--respect-default-checker-python-mode (orig-fn &rest args)
  "Ensure `flycheck-checker' isn't overwritten by `lsp' in python-mode."
  :around #'lsp-diagnostics-flycheck-enable
  (if (eq major-mode 'python-mode)
      (let ((old-checker flycheck-checker))
        (apply orig-fn args)
        (setq-local flycheck-checker old-checker))
    (apply orig-fn args))
  )

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
        "9" #'winum-select-window-9
        ))

;; Email (mu4e)
(set-email-account!
 "Gmail"
 '((user-mail-address   .  "rpc01234@gmail.com")
   (smtpmail-smtp-user  .  "rpc01234@gmail.com")
   (mu4e-sent-folder    .  "/[Gmail].Sent")
   (mu4e-drafts-folder  .  "/[Gmail].Drafts")
   (mu4e-trash-folder   .  "/[Gmail].Trash")
   (mu4e-refile-folder  .  "/[Gmail].All")
   ))
(use-package! mu4e-alert
  :config
  (setq mu4e-alert-mode-line t
        doom-modeline-mu4e t
        mu4e-update-interval nil
        )
  (mu4e-alert-enable-mode-line-display))


;;; Heavy customization
;; School agenda
(defun school-agenda (&optional kill)
  "Open tasks and agenda with schedule in a sidebar."
  (interactive (list (if (string= (buffer-name (current-buffer)) "*doom*")
                         nil t)))
  (if kill (call-interactively #'doom/kill-all-buffers))
  (cd org-directory)
  (evil-edit "schedule.org") (read-only-mode)
  (evil-window-vsplit nil "tasks.org") (org-shifttab 2)
  (evil-window-left 1) (evil-window-set-width 39) ;; (schedule)

  (evil-window-right 1) (evil-window-split) (org-agenda-list)
  (evil-window-set-height 23)

  (evil-window-left 1) ;; (schedule)
  )

(map!
 :leader :desc "Open school agenda" "o a s" #'school-agenda
 :map doom-leader-notes-map :localleader :desc "Open school agenda"
 "s" #'school-agenda
 )
(map!
 :map org-mode-map :after org
 :localleader :desc "Set visibility to 2"
 "TAB" (cmd! (org-shifttab 2))
 )

(setcar (cdr +doom-dashboard-menu-sections)
        `("Open school agenda" .
          ,(plist-put
            (cdr (assoc "Open org-agenda" +doom-dashboard-menu-sections))
            :action #'school-agenda
            )))

;; Org time execute
(defun time-call (time-call &rest args)
  "Time the execution of an org-mode source block"
  (let ((start-time (float-time)))
    (apply time-call args)
    (message "Function call took %f seconds" (- (float-time) start-time))
    ))
(defun time-execute ()
  "Execute a source block and time its execution"
  (interactive)
  (advice-add 'org-babel-execute-src-block-maybe :around #'time-call)
  (funcall-interactively 'org-babel-execute-src-block-maybe)
  (advice-remove 'org-babel-execute-src-block-maybe #'time-call)
  )
(map!
 :map org-mode-map :after org
 :localleader :desc "Execute code block and time execution"
 "C-c" #'time-execute
 )

;; Custom org links
(defun org-zoom-open (full-id)
  "Takes in a `full-id' of the meeting in the format \"<id>\" or
\"<id>&<pwd>\" and opens the join meeting URI with
`browse-url-xdg-open'; corresponds to the
\"x-scheme-handler/zoommtg\" mimetype"
  (browse-url-xdg-open
   (let ((formatted (dired-string-replace-match "&" full-id "&pwd=")))
      (concat "zoommtg://zoom.us/join?action=join&confno="
              (if formatted formatted full-id))
      )))
(after! org
  (org-link-set-parameters "zoom"
                           :follow #'org-zoom-open
                           )
  (org-link-set-parameters "xdg-open"
                           :follow #'browse-url-xdg-open
                           ))


