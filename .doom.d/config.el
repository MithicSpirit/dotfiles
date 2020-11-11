;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Basic Configuration
(setq
 user-full-name "Ricardo Prado Cunha"
 user-mail-address "rpc01234@gmail.com"
 )
(set-language-environment "UTF-8")

;; $PATH
(setq
 org-directory "~/org/"

 langtool-language-tool-jar
 "/snap/languagetool/23/usr/bin/languagetool-commandline.jar"

 projectile-project-search-path
 '("~/coding/" "~/coding/practice/" "~/coding/langs" "~/source/")
 )


;;; Appearance
;; Fonts
(let
    ((fontsize 18)
     (fontface "Iosevka Mithic Book Extended")
     )
  (setq
   doom-font (font-spec :family fontface :size fontsize :antialiasing nil)
   doom-variable-pitch-font (font-spec :family "CMU Sans Serif"
                                       :size (truncate (* fontsize 0.8))
                                       )
   doom-big-font (font-spec :family fontface
                            :size (truncate (* fontsize 1.3))
                            :antialiasing t
                            :weight 'medium
                            )
   ))
(setq doom-unicode-font doom-font)

;; Theme
(setq doom-theme 'doom-palenight)
(custom-set-faces!
  '(fringe :inherit line-number)
  '(font-lock-comment-face :slant italic)
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
 doom-modeline-height 28
 )
(custom-set-faces!
  '(doom-modeline-bar :background nil)
  `(solaire-mode-line-face :foreground ,(doom-darken (doom-color 'fg) .1))
  )

;; Telephone modeline [DEFUNCT]
;; (custom-set-faces!
;;   `(telephone-line-evil-emacs :background
;;                                   ,(doom-darken (doom-color 'teal) .33))
;;   `(telephone-line-evil-insert :background
;;                                   ,(doom-darken (doom-color 'blue) .33))
;;   `(telephone-line-evil-motion :background
;;                                   ,(doom-darken (doom-color 'base7) .33))
;;   `(telephone-line-evil-normal :background
;;                                   ,(doom-darken (doom-color 'green) .33))
;;   `(telephone-line-evil-operator :background
;;                                   ,(doom-darken (doom-color 'base2) .33))
;;   `(telephone-line-evil-visual :background
;;                                   ,(doom-darken (doom-color 'yellow) .33))
;;   `(telephone-line-evil-replace :background
;;                                   ,(doom-darken (doom-color 'red) .33))
;;   `(telephone-line-evil-god :background
;;                                   ,(doom-darken (doom-color 'cyan) .33))
;;   `(telephone-line-accent-active :background ,(doom-color 'grey))
;;   `(mode-line-inactive :background ,(doom-color 'bg-alt)
;;                        :foreground ,(doom-darken (doom-color 'fg-alt) .33))
;;   `(telephone-line-accent-inactive :background
;;                                     ,(doom-lighten (doom-color 'bg-alt) .03)
;;                                    :foreground
;;                                     ,(doom-darken (doom-color 'fg-alt) .33))
;; )
;; (telephone-line-defsegment* telephone-custom-evil-segment ()
;;   "Displays current evil mode with an equal-length tag."
;;   (when (bound-and-true-p evil-mode)
;;     (let ((tag (cond
;;                 ((evil-operator-state-p)
;;                  (if telephone-line-evil-use-short-tag "OP" "OPERTR"))
;;                 ((evil-replace-state-p)
;;                  (if telephone-line-evil-use-short-tag "RE" "REPLCE"))
;;                 ((not (evil-visual-state-p)) (upcase (symbol-name evil-state)))
;;                 ((eq evil-visual-selection 'block)
;;                  (if telephone-line-evil-use-short-tag "VB" "V-BLCK"))
;;                 ((eq evil-visual-selection 'line)
;;                  (if telephone-line-evil-use-short-tag "VL" "V-LINE"))
;;                 (t "VISUAL"))))
;;       (if telephone-line-evil-use-short-tag
;;           (seq-take tag 2)
;;         tag))))
;; (setq telephone-line-lhs
;;       '((evil   . (telephone-custom-evil-segment))
;;         (accent . (telephone-line-vc-segment
;;                    telephone-line-filesize-segment
;;                    telephone-line-input-info-segment
;;                    (telephone-line-projectile-buffer-segment 0 1)))
;;         (nil    . (telephone-line-airline-position-segment
;;                    telephone-line-process-segment))))
;; (setq telephone-line-rhs
;;       '((nil    . (telephone-line-misc-info-segment))
;;         (accent . (telephone-line-major-mode-segment))
;;         (evil   . (telephone-line-flycheck-segment
;;                    telephone-line-hud-segment))))
;; (setq telephone-line-height 23)
;; (telephone-line-mode t)

;; Custom ligatures
(plist-put! +ligatures-extra-symbols
            :map        "→"
            :return     "↦"
            :yield      "↤"
            :union      "∪"
            :tuple      "⊗"
            :pipe       "𝔭")

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
 all-the-icons-scale-factor 1.1
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
 )
(after! dired
  (setq dired-listing-switches "-AlhDF --group-directories-first"))

(add-hook! 'python-mode-hook (setq fill-column 79))
(add-hook! 'python-mode-disable-hook (setq fill-column 80))

(add-hook! text-mode 'display-fill-column-indicator-mode 'auto-fill-mode)
;; (add-hook! text-mode 'visual-fill-column)

;; Autosaving
(setq auto-save-visited-interval 30)
(auto-save-visited-mode)

;; Tabs vs. Spaces
(setq-default indent-tabs-mode nil)
(add-hook! '(javascript-mode-hook
             typescript-mode-hook
             cpp-mode-hook
             )
  (setq indent-tabs-mode t)
  )
(add-hook! '(javascript-mode-disable-hook
             typescript-mode-disable-hook
             cpp-mode-disable-hook
             )
  (setq indent-tabs-mode nil)
  )

;; [e]Vi[l] fixes/tweaks
(map! :after evil
      :n "x" "\"_dl"
      :n "X" "\"_dh"
      :n "~" "g~l"
      )
(setq evil-split-window-below t
      evil-vsplit-window-right t
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
 :localleader :desc "Preview pane"
 "V" #'latex-preview-pane-mode
 :localleader :desc "Update preview pane"
 "v" #'latex-preview-pane-update
 :localleader :desc "Check current buffer with langtool"
 "C c" #'langtool-check
 :localleader :desc "Check current buffer with langtool"
 "C C" #'langtool-check
 :localleader :desc "Interactively correct buffer with langtool"
 "c" #'langtool-correct-buffer
 :localleader :desc "End current langtool session"
 "C C-c" #'langtool-check-done
 :localleader :desc "Return to langtool"
 "C-c" #'exit-recursive-edit
 )
(setq +latex-indent-level-item-continuation 2)

;; Writeroom tweaks
(after! writeroom-mode (setq writeroom-width 62))
(add-hook! writeroom-mode '(display-fill-column-indicator-mode -1))
(add-hook! writeroom-mode-disable '(display-fill-column-indicator-mode))

;; Vterm keybinds
(map! :map vterm-mode-map :after vterm
      :localleader :desc "Toggle copy mode"
      "c" #'vterm-copy-mode
      )
(map! :map vterm-copy-mode-map :after vterm
      :localleader :desc "Toggle copy mode"
      "c" #'vterm-copy-mode
      )


;;; Heavy customization
;; School agenda
(defun school-agenda (&optional kill)
  "Open tasks and agenda with schedule in a sidebar."
  (interactive (list (if (string= (buffer-name (current-buffer)) "*doom*")
                         nil t)))
  (if kill (+evil:kill-all-buffers nil))
  (cd "~/org")
  (evil-edit "schedule.org") (read-only-mode)
  (evil-window-vsplit nil "tasks.org") (org-shifttab 2)
  (evil-window-left 1) (evil-window-set-width 36) ;; (schedule)

  (evil-window-right 1) (evil-window-split) (org-agenda-list)
  (evil-window-set-height 19)

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
(defun org-zoom-export (full-id)
  "Takes in a `full-id' of the meeting in the format \"<id>\" or
\"<id>&<pwd>\" and returns the join meeting URI; corresponds to
the \"x-scheme-handler/zoommtg\" mimetype"
  (let ((formatted (dired-string-replace-match "&" full-id "&pwd=")))
    (concat "zoommtg://zoom.us/join?action=join&confno="
            (if formatted formatted full-id))))
(defun org-zoom-open (full-id)
  "Opens the meeting corresponding to `full-id' with
`browse-url-xdg-open'. see `org-zoom-export' for more
information."
  (browse-url-xdg-open (org-zoom-export full-id)))
(after! org
  (org-link-set-parameters "xdg-open"
                           :follow #'browse-url-xdg-open
                           :export (lambda (resource) resource))
  (org-link-set-parameters "zoom"
                           :follow #'org-zoom-open
                           :export #'org-zoom-export))
