;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ricardo Prado Cunha"
      user-mail-address "rpc01234@gmail.com")


;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq fontsize 17)
(setq doom-font (font-spec :family "Fira Code Retina" :size fontsize)
      doom-variable-pitch-font (font-spec :family "CMU Sans Serif"
                                          :size (truncate (* fontsize 0.8)))
      doom-big-font (font-spec :family "Fira Code Retina"
                               :size (truncate (* fontsize 1.3))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-palenight)
(custom-set-faces! '(fringe :inherit line-number))

;; Doom modeline evil state
(setq doom-modeline-modal-icon nil
      evil-emacs-state-tag    (propertize "[Emacs ]")
      evil-insert-state-tag   (propertize "[Insert]")
      evil-motion-state-tag   (propertize "[Motion]")
      evil-normal-state-tag   (propertize "[Normal]")
      evil-operator-state-tag (propertize "[Opertr]")
      evil-visual-state-tag   (propertize "[Visual]")
      evil-replace-state-tag  (propertize "[Replce]")
      doom-modeline-bar-width 0
      doom-modeline-height 24)
(custom-set-faces! '(doom-modeline-bar :background nil))

;; Telephone modeline
;(custom-set-faces!
;  `(telephone-line-evil-emacs :background
;                                  ,(doom-darken (doom-color 'teal) .33))
;  `(telephone-line-evil-insert :background
;                                  ,(doom-darken (doom-color 'blue) .33))
;  `(telephone-line-evil-motion :background
;                                  ,(doom-darken (doom-color 'base7) .33))
;  `(telephone-line-evil-normal :background
;                                  ,(doom-darken (doom-color 'green) .33))
;  `(telephone-line-evil-operator :background
;                                  ,(doom-darken (doom-color 'base2) .33))
;  `(telephone-line-evil-visual :background
;                                  ,(doom-darken (doom-color 'yellow) .33))
;  `(telephone-line-evil-replace :background
;                                  ,(doom-darken (doom-color 'red) .33))
;  `(telephone-line-evil-god :background
;                                  ,(doom-darken (doom-color 'cyan) .33))
;
;  `(telephone-line-accent-active :background ,(doom-color 'grey))
;  `(mode-line-inactive :background ,(doom-color 'bg-alt)
;                       :foreground ,(doom-darken (doom-color 'fg-alt) .33))
;  `(telephone-line-accent-inactive :background
;                                    ,(doom-lighten (doom-color 'bg-alt) .03)
;                                   :foreground
;                                    ,(doom-darken (doom-color 'fg-alt) .33))
;)
;(telephone-line-defsegment* telephone-custom-evil-segment ()
;  "Displays current evil mode with an equal-length tag."
;  (when (bound-and-true-p evil-mode)
;    (let ((tag (cond
;                ((evil-operator-state-p)
;                 (if telephone-line-evil-use-short-tag "OP" "OPERTR"))
;                ((evil-replace-state-p)
;                 (if telephone-line-evil-use-short-tag "RE" "REPLCE"))
;                ((not (evil-visual-state-p)) (upcase (symbol-name evil-state)))
;                ((eq evil-visual-selection 'block)
;                 (if telephone-line-evil-use-short-tag "VB" "V-BLCK"))
;                ((eq evil-visual-selection 'line)
;                 (if telephone-line-evil-use-short-tag "VL" "V-LINE"))
;                (t "VISUAL"))))
;      (if telephone-line-evil-use-short-tag
;          (seq-take tag 2)
;        tag))))
;
;(setq telephone-line-lhs
;      '((evil   . (telephone-custom-evil-segment))
;        (accent . (telephone-line-vc-segment
;                   telephone-line-filesize-segment
;                   telephone-line-input-info-segment
;                   (telephone-line-projectile-buffer-segment 0 1)))
;        (nil    . (telephone-line-airline-position-segment
;                   telephone-line-process-segment))))
;(setq telephone-line-rhs
;      '((nil    . (telephone-line-misc-info-segment))
;        (accent . (telephone-line-major-mode-segment))
;        (evil   . (telephone-line-flycheck-segment
;                   telephone-line-hud-segment))))
;
;(setq telephone-line-height 23)
;(telephone-line-mode t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun school-agenda (&optional kill)
  "Open tasks and agenda with schedule in a sidebar."
  (interactive (list (if (string= (buffer-name (current-buffer)) "*doom*")
                         nil t)))
  ;; Close all current buffers
  (if kill (+evil:kill-all-buffers nil))
  ;; Open schedule in main window in read-only mode
  (evil-edit "~/org/schedule.org") (read-only-mode)
  ;; Split schedule with tasks and set view level
  (evil-window-vsplit nil "~/org/tasks.org") (org-shifttab 2)
  ;; Reduce schedule size to look like sidebar
  (evil-window-left 1) (evil-window-set-width 36)
  ;; Open org-agenda below tasks (and make it small)
  (evil-window-right 1) (evil-window-split) (org-agenda-list)
  (evil-window-set-height 19)
  ;; Return to schedule
  (evil-window-left 1))

(map!
 ;; vim keybind tweaks
 :n "x" "\"_dl"
 :n "X" "\"_dh"
 :n "~" "g~l"

 :leader :desc "Open 10-day agenda" "o a a" #'org-agenda-list
 :map doom-leader-notes-map :localleader :desc "Open 10-day agenda"
 "a" #'org-agenda-list

 :leader :desc "Open Neotree" "o n" #'neotree
 :map doom-leader-open-map :localleader :desc "Open Neotree"
 "n" #'neotree
 :leader :desc "Close Neotree" "o N" #'neotree-hide
 :map doom-leader-open-map :localleader :desc "Close Neotree"
 "N" #'neotree-hide

 :leader :desc "Open school agenda" "o a s" #'school-agenda
 :map doom-leader-notes-map :localleader :desc "Open school agenda"
 "s" #'school-agenda

 :map org-mode-map :localleader :desc "Set visibility to 2"
 "TAB" (cmd! (org-shifttab 2))
 ;; LaTeX keybind shortcuts
 :map LaTeX-mode-map
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
 "C-c" #'exit-recursive-edit)

(setq ;; More natural split direction
      evil-split-window-below t
      evil-vsplit-window-right t
      ;; Consistent checkbox case in markdown FIXED
      ; markdown-gfm-uppercase-checkbox nil
      ;; Langtool location
      langtool-language-tool-jar
      "/snap/languagetool/23/usr/bin/languagetool-commandline.jar"
      ;; LaTeX list indent
      +latex-indent-level-item-continuation 2

      fill-column 80
      writeroom-width 62
      org-ellipsis " ▼"
      doom-themes-neotree-enable-variable-pitch nil)

(custom-set-faces! `(org-ellipsis :foreground ,(doom-color 'base7))
  `(org-scheduled-today :foreground ,(doom-darken (doom-color 'fg) .1))
  `(org-scheduled-previously :foreground ,(doom-color 'fg)))

;; Set default encoding
(set-language-environment "UTF-8")


;; Ruler & autofill
(add-hook! text-mode 'display-fill-column-indicator-mode 'auto-fill-mode)
;; (add-hook! text-mode 'visual-fill-column) ; disabled because not compatible
                                             ; with auto-fill-mode

;; Writeroom (zen mode) tweaks
(add-hook! writeroom-mode-hook '(display-fill-column-indicator-mode -1))
(add-hook! writeroom-mode-disable-hook '(display-fill-column-indicator-mode))

;; Doom dashboard
(setcar (cdr +doom-dashboard-menu-sections)
        `("Open school agenda" . ,(plist-put (cdr (assoc
                                                 "Open org-agenda"
                                                 +doom-dashboard-menu-sections))
                                           :action #'school-agenda)))

;; Fix 2-wide ligatures
(plist-put! +ligatures-extra-symbols
            :map        "→"
            :return     "↦"
            :yield      "↤"
            :union      "∪"
            :tuple      "⊗"
            :pipe       "𝔭")
