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
(setq doom-theme 'doom-opera)
(load-theme 'doom-opera t)

;; Telephone modeline
(custom-set-faces!
  `(telephone-line-evil-emacs :background
                                  ,(doom-darken (doom-color 'teal) .33))
  `(telephone-line-evil-insert :background
                                  ,(doom-darken (doom-color 'blue) .33))
  `(telephone-line-evil-motion :background
                                  ,(doom-darken (doom-color 'base7) .33))
  `(telephone-line-evil-normal :background
                                  ,(doom-darken (doom-color 'green) .33))
  `(telephone-line-evil-operator :background
                                  ,(doom-darken (doom-color 'base2) .33))
  `(telephone-line-evil-visual :background
                                  ,(doom-darken (doom-color 'yellow) .33))
  `(telephone-line-evil-replace :background
                                  ,(doom-darken (doom-color 'red) .33))
  `(telephone-line-evil-god :background
                                  ,(doom-darken (doom-color 'cyan) .33))

  `(telephone-line-accent-active :background ,(doom-color 'grey))
  `(mode-line-inactive :background ,(doom-color 'bg-alt)
                       :foreground ,(doom-darken (doom-color 'fg-alt) .33))
  `(telephone-line-accent-inactive :background
                                    ,(doom-lighten (doom-color 'bg-alt) .03)
                                   :foreground
                                    ,(doom-darken (doom-color 'fg-alt) .33))
)

(telephone-line-defsegment* telephone-custom-evil-segment ()
  "Displays current evil mode with an equal-length tag."
  (when (bound-and-true-p evil-mode)
    (let ((tag (cond
                ((evil-operator-state-p)
                 (if telephone-line-evil-use-short-tag "OP" "OPERTR"))
                ((evil-replace-state-p)
                 (if telephone-line-evil-use-short-tag "RE" "REPLCE"))
                ((not (evil-visual-state-p)) (upcase (symbol-name evil-state)))
                ((eq evil-visual-selection 'block)
                 (if telephone-line-evil-use-short-tag "VB" "V-BLCK"))
                ((eq evil-visual-selection 'line)
                 (if telephone-line-evil-use-short-tag "VL" "V-LINE"))
                (t "VISUAL"))))
      (if telephone-line-evil-use-short-tag
          (seq-take tag 2)
        tag))))

(setq telephone-line-lhs
      '((evil   . (telephone-custom-evil-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-filesize-segment
                   telephone-line-input-info-segment
                   (telephone-line-projectile-buffer-segment 0 1)))
        (nil    . (telephone-line-airline-position-segment
                   telephone-line-process-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-flycheck-segment
                   telephone-line-hud-segment))))

(setq telephone-line-height 23)
(telephone-line-mode t)

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

(map! :n "x" "\"_dl"
      :n "X" "\"_dh"
      :n "~" "g~l")

(map! :leader :desc "Open 10-day agenda"
      "o a a" #'org-agenda-list)

(setq ;; More natural split direction
      evil-split-window-below t
      evil-vsplit-window-right t
      ;; Consistent checkbox case in markdown
      markdown-gfm-uppercase-checkbox nil
      ;; Langtool location
      langtool-language-tool-jar
      "/snap/languagetool/23/usr/bin/languagetool-commandline.jar")

;; Set defuault encoding
(set-language-environment "UTF-8")

(defun school-agenda ()
  "Open tasks and agenda with schedule in a sidebar."
  (interactive)
  ;; Close all current buffers
  (+evil:kill-all-buffers nil)
  ;; Open schedule in main window in read-only mode
  (evil-edit "~/org/schedule.org") (read-only-mode)
  ;; Split schedule with tasks
  (evil-window-vsplit nil "~/org/tasks.org")
  ;; Reduce schedule size to look like sidebar
  (evil-window-left 1) (evil-window-set-width 36)
  ;; Open org-agenda below tasks (and make it small)
  (evil-window-right 1)
  (evil-window-split) (org-agenda-list)
  (evil-window-set-height 19)
  ;; Return to schedule
  (evil-window-left 1))

;; Ruler & autofill at 80 chars
(setq fill-column 80)
(add-hook! text-mode 'display-fill-column-indicator-mode)
(add-hook! text-mode 'auto-fill-mode)
;; (add-hook! text-mode 'visual-fill-column) ; disabled because not compatible
                                             ; with auto-fill-mode

;; Writeroom (zen mode) tweaks
(setq writeroom-width 62)
(add-hook! writeroom-mode-hook '(display-fill-column-indicator-mode -1))
(add-hook! writeroom-mode-disable-hook '(display-fill-column-indicator-mode))

;; LaTeX keybind tweaks
(map! :map LaTeX-mode-map
      ;; Easy access to preview pane
      :localleader :desc "Preview pane"
      "V" #'latex-preview-pane-mode
      :localleader :desc "Update preview pane"
      "v" #'latex-preview-pane-update
      ;; Easy access to langtool
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

;; Fix 2-wide ligatures
(plist-put! +ligatures-extra-symbols
  '(:map           "→"
    :return        "↦"
    :yield         "↤"
    :union         "∪"
    :tuple         "⊗"
    :pipe          "𝔭"))

;; Fix bad fira-code (can't wait for harfbuzz to be fixed!)
(+ligatures--def-font fira
    ("Fira Code Symbol"
     :range '(#Xe100 . #Xe16f)
     :url "https://github.com/tonsky/FiraCode/raw/13234c0/distr/ttf/%s"
     :files '("FiraCode-Bold.ttf"
              "FiraCode-Light.ttf"
              "FiraCode-Medium.ttf"
              "FiraCode-Regular.ttf"
              "FiraCode-Retina.ttf"))
  ("www"    . #Xe100)
  ("**"     . #Xe101)
  ("***"    . #Xe102)
  ("**/"    . #Xe103)
  ("*>"     . #Xe104)
  ("*/"     . #Xe105)
  ("\\\\"   . #Xe106)
  ("\\\\\\" . #Xe107)
  ("{-"     . #Xe108)
  ("::"     . #Xe10a)
  (":::"    . #Xe10b)
  (":="     . #Xe10c)
  ("!!"     . #Xe10d)
  ("!="     . #Xe10e)
  ("!=="    . #Xe10f)
  ("-}"     . #Xe110)
  ("--"     . #Xe111)
  ("---"    . #Xe112)
  ("-->"    . #Xe113)
  ("->"     . #Xe114)
  ("->>"    . #Xe115)
  ("-<"     . #Xe116)
  ("-<<"    . #Xe117)
  ("-~"     . #Xe118)
  ("#{"     . #Xe119)
  ("#["     . #Xe11a)
  ("##"     . #Xe11b)
  ("###"    . #Xe11c)
  ("####"   . #Xe11d)
  ("#("     . #Xe11e)
  ("#?"     . #Xe11f)
  ("#_"     . #Xe120)
  ("#_("    . #Xe121)
  (".-"     . #Xe122)
  (".="     . #Xe123)
  (".."     . #Xe124)
  ("..<"    . #Xe125)
  ("..."    . #Xe126)
  ("?="     . #Xe127)
  ("??"     . #Xe128)
  (";;"     . #Xe129)
  ("/*"     . #Xe12a)
  ("/**"    . #Xe12b)
  ("/="     . #Xe12c)
  ("/=="    . #Xe12d)
  ("/>"     . #Xe12e)
  ("//"     . #Xe12f)
  ("///"    . #Xe130)
  ("&&"     . #Xe131)
  ("||"     . #Xe132)
  ("||="    . #Xe133)
  ("|="     . #Xe134)
  ("|>"     . #Xe135)
  ("^="     . #Xe136)
  ("$>"     . #Xe137)
  ("++"     . #Xe138)
  ("+++"    . #Xe139)
  ("+>"     . #Xe13a)
  ("=:="    . #Xe13b)
  ("=="     . #Xe13c)
  ("==="    . #Xe13d)
  ("==>"    . #Xe13e)
  ("=>"     . #Xe13f)
  ("=>>"    . #Xe140)
  ("=<"     . #Xe141)
  ("=<<"    . #Xe142)
  ("=/="    . #Xe143)
  (">-"     . #Xe144)
  (">="     . #Xe145)
  (">=>"    . #Xe146)
  (">>"     . #Xe147)
  (">>-"    . #Xe148)
  (">>="    . #Xe149)
  (">>>"    . #Xe14a)
  ("<*"     . #Xe14b)
  ("<*>"    . #Xe14c)
  ("<|"     . #Xe14d)
  ("<|>"    . #Xe14e)
  ("<$"     . #Xe14f)
  ("<$>"    . #Xe150)
  ("<!--"   . #Xe151)
  ("<-"     . #Xe152)
  ("<--"    . #Xe153)
  ("<->"    . #Xe154)
  ("<+"     . #Xe155)
  ("<+>"    . #Xe156)
  ("<="     . #Xe157)
  ("<=="    . #Xe158)
  ("<=>"    . #Xe159)
  ("<=<"    . #Xe15a)
  ("<>"     . #Xe15b)
  ("<<"     . #Xe15c)
  ("<<-"    . #Xe15d)
  ("<<="    . #Xe15e)
  ("<<<"    . #Xe15f)
  ("<~"     . #Xe160)
  ("<~~"    . #Xe161)
  ("</"     . #Xe162)
  ("</>"    . #Xe163)
  ("~@"     . #Xe164)
  ("~-"     . #Xe165)
  ("~="     . #Xe166)
  ("~>"     . #Xe167)
  ("~~"     . #Xe168)
  ("~~>"    . #Xe169)
  ("%%"     . #Xe16a)
  (":"      . #Xe16c)
  ("+"      . #Xe16d)
  ("+"      . #Xe16e)
  ("*"      . #Xe16f))
