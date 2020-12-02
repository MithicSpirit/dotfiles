;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a

;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;(package! mastodon)

;; (use-package! telephone-line
;;   :config
;;   (custom-set-faces!
;;     `(telephone-line-evil-emacs :background
;;                                 ,(doom-darken (doom-color 'teal) .33))
;;     `(telephone-line-evil-insert :background
;;                                  ,(doom-darken (doom-color 'blue) .33))
;;     `(telephone-line-evil-motion :background
;;                                  ,(doom-darken (doom-color 'base7) .33))
;;     `(telephone-line-evil-normal :background
;;                                  ,(doom-darken (doom-color 'green) .33))
;;     `(telephone-line-evil-operator :background
;;                                    ,(doom-darken (doom-color 'base2) .33))
;;     `(telephone-line-evil-visual :background
;;                                  ,(doom-darken (doom-color 'yellow) .33))
;;     `(telephone-line-evil-replace :background
;;                                   ,(doom-darken (doom-color 'red) .33))
;;     `(telephone-line-evil-god :background
;;                               ,(doom-darken (doom-color 'cyan) .33))
;;     `(telephone-line-accent-active :background ,(doom-color 'grey))
;;     `(mode-line-inactive :background ,(doom-color 'bg-alt)
;;                          :foreground ,(doom-darken (doom-color 'fg-alt) .33))
;;     `(telephone-line-accent-inactive :background
;;                                      ,(doom-lighten (doom-color 'bg-alt) .03)
;;                                      :foreground
;;                                      ,(doom-darken (doom-color 'fg-alt) .33))
;;     )
;;   (telephone-line-defsegment*
;;    telephone-custom-evil-segment ()
;;    "Displays current evil mode with an equal-length tag."
;;    (when (bound-and-true-p evil-mode)
;;      (let ((tag (cond
;;                  ((evil-operator-state-p)
;;                   (if telephone-line-evil-use-short-tag "OP" "OPERTR"))
;;                  ((evil-replace-state-p)
;;                   (if telephone-line-evil-use-short-tag "RE" "REPLCE"))
;;                  ((not (evil-visual-state-p)) (upcase (symbol-name evil-state)))
;;                  ((eq evil-visual-selection 'block)
;;                   (if telephone-line-evil-use-short-tag "VB" "V-BLCK"))
;;                  ((eq evil-visual-selection 'line)
;;                   (if telephone-line-evil-use-short-tag "VL" "V-LINE"))
;;                  (t "VISUAL"))))
;;        (if telephone-line-evil-use-short-tag
;;            (seq-take tag 2)
;;          tag))))
;;   (setq telephone-line-lhs
;;         '((evil   . (telephone-custom-evil-segment))
;;           (accent . (telephone-line-vc-segment
;;                      telephone-line-filesize-segment
;;                      telephone-line-input-info-segment
;;                      (telephone-line-projectile-buffer-segment 0 1)))
;;           (nil    . (telephone-line-airline-position-segment
;;                      telephone-line-process-segment))))
;;   (setq telephone-line-rhs
;;         '((nil    . (telephone-line-misc-info-segment))
;;           (accent . (telephone-line-major-mode-segment))
;;           (evil   . (telephone-line-flycheck-segment
;;                      telephone-line-hud-segment))))
;;   (setq telephone-line-height 23)
;;   (telephone-line-mode t)
;;   )

(when (package! lsp-mode) (package! lsp-jedi))
