;;; packages.el --- media layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alejandro Erickson <alejo@alejandro.wlan.dur.ac.uk>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `media-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `media/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `media/pre-init-PACKAGE' and/or
;;   `media/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst media-packages
  '(
    (emms :location (recipe
                     :fetcher git
                     :url "http://git.savannah.gnu.org/cgit/emms.git/"
                     :files ("lisp/*.el"
                             ("cd-here-and-make-emms-print-metadata-and-put-in-path" "Makefile")
                             ("cd-here-and-make-emms-print-metadata-and-put-in-path/src" "src/*")
                             )
                     )
          )
    emms-state
    helm-emms
    emms-info-libtag
    )
  "The list of Lisp packages required by the media layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun media/init-emms ()
  (use-package emms
    :init
    (global-set-key [(f7)] 'emms-smart-browse)
    (add-hook 'emms-browser-show-display-hook 'evil-emms-keymap-mode)
    (add-hook 'emms-browser-show-display-hook 'evil-initialize)
    (evil-set-initial-state 'emms-playlist-mode 'emacs)
    (evil-set-initial-state 'emms-browser-mode 'normal)
    :config
    ;;(require 'emms-setup)
    (emms-all)
    (emms-mode-line 0)
    (emms-playing-time 1)
    (emms-default-players)
    (autoload 'emms-smart-browse "emms-browser.el" "Browse with EMMS" t)
    (define-key emms-browser-mode-map (kbd "D") 'emms-browser-delete-files-by-moving-to-trash)
    (define-key emms-browser-mode-map (kbd "t") 'emms-browser-toggle-subitems)
    (require 'emms-info-libtag)
    (setq emms-info-functions '(emms-info-libtag))

    (evilified-state-evilify-map emms-browser-mode-map
      :mode emms-browser-mode
      :bindings
      ;; We only need to add one key binding, since this is normally SPC
      "t" 'emms-browser-toggle-subitems
     )
    )
  )

(defun media/init-emms-state ()
  (use-package emms-state
    :config
    (emms-state-mode 0)
    ))

(defun media/init-helm-emms ()
  (use-package helm-emms)
  )
;;; packages.el ends here
