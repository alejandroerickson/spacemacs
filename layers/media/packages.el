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
    (require 'emms-info-libtag)
    (setq emms-info-functions '(emms-info-libtag))

    ;; PROBLEMS BEGIN HERE

    ;; this works fine

    ;;this doesn't work.
    ;; It might be because emms-browser-mode is not a real mode and mode-hooks
    ;; don't work here
    
    ;; As a result, evilifying didn't work
    (evilified-state-evilify-map emms-browser-mode-map
      :mode emms-browser-mode
   ;;   :bindings
     ;;"r" 'emms-browser-goto-random 
     )

;;     ;; and this doesn't work
;;     (define-minor-mode evil-emms-keymap-mode
;;       "Evil EMMS Key map mode."
;;       :keymap (make-sparse-keymap)
;;       )
;;    (evil-define-key 'normal evil-emms-keymap-mode-map "b1" 'emms-browse-by-artist)
;;    (evil-define-key 'normal evil-emms-keymap-mode-map "b2" 'emms-browse-by-artist)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "C-j" 'emms-browser-add-tracks-and-play)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "RET" 'emms-browser-add-tracks)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "SPC" 'emms-browser-toggle-subitems)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "/" 'emms-isearch-buffer)
;; (evil-define-key 'normal evil-emms-keymap-mode "1" 'emms-browser-collapse-all)
;; (evil-define-key 'normal evil-emms-keymap-mode "2" 'emms-browser-expand-to-level-2)
;; (evil-define-key 'normal evil-emms-keymap-mode "3" 'emms-browser-expand-to-level-3)
;; (evil-define-key 'normal evil-emms-keymap-mode "4" 'emms-browser-expand-to-level-4)
;; (evil-define-key 'normal evil-emms-keymap-mode "<" 'emms-browser-previous-filter)
;; (evil-define-key 'normal evil-emms-keymap-mode ">" 'emms-browser-next-filter)
;; (evil-define-key 'normal evil-emms-keymap-mode "?" 'describe-mode)
;; (evil-define-key 'normal evil-emms-keymap-mode "C" 'emms-browser-clear-playlist)
;; (evil-define-key 'normal evil-emms-keymap-mode "E" 'emms-browser-expand-all)
;; (evil-define-key 'normal evil-emms-keymap-mode "d" 'emms-browser-view-in-dired)
;; (evil-define-key 'normal evil-emms-keymap-mode "D" 'emms-browser-delete-files)
;; (evil-define-key 'normal evil-emms-keymap-mode "q" 'emms-browser-bury-buffer)
;; (evil-define-key 'normal evil-emms-keymap-mode "r" 'emms-browser-goto-random)
;; (evil-define-key 'normal evil-emms-keymap-mode "n" 'next-line)
;; (evil-define-key 'normal evil-emms-keymap-mode "p" 'previous-line)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "C-/" 'emms-playlist-mode-undo)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "<C-return>" 'emms-browser-add-tracks-and-play)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "<backtab>" 'emms-browser-prev-non-track)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "<tab>" 'emms-browser-next-non-track)

;; ;; (evil-define-key 'normal evil-emms-keymap-mode "sA" 'emms-browser-search-by-album)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "sa" 'emms-browser-search-by-artist)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "sc" 'emms-browser-search-by-composer)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "ss" 'emms-browser-search-by-names)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "st" 'emms-browser-search-by-title)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "sp" 'emms-browser-search-by-performer)

;; ;; (evil-define-key 'normal evil-emms-keymap-mode "b1" 'emms-browse-by-artist)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "b2" 'emms-browse-by-album)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "b3" 'emms-browse-by-genre)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "b4" 'emms-browse-by-year)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "b5" 'emms-browse-by-composer)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "b6" 'emms-browse-by-performer)

;; ;; (evil-define-key 'normal evil-emms-keymap-mode "Wap" 'emms-browser-lookup-album-on-pitchfork)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "Waw" 'emms-browser-lookup-album-on-wikipedia)

;; ;; (evil-define-key 'normal evil-emms-keymap-mode "WAp" 'emms-browser-lookup-artist-on-pitchfork)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "WAw" 'emms-browser-lookup-artist-on-wikipedia)

;; ;; (evil-define-key 'normal evil-emms-keymap-mode "WCp" 'emms-browser-lookup-composer-on-pitchfork)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "WCw" 'emms-browser-lookup-composer-on-wikipedia)

;; ;; (evil-define-key 'normal evil-emms-keymap-mode "WPp" 'emms-browser-lookup-performer-on-pitchfork)
;; ;; (evil-define-key 'normal evil-emms-keymap-mode "WPw" 'emms-browser-lookup-performer-on-wikipedia)
;;     ;; "T" 'emms-browser-delete-files-by-moving-to-trash
;; ;;      "RET"
;; ;;      "SPC"
;;      ;;  )
    )
  )
;;(defun media/init-emms-info-libtag ()
;;  (use-package emms-info-libtag
;;    :init
;;    )
;;  )
(defun media/init-emms-state ()
  (use-package emms-state
    :config
    (emms-state-mode 0)
    ))

(defun media/init-helm-emms ()
  (use-package helm-emms)
  )
;;; packages.el ends here
