;;; packages.el --- media layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alejandro Erickson <alejandro.erickson@gmail.com>
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
    ;; We need this recipe because MELPA version doesn't download the taglib metadata reader
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
    :defer t
    :init
    (progn
      ;; TODO: find a better global key, more evily
      (global-set-key [(f7)] 'emms-smart-browse)
      (add-hook 'emms-browser-show-display-hook 'evil-initialize)
      (add-hook 'emms-stream-hook 'evil-initialize)
      )
    :config
    (progn
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

      (evilified-state-evilify-map emms-stream-mode-map
        :mode emms-stream-mode
        )
      (evilified-state-evilify-map emms-mark-mode-map
        :mode emms-mark-mode
        :bindings
        "t" 'emms-mark-toggle
        "u" 'emms-mark-unmark-forward
        "K" 'emms-mark-kill-marked-tracks
        "M" 'emms-mark-mode-disable
        )
      (evilified-state-evilify-map emms-playlist-mode-map
        :mode emms-playlist-mode
        :bindings
        "l" 'emms-next
        "h" 'emms-previous
        "H" 'emms-playlist-mode-first
        "L" 'emms-playlist-mode-last
        "W" 'emms-playlist-save
        ;; P also works for emms-pause but it's kind of a stupid binding.
        ;; can't use SPC, so we'll make do with TAB
        (kbd "TAB") 'emms-pause
        "," 'emms-seek-minute-backward
        "." 'emms-seek-minute-forward
        "u" 'emms-playlist-mode-undo
        "p" 'emms-playlist-mode-yank
        "P" 'emms-playlist-mode-yank-pop
        "O" 'emms-playlist-mode-insert-newline
        ;; having trouble with this because it is
        ;; sometimes calling 'emms-playlist-mode-current-kill
        "K" 'emms-mark-kill-marked-tracks
        "M" 'emms-mark-mode
        )
      (evilified-state-evilify-map emms-browser-mode-map
        :mode emms-browser-mode
        :bindings
        ;; since this is normally SPC
        "t" 'emms-browser-toggle-subitems
        ;; makes more sense than C-j
        (kbd "<S-return>") 'emms-browser-add-tracks-and-play
        )
      ;; TODO: emms-browser search mode keybindings
      ) 
    )
  )

(defun media/init-emms-state ()
  (use-package emms-state
    ;; for some reason if this is deferred you can't bring up the smart browser.
    :config
    (emms-state-mode 0)
    ))

(defun media/init-helm-emms ()
  (use-package helm-emms
    :defer t
    )
  )

(defun media/post-init-emms ()
    (defun emms-browser-delete-files-by-moving-to-trash ()
      "Move all files under point to trash.
Disabled by default."
      (interactive)
      (let ((tracks (emms-browser-tracks-at-point))
            dirs path)
        (unless (yes-or-no-p
                 (format "Really permanently move these %d tracks to trash? "
                         (length tracks)))
          (error "Cancelled!"))
        (message "Moving files to trash..")
        (dolist (track tracks)
          (setq path (emms-track-get track 'name))
          (move-file-to-trash path)
          (add-to-list 'dirs (file-name-directory path))
          (emms-cache-del path))
        ;; remove empty dirs
        (dolist (dir dirs)
          (run-hook-with-args 'emms-browser-delete-files-by-moving-to-trash-hook dir tracks)
          (condition-case nil
              (delete-directory dir)
            (error nil)))
        ;; remove the item from the browser
        (emms-browser-delete-current-node)
        (message "Moving files to trash..done")))

    ;; disable this function so you have to do an extra confirmation to use it.
    (put 'emms-browser-delete-files-by-moving-to-trash  'disabled t)


(defun emms-seek-minute-backward()
  "Seek one minute backward."
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek (- 1))))

(defun emms-seek-minute-forward ()
  "Seek one minute forward."
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek 1)))

;; Adding some streams here.  Delete others that no longer work

(require 'emms-streams)
(defvar emms-stream-default-list
      '(("SomaFM: Beatblender"
         "http://www.somafm.com/beatblender.pls" 1 streamlist)
        ("SomaFM: Secret Agent"
         "http://www.somafm.com/secretagent.pls" 1 streamlist)
        ("SomaFM: Groove Salad"
         "http://www.somafm.com/groovesalad.pls" 1 streamlist)
        ("SomaFM: Drone Zone"
         "http://www.somafm.com/dronezone.pls" 1 streamlist)
        ("SomaFM: Tag's Trance"
         "http://www.somafm.com/tagstrance.pls" 1 streamlist)
        ("SomaFM: Indie Pop Rocks"
         "http://www.somafm.com/indiepop.pls" 1 streamlist)
        ("SomaFM: Doomed"
         "http://www.somafm.com/doomed.pls" 1 streamlist)
        ;; DI now only available with premium account.
        ;;("Digitally Imported, Trance"
        ;; "http://www.di.fm/mp3/trance.pls" 1 streamlist)
        ;; ("Digitally Imported, Deephouse"
        ;;  "http://www.di.fm/mp3/deephouse.pls" 1 streamlist)
        ;; ("Digitally Imported, Chillout"
        ;;  "http://www.di.fm/mp3/chillout.pls" 1 streamlist)
        ;; ("Digitally Imported, Drum and Bass"
        ;;  "http://www.di.fm/mp3/drumandbass.pls" 1 streamlist)
        ;; SKY only available with premium account.
        ;; ("SKY.fm, Mostly Classical"
        ;;  "http://www.sky.fm/mp3/classical.pls" 1 streamlist)
        ;; ("SKY.fm, Jazz"
        ;;  "http://www.sky.fm/mp3/jazz.pls" 1 streamlist)
        ;; Appears to be defunct
        ;; ("Philosomatika, Goa-Trance"
        ;;  "http://www.shoutcast.com/sbin/shoutcast-playlist.pls?rn=1712&file=filename.pls" 1 streamlist)
        ("P H I L O S O M A T I K A - Progressive Psytrance"
         "http://listen.radionomy.com:80/-PHILOSOMATIKAPROGRESSIVE-" 1 url)
        ("P H I L O S O M A T I K A - Psytrance"
         "http://listen.radionomy.com:80/-PHILOSOMATIKA-" 1 url)
        ("Drum and Bass Radio, BassDrive"
         "http://www.bassdrive.com/BassDrive.m3u" 1 streamlist)
        ;; These appear to be defunct
        ;; ("Flaresound, Jazzmusique"
        ;;  "http://64.236.34.196:80/stream/1016" 1 url)
        ;; ("Flaresound, Jazzmusique"
        ;;  "http://205.188.234.4:8004" 2 url)
        ;; ("Flaresound, L'Electric"
        ;;  "http://www.bp6.com:8002" 1 url)
        ;; ("Stangs Garage, Eclectic"
        ;;  "http://www.stangsgarage.com/listen.pls" 1 streamlist)
        ;; ("DNA Lounge, Live"
        ;;  "http://www.dnalounge.com/webcast/128.m3u" 1 streamlist)
        ;; ("DNA Lounge Radio"
        ;;  "http://www.dnalounge.com/webcast/dnaradio.m3u" 1 streamlist)
        ;; ("Virgin Radio, The Groove"
        ;;  "http://www.smgradio.com/core/audio/ogg/live.pls?service=grbb"
        ;;  1 streamlist)
        ;; ("Virgin Radio, Virgin Classic"
        ;;  "http://www.smgradio.com/core/audio/ogg/live.pls?service=vcbb"
        ;;  1 streamlist)
        ;; ("Virgin Radio, Virgin 1215AM"
        ;;  "http://www.smgradio.com/core/audio/ogg/live.pls?service=vrbb"
        ;;  1 streamlist)
        ;; ("Voices From Within - Words From Beyond"
        ;;  "http://207.200.96.225:8024/listen.pls" 1 streamlist)
        ("WCPE, Classical Music"
         "http://www.ibiblio.org/wcpe/wcpe.pls" 1 streamlist)
        ;; Defunct
        ;; ("PLUG: Voices of the Free Software movement"
        ;;  "http://purduelug.org:8000/voices-free_software.ogg" 1 url)
        ;; ("VGamp Radio, Video Game music"
        ;;  "http://vgamp.com/listen128.pls" 1 streamlist)
        ("Kohina - Old school game and demo music"
         "http://stream.nute.net/kohina/stream.ogg.m3u" 1 streamlist)
        ;; Changed url, see below
        ;; ("Nectarine, Demoscene Radio"
        ;;  "http://www.scenemusic.eu:8002/high.ogg.m3u" 1 streamlist)
        ;; Nectarine radio stations updated
        ("Nectarine, Demoscene Radio, DE Continuum's relay 192 mp3"
         "http://privat.is-by.us:8000/necta192.mp3.m3u" 1 streamlist)
        ("Nectarine, Demoscene Radio, DE stream (High Bitrate)"
         "http://nectarine.from-de.com/necta192.m3u" 1 streamlist)
        ("Nectarine, Demoscene Radio, FR stream (High Bitrate)"
         "http://necta-relay.mnus.de:8000/necta192.mp3.m3u" 1 streamlist)
        ("Nectarine, Demoscene Radio, Norwegian stream"
         "http://pmaster.no:9000/necta.m3u" 1 streamlist)
        ("Nectarine, Demoscene Radio, UK stream (High Bitrate)"
         "http://necta.jansenit.com:8000/necta192.mp3.m3u" 1 streamlist)
        ("idobi Radio"
         "http://www.idobi.com/radio/iradio.pls" 1 streamlist)
        ("radio.wazee - Modern Alternative Rock"
         "http://www.wazee.org/128.pls" 1 streamlist)
        ;; Defunct
        ;; ("ChroniX Aggression - Loud & Clear"
        ;;  "http://www.chronixradio.com/chronixaggression/listen/listen.pls"
        ;;  1 streamlist)
        ("WFMU, Freeform radio"
         "http://www.wfmu.org/wfmu.pls" 1 streamlist)
        ;; Defunct
        ;; ("KEXP - Seattle Community Radio"
        ;;  "http://kexp-mp3-128k.cac.washington.edu:8000/listen.pls" 1 streamlist)
        ;; Defunct
        ;; ("KRUU-LP - Fairfield, Iowa Community Radio"
        ;;  "http://kruufm.com/live.pls" 1 streamlist)
        ("WBCR-LP - Berkshire Community Radio"
         "http://nyc01.egihosting.com:6232/listen.pls" 1 streamlist))
      )

  )

;;; packages.el ends here
