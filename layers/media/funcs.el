
;;; funcs.el --- media layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alejandro Erickson <alejandro.erickson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
