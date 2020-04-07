;;; audio-mode.el --- Major mode for viewing / playing audio, like image-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kisaragi Hiu

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Keywords: multimedia
;; Package-Requires: ((emacs "25") (mpv "0.1.0"))
;; Homepage: https://kisaragi-hiu.com/projects/audio-mode
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Images have `image-mode', yet audio doesn't have an `audio-mode'.
;; This bridges that gap.
;;
;; This is useful for playing audio from Dired.

;;; Code:

(require 'seq)
;; mpv.el requires org for whatever reason. This works for now.
(require 'mpv)

(defgroup audio ()
  "Audio mode."
  :prefix "audio-"
  :group 'multimedia)

(defcustom audio-mode-auto-play t
  "Whether to automatically play the audio with mpv or not."
  :type 'boolean
  :group 'audio)

;;;; Minor mode and major mode(s)
;; (defvar audio-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     map))

;;;###autoload
(define-derived-mode audio-mode special-mode "Audio"
  "Major mode for playing audio and viewing metadata of audio files.

Two commands, \\<audio-mode-map>\\[audio-toggle-display] and
\\<audio-mode-map>\\[audio-toggle-hex-display] are provided for
viewing the audio file as text or hex. This is just like
`image-mode'."
  (add-hook 'write-contents-functions (lambda () t) nil t)
  (when audio-mode-auto-play
    (mpv-play buffer-file-name))
  (setq-local revert-buffer-function #'audio-mode--revert-buffer)
  (audio-mode--revert-buffer))

;;;###autoload
(dolist (end '(".wav" ".mp3" ".ogg"))
  (add-to-list 'auto-mode-alist
               (cons (rx-to-string `(seq ,end eos) t)
                     #'audio-mode)))

(defvar audio-mode-duration-format "%.2h:%.2m:%.2s")

(defmacro audio-mode--make-command (&rest body)
  "Make a command named NAME for use in Audio mode.

BODY is the body of the command; the command discards any
arguments passed to it. If the command is run in an Audio mode
buffer, it is refreshed."
  `(lambda (&rest _)
     (interactive)
     ,@body
     (when (derived-mode-p 'audio-mode)
       (revert-buffer))))

(defun audio-mode--revert-buffer (&rest _)
  "Render content in audio mode buffer."
  (let ((cur-point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (insert (file-name-nondirectory buffer-file-name) "\n\n")
    (insert-text-button "<<" 'action (audio-mode--make-command (mpv-seek-backward 5)))
    (insert " ")
    (insert-text-button "Play" 'action (audio-mode--make-command (mpv-play buffer-file-name)))
    (insert " ")
    (insert-text-button "Pause" 'action (audio-mode--make-command (mpv-pause)))
    (insert " ")
    (insert-text-button ">>" 'action (audio-mode--make-command (mpv-seek-forward 5)))
    (insert "\n\n"
            (format-seconds audio-mode-duration-format 0)
            ;; TODO: replace with progress
            "    "
            (format-seconds audio-mode-duration-format
                            (or (mpv-get-playback-position) 0))
            "    "
            (format-seconds audio-mode-duration-format
                            (or (mpv-get-duration) 0))
            "\n\n")
    (goto-char (or cur-point (point-min)))
    (set-buffer-modified-p nil)))

(provide 'audio-mode)
;;; audio-mode.el ends here
