;;; es-lib-number-at-point.el --- Manipulate numbers at point
;;; Version: 0.1
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib

;;; Commentary:

;; The project is hosted at https://github.com/sabof/es-lib
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
(require 'cl-lib)
(require 'es-lib-core-macros)

(cl-defun es-number-at-point ()
  (unless (looking-at-p "[[:digit:]]")
    (cl-return-from es-number-at-point))
  (let (allow-negative
        fixed-width)
    (save-excursion
      (skip-chars-backward "0123456789")
      (when (< (point-min) (point))
        (backward-char)
        (unless (looking-at-p "\\_<-")
          (forward-char)))
      (setq allow-negative (looking-at-p "\\_<"))
      (unless (looking-at-p "-?[[:digit:]]+")
        (cl-return-from es-number-at-point))
      (looking-at "-?[[:digit:]]+")
      (when (string-match-p "-?0+[[:digit:]]$"
                            (match-string-no-properties 0))
        (setq fixed-width (length (match-string-no-properties 0))))
      (list (match-string-no-properties 0)
            (match-beginning 0)
            (match-end 0)
            allow-negative
            fixed-width))))

(cl-defun es--change-number-at-point (&optional ammout)
  (let ((number (es-number-at-point)))
    (if (not number)
        (progn (let (( end-distance (- (line-end-position) (point))))
                 (when (re-search-backward "[[:digit:]]" (line-beginning-position) t)
                   (es--change-number-at-point ammout))
                 (goto-char (- (line-end-position) end-distance))))
      (cl-multiple-value-bind
          (num-string beg end allow-negative fixed-width)
          number
        (let* (( start-pos (point))
               ( distance-from-end (- end start-pos))
               ( increment (* (expt 10 (1- distance-from-end))
                              (or ammout 1)))
               ( result (+ (string-to-number num-string) increment))
               ( --- (unless allow-negative
                       (setq result (max 0 result))))
               ( result-string (if fixed-width
                                   (format (format "%%0%dd" fixed-width)
                                           result)
                                 (number-to-string result)))
               ( suggested-new-pos (+ start-pos
                                      (length result-string)
                                      (- (length num-string)))))
          ;; Don't create an unnecessary undo state
          (when (string-equal num-string result-string)
            (cl-return-from es--change-number-at-point))
          (delete-region beg end)
          (insert result-string)

          (goto-char (max beg suggested-new-pos))
          (skip-chars-forward "-")
          )))))

;;;###autoload
(defun es-increase-number-at-point ()
  "Increases the digit at point.
The increment some power of 10, depending on the positon of the cursor. If there
is no number at point, will try to increment the previous number on the same
line."
  (interactive)
  (unless (es-toggle-true-false-maybe)
    (es--change-number-at-point)))

;;;###autoload
(defun es-decrease-number-at-point ()
  "See documentation for `es-increase-number-at-point'."
  (interactive)
  (unless (es-toggle-true-false-maybe)
    (es--change-number-at-point -1)))

(provide 'es-lib-number-at-point)
;; es-lib-number-at-point.el ends here
