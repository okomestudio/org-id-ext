;;; org-id-ext.el --- org-id extension  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-id-ext
;; Version: 0.1.1
;; Keywords: org, convenience
;; Package-Requires: ((emacs "30.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This extension adds custom methods for ID generation. Currently, only the
;; following method is added:
;;
;;   - ts-b62: Unix timestamp in milliseconds in base-62 encoding
;;
;;; Code:

(require 'org-id)

(defun org-id-ext-new (fun &optional prefix)
  "Advise FUN to add `ts-b62' method.
PREFIX is passed to FUN."
  (cond
   ((eq org-id-method 'ts-b62)
    (let* ((prefix (if (eq prefix 'none)
		                   ""
		                 (concat (or prefix org-id-prefix) ":")))
           (ts (org-id-ext-timestamp-ms))
           (unique (org-id-ext-int-to-base62 ts)))
      (if (equal prefix ":") (setq prefix ""))
      (concat prefix unique)))
   (t (funcall fun prefix))))

(advice-add #'org-id-new :around #'org-id-ext-new)

(defun org-id-ext-timestamp-ms ()
  "Current timestamp in milliseconds."
  (car (time-convert (current-time) 1000)))

(defun org-id-ext-int-to-base62 (n)
  "Convert a positive integer N to a base-62 string.
Uses digits 0-9, A-Z, a-z, (62 characters total). Returns empty string if N is 0
or negative."
  (when (< n 0)
    (error "N must be zero or positive integer"))
  (let (result remainder)
    (while (> n 0)
      (setq remainder (% n 62))
      (push (cond
             ((< remainder 10) (+ ?0 remainder))
             ((< remainder 36) (+ ?A (- remainder 10)))
             (t (+ ?a (- remainder 36))))
            result)
      (setq n (/ n 62)))
    (apply #'string result)))

(provide 'org-id-ext)
;;; org-id-ext.el ends here
