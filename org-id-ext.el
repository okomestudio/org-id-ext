;;; org-id-ext.el --- org-id extension  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-id-ext
;; Version: 0.2.2
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

(defun org-id-ext-base62-to-int (s)
  "Convert base-62 string S to integer."
  (let ((result 0))
    (dotimes (i (length s))
      (let* ((c (aref s i))
             (v (cond
                 ((and (>= c ?0) (<= c ?9)) (- c ?0))
                 ((and (>= c ?A) (<= c ?Z)) (+ 10 (- c ?A)))
                 ((and (>= c ?a) (<= c ?z)) (+ 36 (- c ?a)))
                 (t (error "Invalid base62 character: %c" c)))))
        (setq result (+ (* result 62) v))))
    result))

(defun org-id-ext-ts-b62-to-time (id)
  "Convert `ts-b62' ID to Emacs time.
The prefix in ID is removed, if it exists."
  (let* ((unique (if (string-match "^[^:]*:\\(.*\\)$" id)
                     (match-string 1 id)
                   id))
         (ts-ms (org-id-ext-base62-to-int unique)))
    (seconds-to-time (/ ts-ms 1000.0))))

(defun org-id-ext-ts-b62-p (id)
  "Return non-nil if ID conforms to `ts-b62' format."
  (and (stringp id)
       (string-match-p
        (rx bos
            (optional (+ (not (any ":"))) ":")
            (+ (any "0-9a-zA-Z"))
            eos)
        id)))

(provide 'org-id-ext)
;;; org-id-ext.el ends here
