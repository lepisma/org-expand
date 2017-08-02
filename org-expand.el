;;; org-expand.el --- Create content by /expanding/ org-entries   -*- lexical-binding: t -*-

;; Copyright (C) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 0.1.4
;; Package-Requires: ((enlive "0.0.1") (s "1.11.0"))
;; URL: https://github.com/lepisma/org-expand

;;; Commentary:

;; org-expand.el takes in an org entry and writes an expanded version using an expand function.
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'enlive)
(require 'org)
(require 's)

(defgroup org-expand nil
  "Org expand"
  :group 'org)

(defun org-expand-read-entry (&optional point)
  "Read the org entry at given point and return an object.")

(defclass org-expand-entry ()
  ((title :initarg :title
          :initform ""
          :type string
          :documentation "Entry header")
   (body :initarg :body
         :initform ""
         :documentation "Body of entry. This can be a list which can then have
other entries, or string in it as items.")
   (tags :initarg :tags
         :type cons
         :documentation "List of tags applied to the item")
   (props :initarg :props
          :type cons))
  "An org entry")

(defmethod org-expand-write-entry ((entry org-expand-entry) &optional point)
  "Write the given entry at given point.")

(defmethod org-expand-wikipedia ((entry org-expand-entry))
  "Return wikipedia intro paragraph.")

(defmethod org-expand-discography ((entry org-expand-entry))
  "Return tracklist for given artist, album")

(defmethod org-expand-youtube-url ((entry org-expand-entry))
  "Return entry with youtube-url set as a property.")

;;;###autoload
(defun org-expand ()
  "Run org expand")

(provide 'org-expand)
;;; org-expand.el ends here
