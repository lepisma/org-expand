;;; org-expand.el --- Create content by /expanding/ org-entries   -*- lexical-binding: t -*-

;; Copyright (C) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 0.1.4
;; Package-Requires: ((request "0.3.0") (helm "2.8.1") (enlive "0.0.1") (s "1.11.0"))
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

(require 'helm)
(require 'enlive)
(require 'json)
(require 'org)
(require 'request)
(require 's)

(defgroup org-expand nil
  "Org expand"
  :group 'org)

(defclass org-expand-entry ()
  ((title :initarg :title
          :initform ""
          :type string
          :documentation "Entry header")
   (body :initarg :body
         :initform ""
         :documentation "Body text of entry")
   (tags :initarg :tags
         :documentation "List of tags applied to the item")
   (props :initarg :props
          :documentation "Properties for the entry"))
  "An org entry")

(defun org-expand--get-entry-body ()
  "Return plain body text for org entry at current point."
  (let ((prop-bounds (org-get-property-block))
        (entry-text (substring-no-properties (org-get-entry))))
    (string-trim
     (if prop-bounds
         (substring entry-text (+ 19 (- (cdr prop-bounds) (car prop-bounds))))
       entry-text))))

(defun org-expand-read-entry ()
  "Read the org entry at current point and return an object."
  (make-instance 'org-expand-entry
                 :title (substring-no-properties (org-get-heading t t))
                 :props (org-entry-properties)
                 :tags (org-get-tags-at)
                 :body (org-expand--get-entry-body)))

(defmethod org-expand-wikipedia-summary ((entry org-expand-entry) pos)
  "Return wikipedia intro paragraph."
  (org-expand-get-wikipedia-summary
   (slot-value entry :title)
   (lambda (summary)
     (save-excursion
       (goto-char pos)
       (insert summary)
       (fill-paragraph)))))

(defun org-expand-get-wikipedia-summary (term callback)
  "Search wikipedia for given term"
  (request
   "https://en.wikipedia.org/w/api.php"
   :params `(("format" . "json")
             ("action" . "query")
             ("prop" . "extracts")
             ("redirects" . "1")
             ("titles" . ,term)
             ("explaintext" . "1")
             ("exintro" . "1"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (funcall callback (cdr (assoc 'extract (cadr (assoc 'pages (assoc 'query data))))))))))

(defmethod org-expand-youtube-url ((entry org-expand-entry) pos)
  "Return entry with youtube-url set as a property."
  (org-expand-get-youtube-url
   (slot-value entry :title)
   (lambda (url)
     (save-excursion
       (goto-char pos)
       (org-set-property "youtube" url)))))

(defun org-expand--parse-youtube-search-output (output)
  "Parse web search result to get valid youtube links"
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (let ((results '()))
      (while (search-forward "watch?v=" nil t)
        (push (buffer-substring-no-properties (point) (+ 11 (point))) results))
      (mapcar (lambda (id) (concat "https://youtube.com/watch?v=" id)) (reverse results)))))

(defun org-expand-get-youtube-url (term callback)
  "Search youtube for given term"
  (request
   "https://youtube.com/results"
   :params `(("search_query" . ,term))
   :parser 'buffer-string
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (let ((links (org-expand--parse-youtube-search-output data)))
                 (funcall callback (car links)))))))

(defun org-expand (source-func)
  "Run expand at point using given source-function"
  (funcall source-func (org-expand-read-entry) (point)))

;;;###autoload
(defun helm-org-expand ()
  "Run helm for selecting org-expand sources"
  (interactive)
  (helm :sources (helm-build-sync-source "org-expand sources"
                   :candidates '("youtube-url" "wikipedia-summary")
                   :action '(("Run org-expand" . (lambda (candidate)
                                                   (org-expand
                                                    (intern-soft (concat "org-expand-" candidate)))))))
        :buffer "*helm org-expand*"
        :prompt "Select source: "))

(provide 'org-expand)
;;; org-expand.el ends here
