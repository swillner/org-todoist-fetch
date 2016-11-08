;;; org-todoist-fetch.el --- Fetch tasks from Todoist to org-mode

;; Copyright (C) 2016 Sven Willner <sven.willner@gmail.com>

;; Author: Sven Willner
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; Provides function org-todoist-fetch, which fetches all tasks from
;; Todoist, appends them to org-todoist-fetch-file, and finally deletes them
;; in Todoist.
;;

;;; Code:

(require 'json)
(require 'request-deferred)
(require 'uuidgen)

;; Customization
(defgroup org-todoist-fetch nil
  "Fetch tasks from Todoist to org-mode"
  :tag "Org Todoist"
  :group 'org)

(defcustom org-todoist-fetch-api-token nil
  "API token."
  :group 'org-todoist-fetch
  :type 'string)

(defcustom org-todoist-fetch-file nil
  "File to append Todoist tasks to."
  :group 'org-todoist-fetch
  :type 'file)

(defun org-todoist-fetch ()
  "Fetch all tasks from Todoist, append them to org-todoist-fetch-file, and delete them in Todoist."
  (interactive)
  (deferred:$
    (request-deferred "https://todoist.com/API/v6/sync"
                      :type "GET"
                      :params `(("token" . ,org-todoist-fetch-api-token)
                                ("seq_no" . "0")
                                ("resource_types" . "[\"items\"]"))
                      :parser (lambda ()
                                (let ((json-object-type 'plist))
                                  (json-read))))
    (deferred:nextc it
      (lambda (response)
        (let ((items (sort (mapcar 'identity (plist-get (request-response-data response) :Items))
                           (lambda (a b)
                             (< (plist-get a :item_order) (plist-get b :item_order)))))
              result)
          (if (> (length items) 0)
              (progn
                (setq result (mapconcat (lambda (item)
                                          (concat (make-string (plist-get item :indent) ?*)
                                                  " TODO"
                                                  (pcase (plist-get item :priority)
                                                    (2 " [#A]")
                                                    (3 " [#B]")
                                                    (4 " [#C]")
                                                    (_ ""))
                                                  " "
                                                  (plist-get item :content)
                                                  (when (plist-get item :due_date)
                                                    (format-time-string "\nSCHEDULED: <%Y-%m-%d %a>" (date-to-time (plist-get item :due_date))))
                                                  (format-time-string "\n[%Y-%m-%d %a %H:%M]" (date-to-time (plist-get item :date_added)))))
                                        items "\n"))
                (with-current-buffer (find-file-noselect org-todoist-fetch-file)
                  (save-excursion
                    (goto-char (point-max))
                    (insert result)
                    (save-buffer)))
                (deferred:$
                  (request-deferred "https://todoist.com/API/v6/sync"
                                    :type "GET"
                                    :params `(("token" . ,org-todoist-fetch-api-token)
                                              ("commands" . ,(concat "[{\"type\":\"item_delete\","
                                                                     "\"uuid\":\""
                                                                     (uuidgen-1)
                                                                     "\","
                                                                     "\"args\":{\"ids\":["
                                                                     (mapconcat (lambda (item)
                                                                                  (number-to-string (plist-get item :id))) items ",")
                                                                     "]}}]")))
                                    :parser (lambda ()
                                              (let ((json-object-type 'plist))
                                                (json-read))))
                  (deferred:nextc it
                    (lambda (_)
                      (message "Fetching Todoist complete")))))
            (message "No tasks fetched")))))))

(provide 'org-todoist-fetch)

;;; org-todoist-fetch.el ends here
