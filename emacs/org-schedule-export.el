;;; org-schedule-export.el --- Export org schedules to JSON -*- lexical-binding: t; -*-

;; ----------------------------
;; Dependencies
;; ----------------------------

(require 'org)
(require 'org-element)
(require 'json)

;; ----------------------------
;; Parsing
;; ----------------------------

(defun schedule/org-parse-file (file)
  "Parse FILE as org and return its org-element AST."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-element-parse-buffer)))

;; ----------------------------
;; Timestamp utilities
;; ----------------------------

(defun schedule/org-ts-to-iso (ts)
  "Convert org timestamp TS to ISO date string (YYYY-MM-DD).
Return nil if TS is nil."
  (when ts
    (format-time-string
     "%Y-%m-%d"
     (org-timestamp-to-time ts))))

;; ----------------------------
;; Task extraction
;; ----------------------------

(defun schedule/org-extract-headline-lists (ast)
  "Extract headlines and their list items.

Each level-2+ headline becomes a category.
Each '-' list item under it becomes a task."
  (let (results)
    (org-element-map ast 'headline
      (lambda (hl)
        (let ((level (org-element-property :level hl))
              (title (org-element-property :raw-value hl)))
          ;; Only classify ** and deeper
          (when (>= level 2)
            (let (items)
              ;; Search only inside this headline
              (org-element-map hl 'item
                (lambda (it)
                  (push
                   (org-element-interpret-data
                    (org-element-contents it))
                   items)))
              (when items
                (push
                 `((category . ,title)
                   (items . ,(nreverse items)))
                 results))))))
    (nreverse results)))


;; ----------------------------
;; Grouping (by scheduled date)
;; ----------------------------

(defun schedule/group-tasks-by-date (tasks)
  "Group TASKS alist by scheduled date."
  (let (table)
    (dolist (task tasks)
      (let ((date (cdr (assoc 'scheduled task))))
        (push task (alist-get date table nil nil #'equal))))
    table))

;; ----------------------------
;; Export
;; ----------------------------

(defun schedule/org-export-json (org-file json-file &optional group)
  "Export ORG-FILE to JSON-FILE.

If GROUP is non-nil, group tasks by scheduled date."
  (let* ((ast   (schedule/org-parse-file org-file))
         (tasks (schedule/org-extract-tasks ast))
         (data  (if group
                    (schedule/group-tasks-by-date tasks)
                  tasks)))
    (with-temp-file json-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode data))))))

;; ----------------------------
;; Convenience commands
;; ----------------------------

(defun schedule/export-current-buffer ()
  "Export current org buffer to schedule.json in the same directory."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not an org buffer"))
  (let* ((org-file  (buffer-file-name))
         (json-file (concat (file-name-directory org-file)
                            "schedule.json")))
    (schedule/org-export-json org-file json-file t)
    (message "Exported schedule to %s" json-file)))

;; ----------------------------
;; Optional auto-export on save
;; ----------------------------

(defun schedule/org-auto-export ()
  "Automatically export org schedules on save."
  (when (and buffer-file-name
             (string-equal (file-name-extension buffer-file-name) "org"))
    (schedule/org-export-json
     buffer-file-name
     (concat (file-name-directory buffer-file-name)
             "schedule.json")
     t)))

;;; Uncomment if you want auto-export
;; (add-hook 'after-save-hook #'schedule/org-auto-export)

(provide 'org-schedule-export)
;;; org-schedule-export.el ends here
