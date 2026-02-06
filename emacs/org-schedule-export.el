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

(defun schedule/org-extract-tasks (ast)
  "Extract TODO headlines with scheduling info from org AST."
  (let (tasks)
    (org-element-map ast 'headline
      (lambda (hl)
        (let ((todo      (org-element-property :todo-keyword hl))
              (title     (org-element-property :raw-value hl))
              (scheduled (org-element-property :scheduled hl))
              (priority  (org-element-property :priority hl))
              (tags      (org-element-property :tags hl)))
          ;; Only export TODO headlines
          (when todo
            (push
             `((title     . ,title)
               (todo      . ,todo)
               (scheduled . ,(schedule/org-ts-to-iso scheduled))
               (priority  . ,priority)
               (tags      . ,tags))
             tasks)))))
    (nreverse tasks)))

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
