;;; Commentary: org-schedule-export.el --- Export org schedules to JSON -*- lexical-binding: t; -*-

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
  "Extract level-2 headlines and their direct list items."
  (let ((results (make-hash-table :test #'equal)))
    (org-element-map ast 'headline
      (lambda (hl)
        (when (= (org-element-property :level hl) 2)
          (let ((title (org-element-property :raw-value hl))
                (items '()))
            (org-element-map
                (org-element-contents hl)
                'item
              (lambda (it)
                (let ((parent (org-element-property :parent it)))
                  (when (eq (org-element-type parent) 'plain-list)
                    (let ((text (schedule/org-item-text it)))
                      (when text
                        (push text items))))))
              nil
              nil)
            (when items
              (puthash
               title
               (append (gethash title results)
                       (nreverse items))
               results))))))
    (let (out)
      (maphash
       (lambda (key value)
         (push
          `((category . ,key)
            (items . ,(delete-dups value)))
          out))
       results)
      (nreverse out))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Grouping (by date)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
         (tasks (schedule/org-extract-headline-lists ast))
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
