;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATHS — Your LaTeX book + Org-roam directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq axiom-root "~/Personal-Projects/LaTeX/Axiom/text/")
(setq org-roam-directory (file-truename "~/Personal-Projects/LaTeX/Axiom/text/org/"))
(setq org-roam-dailies-directory "journal/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-ROAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org-roam
  :custom
  ;; Set your Org-roam directory
  (org-roam-directory "~/Personal-Projects/LaTeX/Axiom/text/org")   
  :config
  ;; Keep the database in sync automatically
  (org-roam-db-autosync-mode)

  ;; Capture templates
  (setq org-roam-capture-templates
        '(("n" "note" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+filetags:\n")
           :unnarrowed t)))

  ;; Daily templates
  (setq org-roam-dailies-capture-templates
        '(("d" "Default daily" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: daily\n\n* Links to Book Chapters\n- ")
           :unnarrowed t))))

;; Ensure IDs are created automatically when needed
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Batch-generate IDs for all Org-roam files (run once)
(after! org-roam
  (require 'org-id)
  (org-id-update-id-locations (org-roam-list-files)))

;; add ID

;; Custom command to add IDs to all Org-roam files
(defun my/org-roam-add-missing-ids ()
  "Add :ID: properties to all Org-roam files that lack them."
  (interactive)
  (require 'org-id)
  (dolist (file (org-roam-list-files))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      ;; If no ID exists in the file, create one
      (unless (org-entry-get (point-min) "ID")
        (org-id-get-create))
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-ROAM UI (GRAPH SERVER)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))
(setq org-roam-graph-viewer nil) ;; or your preferred browser

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-BABEL AND JUPYTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! org
  (setq org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (julia . t)
     (C . t)
     (fortran . t)
     (shell . t)
     (jupyter . t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUCTeX & PDF-TOOLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AUCTeX automatically detects master file
(setq TeX-master nil)

(use-package! pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))
(add-hook 'LaTeX-mode-hook 'cdlatex-mode)
(add-hook 'LaTeX-mode-hook #'company-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-command-default "LatexMk")
(setq TeX-command-list
      '(("LatexMk" "latexmk -pdf -interaction=nonstopmode %s"
         TeX-run-TeX nil t :help "Run LatexMk")))
(add-hook 'LaTeX-mode-hook 'preview-mode)
(add-hook 'LaTeX-mode-hook #'yas-minor-mode)
(add-hook 'LaTeX-mode-hook #'flycheck-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM FUNCTION — Link Org note to LaTeX chapter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun axiom/link-chapter ()
  "Insert a link to a LaTeX chapter file within ~/Personal-Projects/LaTeX/Axiom/text/."
  (interactive)
  (let* ((root (concat axiom-root "text/"))
         (file (read-file-name "Choose chapter: " root)))
    (insert (format "[[file:%s][%s]]"
                    (file-relative-name file (file-name-directory (buffer-file-name)))
                    (file-name-base file)))))

(map! :leader
      :desc "Link to a LaTeX chapter"
      "n l" #'axiom/link-chapter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP — Python, C/C++, Fortran, Julia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting t
        lsp-headerline-breadcrumb-enable t
        lsp-idle-delay 0.5
        lsp-log-io nil))

(add-hook 'python-mode-hook #'lsp)
(add-hook 'julia-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'fortran-mode-hook #'lsp)
(add-hook 'f90-mode-hook #'lsp!)
(add-hook 'LaTeX-mode-hook #'lsp!)
(setq lsp-tex-server 'texlab)
(after! lsp-mode
  (setq lsp-fortran-server 'fortls)
  (setq lsp-fortran-server-command "fortls"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECTILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! projectile
  (add-to-list 'projectile-project-search-path "~/Personal-Projects/LaTeX/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TREEMACS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq treemacs-is-never-other-window t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TURN LATEX TO ORG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-org-nodes-from-tex ()
  "Create Org-roam chapter nodes for all .tex files in main/ and text/, including subfolders."
  (interactive)
  (let ((chapters-dir (concat "~/Personal-Projects/LaTeX/Axiom/org/chapters/")))
    ;; Create the chapters directory if it doesn't exist
    (make-directory chapters-dir t)
    
    ;; Collect all .tex files from main/ and text/ recursively
    (let ((tex-files (append
                      (directory-files-recursively "~/Personal-Projects/LaTeX/Axiom/main/" "\\.tex$")
                      (directory-files-recursively "~/Personal-Projects/LaTeX/Axiom/text/" "\\.tex$"))))
      ;; Loop through all .tex files
      (dolist (file tex-files)
        (let* ((slug (file-name-base file))
               (org-file (concat chapters-dir slug ".org")))
          ;; Only create if file doesn't exist
          (unless (file-exists-p org-file)
            (with-temp-file org-file
              (insert "#+TITLE: " slug "\n")
              (insert "#+FILETAGS: chapter\n\n")
              (insert "* Overview\n- Link to LaTeX file: [[file:" file "][" slug ".tex]]\n")
              (insert "* Notes\n- Add your notes here\n\n")
              (insert "* Key Concepts\n- List concepts here\n\n"))
            ;; Open the file so you can start writing immediately
            (find-file org-file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn Org into LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/export-daily-org-to-LaTeX ()
  "Export all :daily: tagged Org files into one LaTeX chapter file.
Each journal entry becomes a \\section{TITLE}. Headings (*) become
LaTeX subsections/subsubsections. Output is safe, clean, and contains
no Org metadata directives."
  (interactive)
  (require 'ox)
  (require 'ox-latex)

  (let* ((org-dir (expand-file-name "~/Personal-Projects/LaTeX/Axiom/text/org/journal/"))
         (export-file (expand-file-name "~/Personal-Projects/LaTeX/Axiom/text/part_5/daily-notes.tex"))
         (files (directory-files-recursively org-dir "\\.org$"))
         (out-buf (get-buffer-create "*daily-export*"))
         (count 0))

    ;; Start with a clean output buffer
    (with-current-buffer out-buf
      (erase-buffer)
      (insert "% Auto-generated Daily Notes\n\n"))

    ;; Loop through *.org files
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (org-element-cache-reset)

        ;; Extract FILETAGS safely
        (goto-char (point-min))
        (let* ((tags-line (when (re-search-forward "^#\\+FILETAGS:[ \t]*\\(.*\\)$" nil t)
                            (downcase (match-string 1))))
               (tags (and tags-line (delq "" (split-string tags-line ":" t)))))

          ;; Only include daily-tagged files
          (when (member "daily" tags)
            ;; Extract TITLE, fallback to filename
            (goto-char (point-min))
            (let ((title (if (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" nil t)
                             (string-trim (match-string 1))
                           (file-name-base file))))

              ;; Remove metadata at top (#TITLE, #FILETAGS, #PROPERTY sections)
              (goto-char (point-min))
              (while (looking-at "^#\\+")
                (forward-line 1))
              (let ((pure-org (buffer-substring-no-properties (point) (point-max))))

                ;; Org → LaTeX body-only export (FIXED VERSION)
                (setq latex-body
                      (org-export-string-as pure-org 'latex t '(:body-only t)))

                ;; Insert into final buffer
                (when (and latex-body (not (string-empty-p latex-body)))
                  (with-current-buffer out-buf
                    (insert (format "\\section{%s}\n\n" title))
                    (insert latex-body)
                    (insert "\n\n"))
                  (setq count (1+ count)))))))))

    ;; Make sure directory exists
    (make-directory (file-name-directory export-file) t)

    ;; Write final file
    (with-current-buffer out-buf
      (write-region (point-min) (point-max) export-file))

    (message "Exported %d daily entries → %s" count export-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq calc-language 'latex)
(setq calc-embedded-open "[[")
(setq calc-embedded-close "]]")
(setq calc-embedded-update t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FORTRAN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-close 'end do', 'end if', 'end subroutine', etc.
(setq f90-smart-end t)

;; compile FORTRAN
(defun my/fortran-build ()
  "Compile the Fortran project using make."
  (interactive)
  (compile "make -k -j8"))
                                        ; make it runable from SPC mb
(map! :leader
      :desc "Build Fortran project"
      "mb" #'my/fortran-build)
(defun my/fortran-run ()
  "Run the executable after build."
  (interactive)
  (compile "./a.out"))
(defun my/fortran-build-and-run ()
  (interactive)
  (compile "make -j8 && ./a.out"))
(map! :leader "mr" #'my/fortran-build-and-run)
(defun my/fortran-debug ()
  (interactive)
  (compile "gfortran -Wall -Wextra -pedantic -Og -g *.f90 -o debug.out"))
(defun my/fortran-opt ()
  (interactive)
  (compile "gfortran -O3 -march=native -funroll-loops *.f90 -o fast.out"))
(map! :leader
      "m1" #'my/fortran-debug
      "m2" #'my/fortran-opt)
(setq compilation-scroll-output t)
(add-hook 'fortran-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'fprettify nil t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load! "../Personal-Projects/emacs/org-schedule-export.el")
