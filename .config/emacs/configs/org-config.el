;;; org-config.el -*- lexical-binding: t; -*-

;;; Code:

(defun yaz/new-school-file ()
  "Create and open a new school file."
  (interactive)
  (let* ((code (read-string "course code: "))
         (course-name (read-string "course name: "))
         (slug (format "%s_%s"
                       (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" code)
                       (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" (downcase course-name))))
         (file (expand-file-name (concat slug ".org") "~/org/school/")))
    (unless (file-exists-p "~/org/school/") (make-directory "~/org/school/" t))
    (if (file-exists-p file)
        (message "file already exists: %s" file)
      (with-temp-file file
        (insert (format "#+title: %s: %s\n" code course-name))
        (insert "#+filetags: :@school:\n")
        (insert (format "#+created: %s\n\n" (format-time-string "[%Y-%m-%d %A]")))
        (insert "* overview\n\n")
        (insert ":instructor: \n")
        (insert ":credits: \n")
        (insert ":schedule: \n")
        (insert ":grading: \n")
        (insert "* notes\n\n")
        (insert "* assignments\n\n")
        (insert "* exams\n\n")
        (insert "* resources\n\n")
        (insert "** textbooks\n\n")
        (insert "** links\n\n")))
    (find-file file)))

(defun yaz/new-project-file ()
  "Create and open a new project file."
  (interactive)
  (let* ((name (read-string "project name: "))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "_" (downcase name)))
         (file (expand-file-name (concat slug ".org") "~/org/projects/")))
    (unless (file-exists-p "~/org/projects/") (make-directory "~/org/projects/" t))
    (if (file-exists-p file)
        (message "file already exists: %s" file)
      (with-temp-file file
        (insert (format "#+title: %s\n" name))
        (insert "#+filetags: :@project:\n")
        (insert (format "#+created: %s\n\n" (format-time-string "[%Y-%m-%d %A]")))
        (insert "* goals\n\n")
        (insert "* tasks\n\n")
        (insert "* notes\n\n")
        (insert "* resources\n\n")))
    (find-file file)))

(defun yaz/new-learning-file ()
  "Create and open a new learning file."
  (interactive)
  (let* ((name (read-string "learning topic: "))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "_" (downcase name)))
         (file (expand-file-name (concat slug ".org") "~/org/learning/")))
    (unless (file-exists-p "~/org/learning/") (make-directory "~/org/learning/" t))
    (if (file-exists-p file)
        (message "file already exists: %s" file)
      (with-temp-file file
        (insert (format "#+title: %s\n" name))
        (insert "#+filetags: :@learning:\n")
        (insert (format "#+created: %s\n\n" (format-time-string "[%Y-%m-%d %A]")))
        (insert "* core concepts\n\n")
        (insert "* progress\n\n")
        (insert "* notes\n\n")))
    (find-file file)))

(defun yaz/find-school-file ()
  "Select and open an existing school file."
  (interactive)
  (let ((file (read-file-name "find course: " "~/org/school/" nil t)))
    (find-file file)))

(defun yaz/find-project-file ()
  "Select and open an existing project file."
  (interactive)
  (let ((file (read-file-name "find project: " "~/org/projects/" nil t)))
    (find-file file)))

(defun yaz/find-learning-file ()
  "Select and open an existing learning file."
  (interactive)
  (let ((file (read-file-name "find topic: " "~/org/learning/" nil t)))
    (find-file file)))

(defun yaz/open-org-file ()
  "Find any org file."
  (interactive)
  (let ((default-directory "~/org/"))
    (call-interactively 'find-file)))

(defun yaz/create-paper-review-from-bib ()
  "Create paper review from existing bib entry with auto-populated metadata."
  (interactive)
  (let* ((key (citar-select-ref))
         (entry (citar-get-entry key))
         (title (citar-get-value "title" entry))
         (authors (citar-get-value "author" entry))
         (year (citar-get-value "year" entry))
         (venue (or (citar-get-value "journal" entry)
                    (citar-get-value "booktitle" entry)
                    (citar-get-value "publisher" entry)
                    "unknown"))
         (url (or (citar-get-value "url" entry)
                  (citar-get-value "doi" entry)
                  ""))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "_" (downcase key)))
         (filename (format "~/org/papers/reviews/%s-%s.org"
                           (format-time-string "%Y%m%d%H%M%S")
                           slug))
         (pdf-file (format "~/org/papers/pdfs/%s.pdf" key)))

    (find-file filename)
    (insert (format "#+title: %s\n" title))
    (insert "#+filetags: :paper:review:\n")
    (insert "#+bibliography: ~/org/ref.bib\n")
    (insert "#+cite_export: csl\n\n")

    (insert "* metadata\n")
    (insert ":properties:\n")
    (insert (format ":authors: %s\n" authors))
    (insert (format ":year: %s\n" year))
    (insert (format ":venue: %s\n" venue))
    (insert (format ":pdf: [[file:%s]]\n" pdf-file))
    (insert (format ":cite_key: %s\n" key))
    (insert (format ":url: %s\n" url))
    (insert (format ":created: %s\n" (format-time-string "[%Y-%m-%d %A %H:%M]")))
    (insert (format ":status: %s\n" "unread"))
    (insert ":end:\n\n")

    (insert "* problem\n\n")
    (insert "* core idea\n\n")
    (insert "* method\n\n")
    (insert "* results\n\n")
    (insert "* weaknesses / questions\n\n")
    (insert "* connections\n\n")
    (insert "* references\n")
    (insert "#+print_bibliography:\n")

    (goto-char (point-min))
    (goto-char (point-min))
    (search-forward "* core idea")
    (forward-line 1)
    (message "paper review created for: %s" title)))

(defun yaz/create-paper-review-from-pdf ()
  "Create paper review by selecting a pdf file from the library."
  (interactive)
  (let* ((pdf-file (read-file-name "select pdf: " "~/org/papers/pdfs/"))
         (pdf-name (file-name-base pdf-file))
         (title (read-string "paper title: "))
         (authors (read-string "authors: "))
         (year (read-string "year: "))
         (venue (read-string "venue (conference/journal): "))
         (cite-key (read-string "citation key: " pdf-name))
         (url (read-string "url (optional): "))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "_" (downcase cite-key)))
         (filename (format "~/org/papers/reviews/%s-%s.org"
                           (format-time-string "%Y%m%d%H%M%S")
                           slug)))

    (when (y-or-n-p "add to ref.bib? ")
      (let ((bib-entry (format "\n@article{%s,\n  author = {%s},\n  title = {%s},\n  year = {%s},\n  url = {%s}\n}\n"
                               cite-key authors title year url)))
        (append-to-file bib-entry nil "~/org/ref.bib")
        (message "✓ added to ref.bib")))

    (find-file filename)
    (insert (format "#+title: %s\n" title))
    (insert "#+filetags: :paper:review:\n")
    (insert "#+bibliography: ~/org/ref.bib\n\n")
    (insert "* metadata\n")
    (insert ":properties:\n")
    (insert (format ":authors: %s\n" authors))
    (insert (format ":year: %s\n" year))
    (insert (format ":venue: %s\n" venue))
    (insert (format ":pdf: [[file:%s]]\n" pdf-file))
    (insert (format ":cite_key: %s\n" cite-key))
    (insert (format ":url: %s\n" url))
    (insert (format ":created: %s\n" (format-time-string "[%Y-%m-%d %A %H:%M]")))
    (insert (format ":status: %s\n" "unread"))
    (insert ":end:\n\n")
    (insert "* problem\n\n")
    (insert "* core idea\n\n")
    (insert "* method\n\n")
    (insert "* results\n\n")
    (insert "* weaknesses / questions\n\n")
    (insert "* connections\n\n")
    (goto-char (point-min))
    (goto-char (point-min))
    (search-forward "* core idea")
    (forward-line 1)
    (message "✓ review created: %s" title)))

(use-package org
  :config
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))

  (setq org-agenda-files '("~/org/todo.org"
                          "~/org/calendar.org"
                          "~/org/school/"
                          "~/org/projects/"))

  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-pretty-entities t)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width '(400))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-refile-targets '((nil :maxlevel . 3)
                            (org-agenda-files :maxlevel . 2)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-capture-templates
        '(("i" "Inbox" entry (file+headline "~/org/inbox.org" "Tasks")
           "* %?\n  %i\n  %a")
          ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("s" "School" entry (file+headline "~/org/todo.org" "School")
           "* TODO %? :@school:\n  DEADLINE: %^t")))

  (setq org-tag-alist '((:startgroup . nil)
                       ("@school" . ?s)
                       ("@project" . ?p)
                       ("@research" . ?r)
                       ("@learning" . ?l)
                       ("@personal" . ?e)
                       (:endgroup . nil)
                       ("urgent" . ?u)
                       ("important" . ?i)
                       ("review" . ?v)
                       ("reading" . ?d)
                       ("coding" . ?c)))

  (setq org-archive-location "~/org/archive/%s_archive::")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (java . t)
     (C . t)
     (shell . t)
     (emacs-lisp . t)
     (sql . t)))

    (setq org-agenda-custom-commands
          '(("d" "dashboard"
             ((agenda "" ((org-agenda-span 'day)
                         (org-deadline-warning-days 7)
                         (org-agenda-overriding-header "today's schedule")))
              (tags-todo "urgent"
                        ((org-agenda-overriding-header "urgent tasks")))
              (tags-todo "@school/todo|in-progress"
                        ((org-agenda-overriding-header "school tasks")))
              (tags-todo "@project/todo|in-progress"
                        ((org-agenda-overriding-header "project tasks")))
              (tags-todo "@learning/todo|in-progress"
                        ((org-agenda-overriding-header "learning tasks")))
              (tags "paper+todo"
                    ((org-agenda-overriding-header "papers to read")))))
            ("s" "school overview"
             ((tags-todo "@school/in-progress"
                        ((org-agenda-overriding-header "current assignments")))
              (tags-todo "@school+deadline<=\"<+7d>\""
                        ((org-agenda-overriding-header "due this week")))
              (tags-todo "@school/todo"
                        ((org-agenda-overriding-header "all school tasks")))))
            ("p" "projects overview"
             ((tags-todo "@project/in-progress"
                        ((org-agenda-overriding-header "active tasks")))
              (tags-todo "@project/waiting"
                        ((org-agenda-overriding-header "blocked/waiting")))
              (tags-todo "@project/todo"
                        ((org-agenda-overriding-header "backlog")))))
            ("l" "learning overview"
             ((tags-todo "@learning/in-progress"
                        ((org-agenda-overriding-header "currently learning")))
              (tags-todo "@learning/todo"
                        ((org-agenda-overriding-header "to learn")))))
            ("r" "research & reading"
             ((tags "paper+unread"
                   ((org-agenda-overriding-header "papers to read")))
              (tags "paper+reading"
                   ((org-agenda-overriding-header "currently reading")))
              (tags "paper+review"
                   ((org-agenda-overriding-header "to review")))))
            ("w" "weekly review"
             ((agenda "" ((org-agenda-span 'week)))
              (todo "DONE"
                   ((org-agenda-overriding-header "completed this week")))
              (stuck ""
                    ((org-agenda-overriding-header "stuck projects"))))))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere nil)

  :config
  (org-roam-db-autosync-mode))

(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/org/ref.bib"))
  (citar-library-paths '("~/org/papers/pdfs/"))
  (citar-notes-paths '("~/org/papers/reviews/"))
  (citar-file-download-dirs '("~/org/papers/pdfs/"))
  (org-cite-global-bibliography '("~/org/ref.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-templates
   '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "#+title: notes on ${author editor}, ${title}"))))

(defun yaz/citar-smart-download ()
  "Download pdf for selected paper from best available source."
  (interactive)
  (let* ((key (citar-select-ref))
         (entry (citar-get-entry key))
         (filename (expand-file-name
                   (concat key ".pdf")
                   "~/org/papers/pdfs/")))

    (if (file-exists-p filename)
        (message "pdf already exists: %s" filename)

      (cond
       ((citar-get-value "eprint" entry)
        (yaz/download-from-arxiv key entry))

       ((citar-get-value "doi" entry)
        (yaz/download-from-doi key entry))

       ((citar-get-value "url" entry)
        (yaz/download-from-url key entry))

       (t (message "no download source found for: %s" key))))))

(defun yaz/download-from-arxiv (key entry)
  "Download pdf from arxiv."
  (let* ((eprint (citar-get-value "eprint" entry))
         (url (concat "https://arxiv.org/pdf/" eprint ".pdf"))
         (filename (expand-file-name
                   (concat key ".pdf")
                   "~/org/papers/pdfs/")))
    (message "downloading from arxiv: %s" eprint)
    (url-copy-file url filename t)
    (message "✓ downloaded: %s" filename)))

(defun yaz/download-from-doi (key entry)
  "Download pdf from doi via sci-hub (use responsibly!)."
  (let* ((doi (citar-get-value "doi" entry))
         (url (concat "https://sci-hub.se/" doi))
         (filename (expand-file-name
                   (concat key ".pdf")
                   "~/org/papers/pdfs/")))
    (message "attempting to download from doi: %s" doi)
    (shell-command (format "curl -l '%s' -o '%s'" url filename))
    (if (file-exists-p filename)
        (message "downloaded: %s" filename)
      (message "download failed. try manual download."))))

(defun yaz/download-from-url (key entry)
  "Download pdf from url."
  (let* ((url (citar-get-value "url" entry))
         (filename (expand-file-name
                   (concat key ".pdf")
                   "~/org/papers/pdfs/")))
    (message "downloading from url: %s" url)
    (url-copy-file url filename t)
    (message "downloaded: %s" filename)))

(defun yaz/download-all-missing-pdfs ()
  "Download pdfs for all entries without files."
  (interactive)
  (let ((entries (citar-get-entries))
        (downloaded 0)
        (failed 0))
    (maphash
     (lambda (key entry)
       (let ((pdf-file (expand-file-name
                       (concat key ".pdf")
                       "~/org/papers/pdfs/")))
         (unless (file-exists-p pdf-file)
           (message "processing: %s" key)
           (condition-case err
               (progn
                 (yaz/citar-smart-download-for-key key entry)
                 (setq downloaded (1+ downloaded))
                 (sit-for 2))
             (error
              (setq failed (1+ failed))
              (message "failed: %s - %s" key err))))))
     entries)
    (message "downloaded: %d | failed: %d" downloaded failed)))

(defun yaz/citar-smart-download-for-key (key entry)
  "Download helper for batch processing."
  (cond
   ((citar-get-value "eprint" entry)
    (yaz/download-from-arxiv key entry))
   ((citar-get-value "doi" entry)
    (yaz/download-from-doi key entry))
   ((citar-get-value "url" entry)
    (yaz/download-from-url key entry))))

(global-set-key (kbd "C-c r d") 'yaz/citar-smart-download)
(global-set-key (kbd "C-c r A") 'yaz/download-all-missing-pdfs)

(use-package citar-org-roam
  :ensure t
  :after (citar org-roam)
  :config
  (diminish 'citar-org-roam-mode)
  (citar-org-roam-mode)
  (setq citar-org-roam-note-title-template "${author} - ${title}"))

(add-hook 'org-mode-hook
          (lambda ()
            (olivetti-mode -1)))

(with-eval-after-load 'org
  (setq org-highlight-latex-and-related '(latex))
  (setq org-startup-with-latex-preview t)
  (setq org-latex-preview-auto-reload t)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.6)))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :init
  (setq org-modern-star 'replace
        org-modern-replace-stars '("#" "##" "###" "####" "#####" "######")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-todo nil
        org-modern-priority nil
        org-modern-tag nil))

(setq-default line-spacing 0.2)

(modify-all-frames-parameters
 '((right-divider-width . 0)
   (internal-border-width . 0)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(provide 'org-config)
;;; org-config.el ends here
