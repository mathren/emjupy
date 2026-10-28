;;; publish.el --- Build the documentation site  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Builds docs/ into html-content/ for the project site.  Modified from
;; https://systemcrafters.net/publishing-websites-with-org-mode/building-the-site/

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system and install dependencies
(package-initialize)
;; htmlize only colours the source blocks: without it they export as plain
;; text and everything else is identical.  Not worth failing the build for,
;; so a refresh that cannot reach an archive is survived rather than fatal.
(unless (package-installed-p 'htmlize)
  (ignore-errors
    (unless package-archive-contents (package-refresh-contents))
    (package-install 'htmlize)))
(unless (require 'htmlize nil t)
  (setq org-html-htmlize-output-type nil)
  (message "htmlize unavailable: source blocks export as plain text"))

(require 'cl-lib)
(require 'ox-publish)

(defun mr/org-html-src-block-with-meta (orig-fun src-block contents info)
  "Wrap org src-block HTML with a language badge and copy-to-clipboard button."
  (let* ((lang  (org-element-property :language src-block))
         (inner (funcall orig-fun src-block contents info)))
    (format
     (concat "<div class=\"src-block-wrapper\">"
             "<div class=\"src-block-header\">"
             "<span class=\"src-lang\">%s</span>"
             "<button class=\"copy-btn\""
             " onclick=\"mrCopyCode(this)\""
             " aria-label=\"Copy code\">Copy</button>"
             "</div>"
             "%s"
             "</div>")
     (or lang "")
     inner)))

(advice-add 'org-html-src-block :around #'mr/org-html-src-block-with-meta)

(defun mr/get-modified-org-files ()
  "Return a list of Org files modified since the last commit."
  (let* ((default-directory (expand-file-name "./docs"))
         (modified-files (split-string (shell-command-to-string "git diff --name-only HEAD -- *.org") "\n" t)))
    (mapcar (lambda (file) (expand-file-name file default-directory))
            modified-files)))

(defun mr/check-for-all-flag-and-publish ()
  "Check if all is in the command-line arguments."
  (if (member "all" command-line-args)
      (org-publish-all t) ;; publish all
    (let ((modified-files (mr/get-modified-org-files)))
      (if modified-files
          (org-publish-project "org-site:main")
        (message "No modified Org files found.")))))


(defun mr/read-file (file)
  "Read contents of FILE to string"
  (with-temp-buffer (insert-file-contents file) (buffer-string)))
;; Dynamically make footer with pointers to previous and next page
(defun mr/get-html-link (org-file publishing-directory)
  "Convert ORG-FILE to the corresponding HTML file in PUBLISHING-DIRECTORY."
  (when org-file
    (let* ((file-name (file-name-sans-extension org-file))
           (html-file (concat file-name ".html")))
      html-file)))

(defun mr/read-navigation-keywords ()
  "Read the keywords #+PREVIOUS_PAGE and #+NEXT_PAGE from the current Org file.
Return them as two values: previous-page and next-page."
  (let* ((keywords (org-element-map (org-element-parse-buffer) 'keyword
                                    (lambda (el)
                                      (cons (org-element-property :key el)
                                            (org-element-property :value el)))))
         (previous (cdr (assoc "PREVIOUS_PAGE" keywords)))
         (next (cdr (assoc "NEXT_PAGE" keywords))))
    (list previous next)))

(defconst mr/site-html-head
  (mr/read-file "html-content/html-templates/html_head.html"))

(defconst mr/site-html-preamble
  (mr/read-file "html-content/html-templates/preamble.html"))

(defun mr/site-html-postamble (info)
  "Generate a dynamic HTML footer for the Org export.
Substitute placeholders PREVIOUS_PAGE and NEXT_PAGE with corresponding links.
INFO is the export plist."
  (let* ((publishing-directory (plist-get info :publishing-directory))
         (nav (mr/read-navigation-keywords))
         (previous-page (mr/get-html-link (car nav) publishing-directory))
         (next-page (mr/get-html-link (cadr nav) publishing-directory))
         (footer-template
          (mr/read-file "../html-content/html-templates/postamble.html"))
         (emjupy-version
          (with-temp-buffer
            (insert-file-contents "../src/emjupy-pkg.el")
            (nth 2 (read (current-buffer)))))
         (footer
          (format footer-template
                  emjupy-version
                  (format-time-string "%-d %B %Y")
                  emacs-version
                  org-version)))
    (setq footer
          (replace-regexp-in-string
           "PREVIOUS_PAGE" (or previous-page "#") footer t t))
    (setq footer
          (replace-regexp-in-string
           "NEXT_PAGE" (or next-page "#") footer t t))))

;; fix timestamps for html and latex exports
(defun mr/filter-timestamp (trans back _comm)
  "Remove <> around time-stamps."
  (pcase back
    (`html
     (replace-regexp-in-string "&[lg]t;" "" trans))
    (`latex
     (replace-regexp-in-string "[<>]" "" trans))))

;; Custom drawer export for click-to-reveal
(defvar mr/drawer-counter 0
  "Counter for deterministic drawer IDs.")

(add-hook 'org-export-before-processing-hook
          (lambda (_)
            (setq mr/id-counter 0)
            (setq mr/drawer-counter 0)))

(defun mr/format-drawer-html (drawer contents info)
  "Export DRAWER as a collapsible HTML element."
  (let* ((drawer-name (org-element-property :drawer-name drawer))
         (drawer-id (format "drawer-%s-%d"
                            (replace-regexp-in-string "[^a-zA-Z0-9]" "-"
                                                      drawer-name)
                            (cl-incf mr/drawer-counter))))
    (format "<div class=\"org-drawer-container\">
  <button class=\"org-drawer-toggle\" onclick=\"toggleDrawer('%s')\">%s ▼</button>
  <div id=\"%s\" class=\"org-drawer-content\" style=\"display: none;\">%s</div>
</div>"
            drawer-id
            drawer-name
            drawer-id
            (or contents ""))))

(advice-add 'org-html-drawer :override #'mr/format-drawer-html)

;; ---------------------------------------------------------------------------
;; Deterministic HTML IDs (override in-memory, never touch *.org files)
;; ---------------------------------------------------------------------------

(defun mr/slugify (text)
  "Convert TEXT to a URL-friendly slug."
  (downcase
   (replace-regexp-in-string
    "-+$" ""
    (replace-regexp-in-string
     "^-+" ""
     (replace-regexp-in-string
      "[^a-z0-9]+" "-" text)))))

(defvar mr/id-counter 0
  "Counter for deterministic IDs of what is not a headline: figures,
LaTeX blocks and the like.")

(defun mr/slug-every-headline (_backend)
  "Give every headline without a CUSTOM_ID one: the slug of its title.
Runs in the export's copy of the buffer, before parsing, so links to a
headline -- by title, from this page or another -- resolve to the anchor
the headline gets.  The .org files are not touched."
  (org-map-entries
   (lambda ()
     (unless (org-entry-get nil "CUSTOM_ID")
       (org-entry-put nil "CUSTOM_ID"
                      (mr/slugify (org-get-heading t t t t)))))))

(add-hook 'org-export-before-parsing-functions #'mr/slug-every-headline)

;; Links to a headline use its CUSTOM_ID, now its slug, not a random id.
(setq org-html-prefer-user-labels t)

(defun mr/resolve-heading-link (orig search file &rest rest)
  "Resolve a link to another page's headline, SEARCH \"*Title\", to its slug.
Every headline's anchor is the slug of its title, so no cache of the
other page's ids is needed.  Other searches go to ORIG, with FILE and
REST."
  (if (string-prefix-p "*" search)
      (mr/slugify (substring search 1))
    (apply orig search file rest)))

(advice-add 'org-publish-resolve-external-link :around #'mr/resolve-heading-link)

(defun mr/fix-remaining-ids (output backend _info)
  "Replace the random org IDs left in OUTPUT, for BACKEND html, by counted ones.
Headlines have their slugs; what remains -- figures, LaTeX blocks -- is
numbered, so the page is the same each time it is built."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "org[a-f0-9]\\{7\\}"
     (lambda (_match)
       (cl-incf mr/id-counter)
       (format "org-element-%d" mr/id-counter))
     output)))

(add-to-list 'org-export-filter-final-output-functions #'mr/fix-remaining-ids)
(add-to-list 'org-export-filter-timestamp-functions #'mr/filter-timestamp)


;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      user-full-name "Mathieu Renzo"          ;; for creator
      org-html-head mr/site-html-head
      org-html-preamble mr/site-html-preamble
      org-html-postamble #'mr/site-html-postamble
      org-display-custom-times t)

;; Define the publishing project
(setq org-publish-project-alist
      (list
       (list "org-site:main"
             :base-directory "./docs/"
             :publishing-directory "./html-content"
             :recursive nil
             :exclude "README.org\\|LICENSE\\|\\.gitignore\\|\\*.tar\\|\\Makefile\\|\\.dir-locals.el"
             :publishing-function 'org-html-publish-to-html
             :html-doctype "html5"
             :language "en"
             :html-html5-fancy t
             :email "mrenzo@arizona.edu"
             :meta-type "website"
             :description: "Emjupy"
             :with-title nil
             :with-latex t
             :with-sub-superscript t
             :html-head-include-default-style nil
             :with-tags t
             :with-tasks t
             :html-self-link-headlines t
             :with-timestamps t           ; Include time stamp in file
             :with-toc nil                ; Don't include a table of contents
             :section-numbers nil         ; Don't include section numbers
             ;; Use the list of modified files only
             :file-list (mr/get-modified-org-files))))

(mr/check-for-all-flag-and-publish)
(message "Build complete!")

;; Local Variables:
;; no-byte-compile: t
;; End:
