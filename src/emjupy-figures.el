;;; emjupy-figures.el --- Open interactive figures and other outputs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@flatironinstitute.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A figure that draws itself in JavaScript -- plotly, bokeh -- has nothing
;; to show in a text buffer.  The notebook shows a line for it, and
;; `emjupy-open-output' opens it: in an xwidget where this Emacs has one,
;; and in the browser otherwise.  An image output opens in an image buffer
;; of its own, for a figure too small to read at cell width.
;;
;; A plotly figure arrives as its JSON specification, not as HTML: the page
;; is built here, with plotly.js asked of the kernel that made the figure,
;; so it matches the figure's version and needs no network.

;;; Code:

(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-kernel)

(defcustom emjupy-figure-viewer 'auto
  "Where `emjupy-open-output' shows an interactive figure.
`auto' uses an xwidget when this Emacs has one, and the browser
otherwise.  `xwidget' and `browser' insist on one of them."
  :type '(choice (const :tag "xwidget if available, else browser" auto)
                 (const :tag "An xwidget" xwidget)
                 (const :tag "The browser" browser))
  :group 'emjupy)

(defcustom emjupy-plotly-js 'kernel
  "Where the plotly.js a figure page loads comes from.
`kernel' asks the kernel that made the figure for its own copy, once per
plotly version, and keeps it: it matches the figure, and needs no
network, which also makes it work through a tunnel.  A string is used as
the script's address instead -- a URL, such as a CDN's, or a file name."
  :type '(choice (const :tag "The kernel's own copy" kernel)
                 (string :tag "A URL or file name"))
  :group 'emjupy)

(defun emjupy--figures-dir ()
  "Return the directory figure pages are written to, creating it."
  (let ((dir (expand-file-name "emjupy-figures/" temporary-file-directory)))
    (make-directory dir t)
    dir))

(defun emjupy--xwidget-available-p ()
  "Return non-nil if this Emacs can show an xwidget.
`xwidget-webkit-browse-url' is `fboundp' on a build with no xwidget
support at all -- an autoload in a file that always ships -- so the
feature is what is tested.  And an xwidget needs a graphical frame."
  (and (featurep 'xwidget-internal) (display-graphic-p)))

(defun emjupy--figure-viewer ()
  "Return the viewer to use, `xwidget' or `browser'."
  (pcase emjupy-figure-viewer
    ('browser 'browser)
    ('xwidget (if (emjupy--xwidget-available-p) 'xwidget
                (user-error "This Emacs has no xwidget support")))
    (_ (if (emjupy--xwidget-available-p) 'xwidget 'browser))))

(defun emjupy--show-file (file)
  "Show the page FILE in the figure viewer."
  (let ((url (concat "file://" file)))
    (pcase (emjupy--figure-viewer)
      ('xwidget (require 'xwidget)
                (xwidget-webkit-browse-url url t))
      (_ (browse-url url)))))

(defun emjupy--write-page (html)
  "Write the page HTML to the figures directory and return its file name.
Named by its content: opening the same figure again opens the same
page.  A file rather than a `data:' URL, since a page can run to
megabytes."
  (let ((file (expand-file-name (concat (md5 html) ".html") (emjupy--figures-dir))))
    (unless (file-exists-p file)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region html nil file nil 'quiet)))
    file))

(defun emjupy--plotly-page (figure script)
  "Return a page drawing the plotly FIGURE, a parsed JSON object.
SCRIPT is the address plotly.js is loaded from."
  (format "<!DOCTYPE html>
<html><head><meta charset=\"utf-8\"><title>plotly figure</title>
<style>html,body{margin:0;height:100%%}#figure{width:100%%;height:100vh}</style>
<script src=\"%s\"></script></head>
<body><div id=\"figure\"></div>
<script>
var spec = %s;
Plotly.newPlot('figure', spec.data || [], spec.layout || {},
               Object.assign({responsive: true}, spec.config || {}));
</script></body></html>
"
          script (decode-coding-string (json-serialize figure) 'utf-8)))

(defconst emjupy--plotly-js-chunk 900000
  "Characters of plotly.js asked of the kernel at a time.")

(defun emjupy--plotly-js (callback)
  "Call CALLBACK with the address of plotly.js, once it is known.
See `emjupy-plotly-js'.  Asking the kernel is asynchronous.

The kernel cannot simply print it: the Jupyter server limits how fast
output reaches its clients, 1 MB a second by default, and replaces what
goes faster with a notice -- which is what would be saved as plotly.js.
So the kernel holds it, and it is fetched in pieces a second apart: some
five seconds, once per plotly version, after which the copy kept is
used."
  (if (stringp emjupy-plotly-js)
      (funcall callback (if (string-match-p "\\`[a-z]+://" emjupy-plotly-js)
                            emjupy-plotly-js
                          (concat "file://" (expand-file-name emjupy-plotly-js))))
    (let ((kernel (and emjupy--buffer-notebook
                       (emjupy-notebook-kernel emjupy--buffer-notebook))))
      (unless (and kernel (emjupy--ws-live-p kernel))
        (user-error "Opening a plotly figure asks its kernel for plotly.js, %s"
                    "and this notebook is not connected; or set `emjupy-plotly-js'"))
      (emjupy--kernel-eval
       kernel (concat "import plotly, plotly.offline as _emjupy_po; "
                      "_emjupy_js = _emjupy_po.get_plotlyjs(); "
                      "print(plotly.__version__, len(_emjupy_js))")
       (lambda (answer)
         (let* ((words (split-string answer))
                (version (car words))
                (size (string-to-number (or (cadr words) "0")))
                (file (expand-file-name (format "plotly-%s.min.js" version)
                                        (emjupy--figures-dir))))
           (if (file-exists-p file)
               (progn (emjupy--kernel-eval kernel "del _emjupy_js" #'ignore)
                      (funcall callback (concat "file://" file)))
             (emjupy--plotly-js-fetch kernel file size 0 nil callback))))))))

(defun emjupy--plotly-js-fetch (kernel file size from pieces callback)
  "Fetch plotly.js from KERNEL into FILE, SIZE characters, starting at FROM.
PIECES holds what has come, newest first.  When all of it has, the file
is written and CALLBACK called with its address."
  (if (>= from size)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (apply #'concat (reverse pieces)) nil file nil 'quiet)
        (emjupy--kernel-eval kernel "del _emjupy_js" #'ignore)
        (funcall callback (concat "file://" file)))
    (message "[emjupy] Fetching plotly.js from the kernel... %d%%" (/ (* 100 from) size))
    (emjupy--kernel-eval
     kernel (format "print(_emjupy_js[%d:%d], end='')"
                    from (+ from emjupy--plotly-js-chunk))
     (lambda (piece)
       ;; a second apart, to stay under the server's output limit
       (run-at-time 1.0 nil #'emjupy--plotly-js-fetch kernel file size
                    (+ from emjupy--plotly-js-chunk) (cons piece pieces) callback)))))

(defun emjupy--show-image (payload type)
  "Show the image PAYLOAD of TYPE in a buffer of its own."
  (let ((buf (get-buffer-create "*emjupy image*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image (emjupy--render-image-output payload type))
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf)))

(defun emjupy--openable-data (cell)
  "Return the first output bundle of CELL that can be opened, or nil."
  (cl-loop for o across (or (emjupy-cell-outputs cell) [])
           for data = (and (hash-table-p o) (gethash "data" o))
           when (and data (or (gethash emjupy--plotly-mime data)
                              (emjupy--scripted-html data)
                              (cl-loop for (mime . _) in emjupy--image-mime-types
                                       thereis (gethash mime data))))
           return data))

;;;###autoload
(defun emjupy-open-output (&optional data)
  "Open an output in a window of its own.
DATA is its MIME bundle; interactively, the output whose line is at
point, or else the first output of the cell at point that can be opened.

A plotly figure opens in an xwidget where this Emacs has one, and in the
browser otherwise -- see `emjupy-figure-viewer' -- as does other HTML
that runs JavaScript.  An image opens in an image buffer.  Opening is
always asked for, never automatic: it runs the notebook's JavaScript,
and a notebook is a file people share."
  (interactive)
  (let ((data (or data
                  (get-text-property (point) 'emjupy-output-data)
                  (let ((cell (emjupy--cell-at-point-including-output)))
                    (and cell (emjupy--openable-data cell))))))
    (unless data (user-error "No output here that can be opened"))
    (let ((plotly (gethash emjupy--plotly-mime data))
          (html (emjupy--scripted-html data))
          (image (cl-loop for (mime . type) in emjupy--image-mime-types
                          for p = (gethash mime data)
                          when p return (list type p))))
      (cond
       (plotly
        (emjupy--plotly-js
         (lambda (script)
           (emjupy--show-file (emjupy--write-page (emjupy--plotly-page plotly script)))
           (message "[emjupy] Opened the plotly figure."))))
       (html (emjupy--show-file (emjupy--write-page html))
             (message "[emjupy] Opened the HTML output."))
       (image (emjupy--show-image (nth 1 image) (nth 0 image)))
       (t (user-error "This output has nothing to open"))))))

(provide 'emjupy-figures)

;;; emjupy-figures.el ends here
