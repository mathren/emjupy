;;; emjupy-figures.el --- Open interactive figures and other outputs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
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
(require 'emjupy-kernel)

;;;; Options

(defcustom emjupy-figure-viewer 'window
  "Where `emjupy-open-output' shows an interactive figure.
`window' is a window of its own, one per figure -- see
`emjupy-figure-window-command'.  `browser' is the browser, which most
often means a tab in one already open.  `xwidget' is inside Emacs, and
`auto' an xwidget when this Emacs has them; neither is the default:
Emacs 30.1 refuses WebKitGTK from 2.41.92, and a build made to take a
newer one aborts as the figure opens -- seen with 2.52 -- so only choose
an xwidget on a build that works with it."
  :type '(choice (const :tag "A window of its own" window)
                 (const :tag "The browser" browser)
                 (const :tag "An xwidget" xwidget)
                 (const :tag "An xwidget if available, else a window" auto))
  :group 'emjupy-figures)

(defcustom emjupy-figure-window-command nil
  "The program that shows a figure in a window of its own, or nil.
A list: the program and its arguments, where \"%s\" stands for the
page's URL.  nil finds one: the WebKitGTK window that comes with emjupy
-- it needs PyGObject and WebKitGTK, which GNOME desktops have -- then
the first of `emjupy-figure-window-browsers' installed, and the
browser if there is none of these."
  :type '(choice (const :tag "Find one" nil)
                 (repeat :tag "Program and arguments" string))
  :group 'emjupy-figures)

(defcustom emjupy-figure-window-browsers
  '(("firefox" "--new-window" "%s")
    ("chromium" "--app=%s"))
  "Browsers that can show a figure in a window of their own, tried in order.
Each is the program and its arguments, where \"%s\" stands for the page's
URL; the first installed is used, when the WebKitGTK window that comes
with emjupy cannot run.  Firefox opens a new window; Chromium an app
window, without tabs or toolbar.  Another browser can be added: Chrome
as (\"google-chrome\" \"--app=%s\"), say."
  :type '(repeat (repeat :tag "Program and arguments" string))
  :group 'emjupy-figures)

(defcustom emjupy-plotly-js 'kernel
  "Where the plotly.js a figure page loads comes from.
`kernel' asks the kernel that made the figure for its own copy, once per
plotly version, and keeps it: it matches the figure, and needs no
network, which also makes it work through a tunnel.  A string is used as
the script's address instead -- a URL, such as a CDN's, or a file name."
  :type '(choice (const :tag "The kernel's own copy" kernel)
                 (string :tag "A URL or file name"))
  :group 'emjupy-figures)

;;;; The figure window

;; Found beside this file: it ships with the package, which MELPA builds
;; into one directory, so it lives in src/ with the Lisp, in a checkout
;; as in the package.
(defconst emjupy--figure-window-script
  (expand-file-name "emjupy-figure-window.py"
                    (file-name-directory (or load-file-name buffer-file-name
                                             default-directory)))
  "The WebKitGTK figure window that comes with emjupy.")

(defvar emjupy--figure-window-bundled-command 'unknown
  "The command running the bundled figure window, nil if it cannot, or `unknown'.
Kept: finding out starts Python.")

(defun emjupy--figure-window-bundled ()
  "Return the command running the bundled figure window, or nil if it cannot.
Asks Python, once, for PyGObject and WebKitGTK, trying the system's too:
a virtualenv or conda Python first on the PATH usually lacks them."
  (when (file-exists-p emjupy--figure-window-script)
    (cl-loop for python in (delete-dups
                            (delq nil (list (executable-find "python3")
                                            (and (file-executable-p "/usr/bin/python3")
                                                 "/usr/bin/python3"))))
             when (zerop (call-process
                          python nil nil nil "-c"
                          (concat "import gi; gi.require_version('Gtk', '3.0')\n"
                                  "for v in ('4.1', '4.0'):\n"
                                  "    try: gi.require_version('WebKit2', v); break\n"
                                  "    except ValueError: pass\n"
                                  "from gi.repository import Gtk, WebKit2")))
             return (list python emjupy--figure-window-script "%s"))))

(defun emjupy--figure-window-command ()
  "Return the command showing a figure in a window of its own, or nil.
See `emjupy-figure-window-command'.  The browsers are looked for each
time, so a change to `emjupy-figure-window-browsers' counts at once."
  (or emjupy-figure-window-command
      (if (eq emjupy--figure-window-bundled-command 'unknown)
          (setq emjupy--figure-window-bundled-command
                (emjupy--figure-window-bundled))
        emjupy--figure-window-bundled-command)
      (cl-loop for (program . args) in emjupy-figure-window-browsers
               for path = (executable-find program)
               when path return (cons path args))))

;;;; Showing a page

(defun emjupy--figures-dir ()
  "Return the directory figure pages are written to, the user's own.
Made for the user alone, and checked: in a shared /tmp another user
could make it first, to read the pages, or plant a script they load."
  (let ((dir (expand-file-name (format "emjupy-figures-%s/" (user-login-name))
                               temporary-file-directory)))
    (emjupy--private-directory dir "figure pages")
    dir))

(defun emjupy--xwidget-available-p ()
  "Return non-nil if this Emacs can show an xwidget.
`xwidget-webkit-browse-url' is `fboundp' on a build with no xwidget
support at all -- an autoload in a file that always ships -- so the
feature is what is tested.  And an xwidget needs a graphical frame."
  (and (featurep 'xwidget-internal) (display-graphic-p)))

(defun emjupy--figure-viewer ()
  "Return the viewer to use: `window', `browser' or `xwidget'."
  (pcase emjupy-figure-viewer
    ('browser 'browser)
    ('xwidget (if (emjupy--xwidget-available-p) 'xwidget
                (user-error "This Emacs has no xwidget support")))
    ('auto (if (emjupy--xwidget-available-p) 'xwidget 'window))
    (_ 'window)))

(defun emjupy--show-file (file)
  "Show the page FILE in the figure viewer."
  (let ((url (concat "file://" file)))
    (pcase (emjupy--figure-viewer)
      ('xwidget (require 'xwidget)
                (xwidget-webkit-browse-url url t))
      ('window
       (let ((command (emjupy--figure-window-command)))
         (if (not command)
             (progn (message "[emjupy] No program for a figure window; %s"
                             "opening it in the browser")
                    (browse-url url))
           (apply #'start-process "emjupy-figure" nil (car command)
                  (mapcar (lambda (arg) (string-replace "%s" url arg))
                          (cdr command))))))
      (_ (browse-url url)))))

(defun emjupy--write-page (html)
  "Write the page HTML to the figures directory and return its file name.
Named by its content: opening the same figure again opens the same
page.  A file rather than a `data:' URL, since a page can run to
megabytes."
  (let ((file (expand-file-name (concat (md5 html) ".html")
                                (emjupy--figures-dir))))
    (unless (file-exists-p file)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region html nil file nil 'quiet)))
    file))

;;;; Plotly

(defun emjupy--plotly-title (figure)
  "Return the title of the plotly FIGURE, for its window, made safe for HTML."
  (let* ((layout (and (hash-table-p figure) (gethash "layout" figure)))
         (title (and (hash-table-p layout) (gethash "title" layout)))
         (text (cond ((stringp title) title)
                     ((hash-table-p title) (gethash "text" title)))))
    (if (and (stringp text) (not (string-empty-p text)))
        (replace-regexp-in-string "[<>&\"]" (lambda (c)
                                              (format "&#%d;"
                                                      (string-to-char c))) text)
      "plotly figure")))

(defun emjupy--plotly-page (figure script)
  "Return a page drawing the plotly FIGURE, a parsed JSON object.
SCRIPT is the address plotly.js is loaded from."
  (format "<!DOCTYPE html>
<html><head><meta charset=\"utf-8\"><title>%s</title>
<style>html,body{margin:0;height:100%%}#figure{width:100%%;height:100vh}</style>
<script src=\"%s\"></script></head>
<body><div id=\"figure\"></div>
<script>
var spec = %s;
Plotly.newPlot('figure', spec.data || [], spec.layout || {},
               Object.assign({responsive: true}, spec.config || {}));
</script></body></html>
"
          (emjupy--plotly-title figure) script
          (decode-coding-string (json-serialize figure) 'utf-8)))

(defconst emjupy--plotly-js-chunk 900000
  "Characters of plotly.js first asked of the kernel at a time.
Within what Jupyter's default limit lets one request print.")

(defconst emjupy--plotly-js-min-chunk 25000
  "Fewest characters of plotly.js asked at a time before giving up.")

(defun emjupy--plotly-js (callback)
  "Call CALLBACK with the address of plotly.js, once it is known.
See `emjupy-plotly-js'.  The kernel holds it, and it is fetched in
pieces: the server cuts a request's output past its rate limit, putting
a notice in its place.  A piece cut short is asked again in halves, for
a server set lower than Jupyter's default.  Once per plotly version."
  (if (stringp emjupy-plotly-js)
      (funcall callback (if (string-match-p "\\`[a-z]+://" emjupy-plotly-js)
                            emjupy-plotly-js
                          (concat "file://"
                                  (expand-file-name emjupy-plotly-js))))
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
             (emjupy--plotly-js-fetch
              (emjupy--plotly-fetch-make
               :kernel kernel :file file :size size :callback callback
               :chunk emjupy--plotly-js-chunk)))))))))

(cl-defstruct (emjupy--plotly-fetch (:constructor emjupy--plotly-fetch-make)
                                    (:copier nil))
  "A fetch of plotly.js from a kernel, in pieces.
KERNEL holds it; it is written to FILE, SIZE characters, and CALLBACK
called with the file's address.  FROM is how much has come so far,
PIECES the pieces, newest first, and CHUNK how much is asked at a time."
  kernel file size callback (from 0) (pieces nil) chunk)

(defun emjupy--plotly-js-fetch (fetch)
  "Ask the kernel of FETCH, an `emjupy--plotly-fetch', for the next piece.
A piece of the wrong length was cut by the server's output limit, and is
asked again at half the size.  When all has come, the file is written and
the callback called with its address."
  (let ((kernel (emjupy--plotly-fetch-kernel fetch))
        (from (emjupy--plotly-fetch-from fetch))
        (size (emjupy--plotly-fetch-size fetch))
        (chunk (emjupy--plotly-fetch-chunk fetch)))
    (if (>= from size)
        (let ((coding-system-for-write 'utf-8-unix)
              (file (emjupy--plotly-fetch-file fetch)))
          (write-region (apply #'concat
                               (reverse (emjupy--plotly-fetch-pieces fetch)))
                        nil file nil 'quiet)
          (emjupy--kernel-eval kernel "del _emjupy_js" #'ignore)
          (funcall (emjupy--plotly-fetch-callback fetch)
                   (concat "file://" file)))
      (message "[emjupy] Fetching plotly.js from the kernel... %d%%"
               (/ (* 100 from) size))
      (let ((wanted (min chunk (- size from))))
        (emjupy--kernel-eval
         kernel (format "print(_emjupy_js[%d:%d], end='')" from (+ from wanted))
         (lambda (piece)
           (cond
            ((= (length piece) wanted)
             (setf (emjupy--plotly-fetch-from fetch) (+ from wanted))
             (push piece (emjupy--plotly-fetch-pieces fetch))
             ;; spaced out, as a server may count output over a few seconds
             (run-at-time 1.0 nil #'emjupy--plotly-js-fetch fetch))
            ((>= (/ chunk 2) emjupy--plotly-js-min-chunk)
             (setf (emjupy--plotly-fetch-chunk fetch) (/ chunk 2))
             (run-at-time 1.0 nil #'emjupy--plotly-js-fetch fetch))
            (t
             (emjupy--kernel-eval kernel "del _emjupy_js" #'ignore)
             (message "[emjupy] plotly.js could not be fetched: the server's output limit is too low.  Set `emjupy-plotly-js' to its address or file")))))))))

;;;; Images and other outputs

(defun emjupy--show-image (payload type)
  "Show the image PAYLOAD of TYPE in a buffer of its own."
  (let ((buf (get-buffer-create "*emjupy image*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image (emjupy--render-image-output payload type))
        (goto-char (point-min)))
      (special-mode))
    ;; In a frame of its own, the same one each time: a figure window.
    (display-buffer buf
                    '((display-buffer-reuse-window display-buffer-pop-up-frame)
                          (reusable-frames . t)
                          (pop-up-frame-parameters (name . "emjupy figure")
                                                   (width . 90)
                                                   (height . 40))))))

(defun emjupy--openable-data (cell)
  "Return the first output bundle of CELL that can be opened, or nil."
  (cl-loop for o across (or (emjupy-cell-outputs cell) [])
           for data = (and (hash-table-p o) (gethash "data" o))
           when (and data (or (gethash emjupy--plotly-mime data)
                              (emjupy--scripted-html data)
                              (cl-loop for (mime . _) in
                                       emjupy--image-mime-types
                                       thereis (gethash mime data))))
           return data))

;;;###autoload

;;;; Opening an output

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
           (emjupy--show-file (emjupy--write-page
                               (emjupy--plotly-page plotly script)))
           (message "[emjupy] Opened the plotly figure."))))
       (html (emjupy--show-file (emjupy--write-page html))
             (message "[emjupy] Opened the HTML output."))
       (image (emjupy--show-image (nth 1 image) (nth 0 image)))
       (t (user-error "This output has nothing to open"))))))

(defun emjupy-open-output-at-point (&optional event)
  "Open the interactive output whose line is at point, or was clicked.
EVENT is the mouse event, when there was one."
  (interactive (list last-nonmenu-event))
  (let ((pos (if (mouse-event-p event) (posn-point (event-start event))
               (point))))
    (emjupy-open-output (get-text-property pos 'emjupy-output-data))))

(provide 'emjupy-figures)

;;; emjupy-figures.el ends here
