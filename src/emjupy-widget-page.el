;;; emjupy-widget-page.el --- Widgets that draw themselves, in a page  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@flatironinstitute.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some widgets draw themselves in JavaScript -- plotly's FigureWidget, a
;; map, a 3D viewer -- so no text stands for them.  Such a widget opens in
;; a page of its own, in the figure window, where the ipywidgets widget
;; manager runs and draws it, live.
;;
;; The page talks to Emacs, not to the Jupyter server: a WebSocket server
;; in Emacs, the bridge, relays the widget's comm messages over the
;; kernel connection emjupy already has.  So the page needs no token, is
;; not refused for its origin, and works through a tunnel as the notebook
;; does.  It starts from the models emjupy already holds.
;;
;; The widget's own JavaScript -- its module -- is asked of the kernel, as
;; plotly.js is: widget packages put a copy for the classic notebook in
;; their environment, which matches the kernel's version and needs no
;; network.  A module the kernel has no copy of is fetched from a CDN by
;; the widget manager, as it would be in any page.

;;; Code:

(require 'emjupy-core)
(require 'emjupy-kernel)
(require 'emjupy-figures)
(require 'emjupy-widgets)
(require 'websocket)

(defcustom emjupy-widget-scripts
  '("https://cdn.jsdelivr.net/npm/requirejs@2.3.7/require.js"
    "https://cdn.jsdelivr.net/npm/@jupyter-widgets/html-manager@1.0.15/dist/embed-amd.js")
  "The scripts a widget\\='s page loads: RequireJS, then the widget manager.
Each a URL or a file name; a file needs no network."
  :type '(repeat string)
  :group 'emjupy)

(defconst emjupy--standard-widget-modules
  '("@jupyter-widgets/controls" "@jupyter-widgets/base" "@jupyter-widgets/output" "")
  "The modules of ipywidgets\\=' own widgets, drawn in the notebook.")

(defvar emjupy--bridge nil
  "The bridge\\='s server process, or nil before the first page.")

(defvar emjupy--bridge-pages (make-hash-table :test 'equal)
  "Open pages, by their token: a plist of :kernel, :cell, :view, :models, :ws.")

;;;; Which widgets go to a page

(defun emjupy--widget-scripted-p (s)
  "Return non-nil if S, a widget\\='s state, is drawn by JavaScript of its own.
Its view comes from a module other than ipywidgets\\=' own."
  (let ((module (gethash "_view_module" s)))
    (and (stringp module) (not (member module emjupy--standard-widget-modules)))))

(defun emjupy--widget-scripted-name (s)
  "Return the name of S, a widget drawing itself, for the line opening it.
An anywidget's model is anywidget's own, the same for all of them, so
its Python class -- which anywidget records -- says more."
  (let ((class (gethash "_anywidget_id" s)))
    (if (and (stringp class) (not (string-empty-p class)))
        (car (last (split-string class "\\.")))
      (string-remove-suffix "Model" (or (gethash "_model_name" s) "widget")))))

(defun emjupy--widget-draw-scripted (id s)
  "Draw S as a line opening it in a page.  ID is the widget's comm id."
  (list (emjupy--widget-button
         (format "▶ Interactive widget: %s (%s) -- RET or click to open it"
                 (emjupy--widget-scripted-name s) (gethash "_view_module" s))
         id 'open-page)))

;;;; The models a page needs

(defun emjupy--widget-references (value)
  "Return the comm ids VALUE refers to, as \"IPY_MODEL_<id>\" strings in it."
  (cond ((and (stringp value) (string-prefix-p "IPY_MODEL_" value))
         (list (emjupy--widget-ref value)))
        ((vectorp value) (cl-mapcan #'emjupy--widget-references (append value nil)))
        ((hash-table-p value)
         (let (refs) (maphash (lambda (_k v) (setq refs (append (emjupy--widget-references v) refs)))
                              value)
              refs))))

(defun emjupy--widget-tree (id)
  "Return ID and the comm ids of every model it refers to, however deep."
  (let ((seen nil) (todo (list id)))
    (while todo
      (let ((next (pop todo)))
        (unless (member next seen)
          (push next seen)
          (let ((state (emjupy--widget-state next)))
            (when state (setq todo (append (emjupy--widget-references state) todo)))))))
    (nreverse seen)))

(defun emjupy--widget-serialize (value path paths buffers)
  "Return VALUE with its bytes taken out, for JSON.
PATH is where VALUE is in the state; each bytes found is replaced by
null and its path and base64 pushed onto PATHS and BUFFERS, cons cells
holding the lists."
  (cond ((emjupy-bytes-p value)
         (push (vconcat (reverse path)) (car paths))
         (push (base64-encode-string (emjupy-bytes-data value) t) (car buffers))
         :null)
        ((vectorp value)
         (vconcat (cl-loop for v across value for i from 0
                           collect (emjupy--widget-serialize v (cons i path) paths buffers))))
        ((hash-table-p value)
         (let ((copy (make-hash-table :test 'equal)))
           (maphash (lambda (k v) (puthash k (emjupy--widget-serialize v (cons k path) paths buffers)
                                           copy))
                    value)
           copy))
        (t value)))

(defun emjupy--widget-model-json (id)
  "Return the model with comm ID as a page receives it, a hash table."
  (let* ((state (emjupy--widget-state id))
         (paths (list nil)) (buffers (list nil))
         (clean (emjupy--widget-serialize state nil paths buffers))
         (m (make-hash-table :test 'equal)))
    (puthash "id" id m)
    (dolist (k '("_model_name" "_model_module" "_model_module_version"))
      (puthash (string-remove-prefix "_" k) (gethash k state) m))
    (puthash "state" clean m)
    (puthash "buffer_paths" (vconcat (nreverse (car paths))) m)
    (puthash "buffers" (vconcat (nreverse (car buffers))) m)
    m))

;;;; The modules a page needs

(defun emjupy--widget-modules-dir ()
  "Return the directory widget modules from the kernel are kept in."
  (let ((dir (expand-file-name "modules/" (emjupy--figures-dir))))
    (make-directory dir t)
    dir))

(defun emjupy--widget-module-file (module version)
  "Return the file MODULE at VERSION is kept in, from the kernel."
  (expand-file-name (format "%s@%s.js" (replace-regexp-in-string "[^A-Za-z0-9_.-]" "_" module)
                            (replace-regexp-in-string "[^A-Za-z0-9_.-]" "_" (or version "")))
                    (emjupy--widget-modules-dir)))

(defun emjupy--widget-fetch-modules (kernel modules callback)
  "Fetch from KERNEL the MODULES, (NAME . VERSION) pairs, then call CALLBACK.
CALLBACK is called with an alist of each module found to its file.
Each is the copy for the classic notebook the widget package put in the
kernel\\='s environment; one it has none of is left out, and the page\\='s
widget manager fetches it from a CDN instead."
  (if (null modules)
      (funcall callback nil)
    (let* ((module (car modules))
           (file (emjupy--widget-module-file (car module) (cdr module))))
      (if (file-exists-p file)
          (emjupy--widget-fetch-modules
           kernel (cdr modules)
           (lambda (found) (funcall callback (cons (cons (car module) file) found))))
        (emjupy--kernel-eval
         kernel
         (format "import os as _o, sys as _s
_p = _o.path.join(_s.prefix, 'share', 'jupyter', 'nbextensions', %S, 'index.js')
print(open(_p).read() if _o.path.exists(_p) else '', end='')" (car module))
         (lambda (js)
           (when (and js (not (string-empty-p js)))
             (let ((coding-system-for-write 'utf-8-unix))
               (write-region js nil file nil 'quiet)))
           (emjupy--widget-fetch-modules
            kernel (cdr modules)
            (lambda (found)
              (funcall callback (if (file-exists-p file)
                                    (cons (cons (car module) file) found)
                                  found))))))))))

;;;; The page

(defun emjupy--widget-script-url (script)
  "Return SCRIPT, a URL or a file name, as a URL."
  (if (string-match-p "\\`[a-z]+://" script) script
    (concat "file://" (expand-file-name script))))

(defun emjupy--widget-page (token port title modules)
  "Return the page for the widget open as TOKEN, on the bridge at PORT.
TITLE names it; MODULES maps module names to the files they are in."
  (format "<!DOCTYPE html>
<html><head><meta charset=\"utf-8\"><title>%s</title>
%s
<style>body{margin:0;font-family:sans-serif} #widget{padding:8px}</style></head>
<body><div id=\"widget\">Connecting to Emacs…</div>
<script>
const MODULES = %s;
const paths = {};
for (const [name, file] of Object.entries(MODULES)) paths[name] = file.replace(/\\.js$/, '');
require.config({paths: paths});
require(['@jupyter-widgets/html-manager', '@jupyter-widgets/base'], function (hm, base) {
  const ws = new WebSocket('ws://127.0.0.1:%d/');
  const comms = {};
  const fromB64 = s => { const b = atob(s), a = new Uint8Array(b.length);
                         for (let i = 0; i < b.length; i++) a[i] = b.charCodeAt(i);
                         return new DataView(a.buffer); };
  const toB64 = d => { const u = d instanceof ArrayBuffer ? new Uint8Array(d)
                         : new Uint8Array(d.buffer, d.byteOffset, d.byteLength);
                       let s = ''; for (let i = 0; i < u.length; i++) s += String.fromCharCode(u[i]);
                       return btoa(s); };
  class Comm {
    constructor(id) { this.comm_id = id; this.target_name = 'jupyter.widget'; comms[id] = this; }
    open() { return ''; }
    close() { return ''; }
    send(data, callbacks, metadata, buffers) {
      ws.send(JSON.stringify({type: 'comm_msg', comm_id: this.comm_id, data: data,
                              buffers: (buffers || []).map(toB64)}));
      return '';
    }
    on_msg(callback) { this.onMsg = callback; }
    on_close(callback) { this.onClose = callback; }
  }
  const loader = (name, version) => (name in MODULES)
        ? new Promise((resolve, reject) => require([name], resolve, reject))
        : hm.requireLoader(name, version);
  class Manager extends hm.HTMLManager {
    constructor() { super({loader: loader}); }
    _get_comm_info() { return Promise.resolve({}); }
    _create_comm(target, id) { return Promise.resolve(comms[id] || new Comm(id)); }
  }
  const manager = new Manager();
  const el = document.getElementById('widget');
  const fail = e => { el.textContent = 'This widget could not be drawn: ' + e; console.error(e); };
  ws.onopen = () => ws.send(JSON.stringify({type: 'hello', page: %S}));
  ws.onclose = () => { document.title += ' (disconnected)'; };
  ws.onmessage = async (event) => {
    const m = JSON.parse(event.data);
    try {
      if (m.type === 'init') {
        await Promise.all(m.models.map(model => {
          const state = model.state;
          base.put_buffers(state, model.buffer_paths, model.buffers.map(fromB64));
          return manager.new_model({model_name: model.model_name, model_module: model.model_module,
                                    model_module_version: model.model_module_version,
                                    model_id: model.id, comm: new Comm(model.id)}, state);
        }));
        const view = await manager.create_view(await manager.get_model(m.view));
        el.textContent = '';
        await manager.display_view(view, el);
      } else if (m.type === 'comm_open') {
        await manager.handle_comm_open(new Comm(m.comm_id),
          {content: {comm_id: m.comm_id, target_name: 'jupyter.widget', data: m.data},
           buffers: m.buffers.map(fromB64), metadata: {version: '2.1.0'}});
      } else if (m.type === 'comm_msg' && comms[m.comm_id] && comms[m.comm_id].onMsg) {
        comms[m.comm_id].onMsg({content: {comm_id: m.comm_id, data: m.data},
                                buffers: m.buffers.map(fromB64)});
      } else if (m.type === 'comm_close' && comms[m.comm_id] && comms[m.comm_id].onClose) {
        comms[m.comm_id].onClose({content: {comm_id: m.comm_id, data: {}}});
      }
    } catch (e) { fail(e); }
  };
}, e => { document.getElementById('widget').textContent =
            'The widget manager could not be loaded: ' + e; });
</script></body></html>
"
          title
          (mapconcat (lambda (s) (format "<script src=\"%s\"></script>" (emjupy--widget-script-url s)))
                     emjupy-widget-scripts "\n")
          (json-serialize (let ((h (make-hash-table :test 'equal)))
                            (pcase-dolist (`(,name . ,file) modules)
                              (puthash name (concat "file://" file) h))
                            h))
          port token))

;;;; The bridge

(defun emjupy--bridge-port ()
  "Return the port of the bridge, starting it if need be."
  (unless (and emjupy--bridge (process-live-p emjupy--bridge))
    (setq emjupy--bridge
          (websocket-server 0 :host 'local
                            :on-message #'emjupy--bridge-on-message
                            :on-close #'emjupy--bridge-on-close)))
  (process-contact emjupy--bridge :service))

(defun emjupy--bridge-send (ws object)
  "Send OBJECT, a hash table, to a page on its WebSocket WS."
  (when (websocket-openp ws)
    (websocket-send-text ws (decode-coding-string (json-serialize object) 'utf-8))))

(defun emjupy--bridge-page-of (ws)
  "Return the token and the page whose WebSocket is WS, as a cons, or nil."
  (catch 'found
    (maphash (lambda (token page) (when (eq (plist-get page :ws) ws) (throw 'found (cons token page))))
             emjupy--bridge-pages)
    nil))

(defun emjupy--bridge-on-message (ws frame)
  "Handle FRAME from a page on WS: its greeting, or a widget\\='s message."
  (let* ((m (json-parse-string (websocket-frame-text frame) :object-type 'hash-table))
         (type (gethash "type" m)))
    (pcase type
      ("hello"
       (let* ((token (gethash "page" m))
              (page (gethash token emjupy--bridge-pages)))
         (when page
           (puthash token (plist-put page :ws ws) emjupy--bridge-pages)
           (let ((init (make-hash-table :test 'equal)))
             (puthash "type" "init" init)
             (puthash "view" (plist-get page :view) init)
             (puthash "models" (vconcat (mapcar #'emjupy--widget-model-json
                                                (plist-get page :models)))
                      init)
             (emjupy--bridge-send ws init)))))
      ("comm_msg"
       (when-let* ((page (cdr (emjupy--bridge-page-of ws))))
         (emjupy--widget-message (gethash "comm_id" m) (gethash "data" m) (plist-get page :cell)
                                 (mapcar #'base64-decode-string
                                         (append (gethash "buffers" m) nil))))))))

(defun emjupy--bridge-on-close (ws)
  "Forget the page whose WebSocket WS has closed."
  (when-let* ((found (emjupy--bridge-page-of ws)))
    (remhash (car found) emjupy--bridge-pages)))

(defun emjupy--bridge-on-comm (kernel msg-type content &optional buffers)
  "Pass KERNEL\\='s comm message to the pages showing its widgets.
MSG-TYPE is the message type, CONTENT its content and BUFFERS its
binary data.  A page gets what concerns its models, and every new model
of its kernel, which may come to be shown in it."
  (let ((id (gethash "comm_id" content)))
    (maphash
     (lambda (_token page)
       (when (and (eq (plist-get page :kernel) kernel) (plist-get page :ws)
                  (or (equal msg-type "comm_open") (member id (plist-get page :models))))
         (when (equal msg-type "comm_open")
           (plist-put page :models (cons id (plist-get page :models))))
         (let ((out (make-hash-table :test 'equal)))
           (puthash "type" msg-type out)
           (puthash "comm_id" id out)
           (puthash "data" (or (gethash "data" content) (make-hash-table :test 'equal)) out)
           (puthash "buffers" (vconcat (mapcar (lambda (b) (base64-encode-string (emjupy-bytes-data b) t))
                                               buffers))
                    out)
           (emjupy--bridge-send (plist-get page :ws) out))))
     emjupy--bridge-pages)))

;;;; Opening a page

(defun emjupy-widget-open-page (id cell)
  "Open the widget with comm ID, in CELL, in a page of its own.
Its JavaScript draws it there, live: a change made in the page reaches
the kernel, and one made in the kernel reaches the page."
  (let* ((kernel (car (gethash id emjupy--widget-models)))
         (models (emjupy--widget-tree id))
         (modules (delete-dups
                   (cl-loop for m in models
                            for s = (emjupy--widget-state m)
                            append (cl-loop for (name . version)
                                            in (list (cons (gethash "_model_module" s)
                                                           (gethash "_model_module_version" s))
                                                     (cons (gethash "_view_module" s)
                                                           (gethash "_view_module_version" s)))
                                            when (and (stringp name)
                                                      (not (member name emjupy--standard-widget-modules)))
                                            collect (cons name version)))))
         (token (format "%s-%s" id (random 100000000)))
         (title (emjupy--widget-scripted-name (emjupy--widget-state id))))
    (unless (and kernel (emjupy--ws-live-p kernel))
      (user-error "This widget's kernel is not connected"))
    (message "[emjupy] Opening the widget...")
    (emjupy--widget-fetch-modules
     kernel modules
     (lambda (found)
       (puthash token (list :kernel kernel :cell cell :view id :models models) emjupy--bridge-pages)
       (emjupy--show-file
        (emjupy--write-page (emjupy--widget-page token (emjupy--bridge-port) title found)))))))

(defun emjupy-widget-page-enable ()
  "Open widgets that draw themselves in a page, and keep the pages in step.
Run when a notebook buffer starts, like the other hooks between layers."
  (add-hook 'emjupy-comm-functions #'emjupy--bridge-on-comm)
  (setq emjupy-widget-page-function #'emjupy-widget-open-page)
  (emjupy--widget-add-rule (list 'scripted #'emjupy--widget-scripted-p
                                 #'emjupy--widget-draw-scripted)))

(provide 'emjupy-widget-page)

;;; emjupy-widget-page.el ends here
