#!/usr/bin/env python3
"""Show a page in a window of its own, for emjupy's interactive figures.

    emjupy-figure-window.py URL [TITLE]

A WebKitGTK window -- the engine GNOME uses -- with WebGL on, so that 3D
plotly figures draw.  One window per figure; closing it ends the
program.  Needs PyGObject and WebKitGTK: python3-gi and
gir1.2-webkit2-4.1 on Debian and Ubuntu, python3-gobject and
webkit2gtk4.1 on Fedora.
"""
import sys

import gi

gi.require_version("Gtk", "3.0")
for version in ("4.1", "4.0"):
    try:
        gi.require_version("WebKit2", version)
        break
    except ValueError:
        continue
from gi.repository import Gtk, WebKit2  # noqa: E402


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    window = Gtk.Window(title=sys.argv[2] if len(sys.argv) > 2 else "emjupy figure")
    window.set_default_size(900, 700)
    view = WebKit2.WebView()
    settings = view.get_settings()
    settings.set_enable_webgl(True)
    # the page's own title, once it has one: a figure's title, say
    view.connect("notify::title",
                 lambda v, _: v.get_title() and window.set_title(v.get_title()))
    view.load_uri(sys.argv[1])
    window.add(view)
    window.connect("destroy", Gtk.main_quit)
    window.show_all()
    Gtk.main()


if __name__ == "__main__":
    main()
