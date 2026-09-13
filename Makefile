# emjupy -- build, test and package
#
# The layout follows the Emacs Lisp manual's "Multi-file Packages": the tar
# unpacks into a single emjupy-VERSION/ directory containing emjupy-pkg.el
# alongside the sources.
#
#   make compile   byte-compile everything (warnings are errors-ish: read them)
#   make test      unit tests only (no server needed)
#   make check     unit + integration tests (needs a live Jupyter server)
#   make package   build emjupy-VERSION.tar
#   make install   install that tar into this Emacs via package-install-file
#   make clean

EMACS   ?= emacs
VERSION := $(shell sed -n 's/^;; Version: \(.*\)/\1/p' emjupy.el)
PKG     := emjupy-$(VERSION)
TAR     := $(PKG).tar

# websocket is the only external dependency. Point this at a checkout to run
# offline, otherwise it is resolved from your package dir.
WEBSOCKET ?= $(EMJUPY_WEBSOCKET_DIR)
LOADPATH  := -L . $(if $(WEBSOCKET),-L $(WEBSOCKET),)

# Load order matters: each file is compiled against the ones it requires.
SOURCES = emjupy-core.el emjupy-http.el emjupy-render.el emjupy-cells.el \
          emjupy-kernel.el emjupy-lsp.el emjupy-eglot.el emjupy-notebook.el emjupy.el
PKGFILES = $(SOURCES) emjupy-pkg.el README.org

.PHONY: all compile test check check-version package install clean timestamps

all: compile

compile:
	@for f in $(SOURCES); do \
	  $(EMACS) -batch -Q $(LOADPATH) -f batch-byte-compile $$f || exit 1; \
	done

test:
	$(EMACS) -batch -Q $(LOADPATH) -l emjupy-run-tests.el

# Integration tests are opt-in; see the README for the environment variables.
check:
	$(EMACS) -batch -Q $(LOADPATH) -l emjupy-run-tests.el

# Called by the release workflow, which passes the tag being released.
# Three things have to agree or a release ships claiming to be a version
# it is not: the Version: header, the emjupy-version constant that
# M-x emjupy-version reports, and the tag itself.
check-version:
	@version=$$(sed -n 's/^;; Version: *//p' emjupy.el | head -1); \
	constant=$$(sed -n 's/^(defconst emjupy-version "\([^"]*\)".*/\1/p' emjupy.el | head -1); \
	tag=$$(echo "$(TAG)" | sed 's/^v//'); \
	if [ -z "$$version" ]; then \
	  echo "check-version: no ';; Version:' header in emjupy.el" >&2; exit 1; \
	fi; \
	if [ "$$constant" != "$$version" ]; then \
	  echo "check-version: emjupy-version is $$constant but the header says $$version" >&2; \
	  exit 1; \
	fi; \
	if [ -n "$$tag" ] && [ "$$tag" != "$$version" ]; then \
	  echo "check-version: tag $$tag does not match version $$version" >&2; \
	  exit 1; \
	fi; \
	echo "check-version: $$version$${tag:+ (tag $$tag)} ok"

package:
	@rm -rf $(PKG) $(TAR)
	@mkdir -p $(PKG)
	@cp $(PKGFILES) $(PKG)/
	@tar -cf $(TAR) $(PKG)
	@rm -rf $(PKG)
	@echo "built $(TAR)"

install: package
	$(EMACS) -batch -Q --eval "(progn (require 'package) (package-initialize) \
	  (package-install-file (expand-file-name \"$(TAR)\")))"

timestamps:
	@find . -name '*.el' -o -name 'Makefile' -o -name '*.org' | xargs touch
	@echo "timestamps reset to now"

clean:
	rm -f *.elc $(TAR)
	rm -rf $(PKG)
