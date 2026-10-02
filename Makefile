# emjupy -- build, test and package
#
# The layout follows the Emacs Lisp manual's "Multi-file Packages": the tar
# unpacks into a single emjupy-VERSION/ directory containing emjupy-pkg.el
# alongside the sources.
#
#   make compile   byte-compile everything (warnings are errors-ish: read them)
#   make test      unit tests only (no server needed)
#   make check     unit + integration tests (needs a live Jupyter server)
#   make lint      checkdoc and, if installed, package-lint
#   make docs      export docs/*.org to docs/html/
#   make versions  which Emacs this is, against the one we target
#   make package   build emjupy-VERSION.tar
#   make install   install that tar into this Emacs via package-install-file
#   make clean

EMACS   ?= emacs

# Development targets Emacs 30.1, which is what CI runs and what most
# users are on.  The package still declares 29.1 as its minimum and is
# tested against it, but 29 is not the version to develop in: Eglot
# manages buffers differently there, and a bug that made every language
# server request go to a local process was invisible on 29 for weeks
# because that is where it was being tested.  `make versions' says which
# Emacs this is.
EMACS_TARGET = 30.1
SRC     := src
VERSION := $(shell sed -n 's/^;; Version: \(.*\)/\1/p' $(SRC)/emjupy.el)
PKG     := emjupy-$(VERSION)
TAR     := $(PKG).tar

# websocket is the only external dependency. Point this at a checkout to run
# offline, otherwise it is resolved from your package dir.
WEBSOCKET ?= $(EMJUPY_WEBSOCKET_DIR)
LOADPATH  := -L $(SRC) $(if $(WEBSOCKET),-L $(WEBSOCKET),)

# Load order matters: each file is compiled against the ones it requires.
SOURCES = $(addprefix $(SRC)/, emjupy-core.el emjupy-http.el emjupy-render.el \
            emjupy-cells.el emjupy-kernel.el emjupy-figures.el emjupy-widgets.el emjupy-remote.el emjupy-lsp.el \
            emjupy-eglot.el emjupy-mode.el emjupy-notebook.el emjupy.el)
# No README: an installed package's README is what C-h P shows, before
# the Commentary, and the README is a web page -- badges, screenshots,
# links into the repository.  MELPA leaves it out for the same reason, so
# the release tarball and a MELPA install describe themselves alike.
PKGFILES = $(SOURCES) $(SRC)/emjupy-pkg.el

.PHONY: all compile test check lint docs coverage set-version versions check-version package install clean timestamps

all: compile

compile:
	@for f in $(SOURCES); do \
	  $(EMACS) -batch -Q $(LOADPATH) -f batch-byte-compile $$f || exit 1; \
	done

test:
	$(EMACS) -batch -Q $(LOADPATH) -l $(SRC)/emjupy-run-tests.el

# Integration tests are opt-in; see the README for the environment variables.
# The command is the same as `test': what decides whether the integration
# tests run is EMJUPY_TEST_URL, which they check themselves and skip without.
# So `check' insists on it -- running `check' and silently getting only the
# unit tests is how a suite comes to look green while the half most likely to
# catch a regression never ran.
check:
ifndef EMJUPY_TEST_URL
	$(error EMJUPY_TEST_URL is not set, so the integration tests would be \
skipped.  Set EMJUPY_TEST_URL, EMJUPY_TEST_TOKEN and EMJUPY_TEST_ROOT, or \
run `make test' if you only want the unit tests)
endif
	$(EMACS) -batch -Q $(LOADPATH) -l $(SRC)/emjupy-run-tests.el

# Line coverage of the test suite, as lcov in coverage/lcov.info.  Uses
# whichever tests can run: set the integration variables, as for `check',
# and the server-facing code is covered too.  Needs undercover.el and its
# dependencies (dash, shut-up) -- installed, or named in
# EMJUPY_UNDERCOVER_PATH.  Runs from .el files: instrumentation happens at
# load time, and a compiled file would be loaded instead.
coverage:
	@rm -f $(SRC)/*.elc
	@mkdir -p coverage
	@status=0; \
	EMJUPY_COVERAGE=$(abspath coverage/lcov.info) \
	  $(EMACS) -batch -Q $(LOADPATH) -l $(SRC)/emjupy-run-tests.el || status=$$?; \
	python3 tools/coverage-summary.py coverage/lcov.info; \
	exit $$status

# What a MELPA review checks, and what the code already follows by hand.
# Which Emacs is this, and is it the one development targets?
versions:
	@$(EMACS) -batch -Q --eval '(message "emacs %s (development targets $(EMACS_TARGET))" emacs-version)'
	@$(EMACS) -batch -Q --eval '(if (version< emacs-version "$(EMACS_TARGET)") (message "  older than the target: test on $(EMACS_TARGET) before trusting a result") (message "  ok"))'

# Export docs/*.org to docs/html/, which is what GitHub Pages serves when
# a site is set to publish from the docs/ folder.
docs:
	$(EMACS) -batch -Q -l publish.el all

lint:
	cd $(SRC) && $(EMACS) -batch -Q -L . $(if $(WEBSOCKET),-L $(abspath $(WEBSOCKET)),) -l lint.el

# Called by the release workflow, which passes the tag being released.
# Three things have to agree or a release ships claiming to be a version
# it is not: the Version: header, the emjupy-version constant that
# M-x emjupy-version reports, and the tag itself.
# Set the version everywhere the current one is written -- the header,
# `emjupy-version', the pkg file and the sample output in the docs:
#   make set-version VERSION=0.1.3
# Also run from GitHub by the "Set version" workflow.
set-version:
	@test -n "$(VERSION)" || { echo "usage: make set-version VERSION=X.Y.Z" >&2; exit 2; }
	@tools/set-version.sh "$(VERSION)"

check-version:
	@version=$$(sed -n 's/^;; Version: *//p' $(SRC)/emjupy.el | head -1); \
	constant=$$(sed -n 's/^(defconst emjupy-version "\([^"]*\)".*/\1/p' $(SRC)/emjupy.el | head -1); \
	tag=$$(echo "$(TAG)" | sed 's/^v//'); \
	if [ -z "$$version" ]; then \
	  echo "check-version: no ';; Version:' header in $(SRC)/emjupy.el" >&2; exit 1; \
	fi; \
	if [ "$$constant" != "$$version" ]; then \
	  echo "check-version: emjupy-version is $$constant but the header says $$version" >&2; \
	  exit 1; \
	fi; \
	pkg=$$(sed -n 's/^(define-package "emjupy" "\([^"]*\)".*/\1/p' $(SRC)/emjupy-pkg.el | head -1); \
	if [ "$$pkg" != "$$version" ]; then \
	  echo "check-version: emjupy-pkg.el says $$pkg but the header says $$version" >&2; \
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
	rm -f $(SRC)/*.elc *.elc $(TAR)
	rm -rf $(PKG)
