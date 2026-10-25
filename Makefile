# Copyright (C) 2026 Mathieu Renzo
# SPDX-License-Identifier: GPL-3.0-or-later
# emjupy -- build, test and package
#
# The tar unpacks into one emjupy-VERSION/ directory, emjupy-pkg.el beside
# the sources: the Emacs Lisp manual's "Multi-file Packages".
#
#   make compile   byte-compile everything (read the warnings)
#   make test      unit tests only (no server needed)
#   make check     unit + integration tests (needs a live Jupyter server)
#   make lint      checkdoc and package-lint, both required
#   make docs      export docs/*.org, and the dependency graph, to html-content/
#   make versions  which Emacs this is, against the one we target
#   make package   build emjupy-VERSION.tar
#   make install   install that tar into this Emacs via package-install-file
#   make clean

EMACS   ?= emacs

# Emacs 30.1: the least the package runs on, and what CI runs.  `make
# versions' says which Emacs this is.
EMACS_TARGET = 30.1
SRC     := src
TEST    := test
VERSION := $(shell sed -n 's/^;; Version: \(.*\)/\1/p' $(SRC)/emjupy.el)
PKG     := emjupy-$(VERSION)
TAR     := $(PKG).tar

# websocket is the only external dependency. Point this at a checkout to run
# offline, otherwise it is resolved from your package dir.
WEBSOCKET ?= $(EMJUPY_WEBSOCKET_DIR)
LOADPATH  := -L $(SRC) -L $(TEST) $(if $(WEBSOCKET),-L $(WEBSOCKET),)

# Load order matters: each file is compiled against the ones it requires.
SOURCES = $(addprefix $(SRC)/, emjupy-core.el emjupy-terminal.el emjupy-ipynb.el emjupy-latex.el emjupy-undo.el emjupy-faces.el emjupy-http.el emjupy-render.el \
            emjupy-cells.el emjupy-redraw.el emjupy-commands.el emjupy-kernel.el emjupy-figures.el emjupy-widgets.el emjupy-widget-page.el emjupy-remote.el emjupy-lsp.el emjupy-shadow.el \
            emjupy-eglot.el emjupy-mode.el emjupy-save.el emjupy-notebook.el emjupy-dashboard.el emjupy-tramp-open.el emjupy.el)
# No README in the tar: C-h P would show a web page -- badges, links --
# before the Commentary.  MELPA leaves it out too.
PKGFILES = $(SOURCES) $(SRC)/emjupy-pkg.el $(SRC)/emjupy-figure-window.py

.PHONY: all compile test check lint checkdoc package-lint docs dependency-graph coverage set-version versions check-version package install clean timestamps print-package-files recipe-check

all: compile

compile:
	@for f in $(SOURCES); do \
	  $(EMACS) -batch -Q $(LOADPATH) -f batch-byte-compile $$f || exit 1; \
	done

test:
	$(EMACS) -batch -Q $(LOADPATH) -l $(TEST)/emjupy-run-tests.el

# The same suite as `test', insisting on EMJUPY_TEST_URL: without it the
# integration tests skip, and a green run would have run half.
check:
ifndef EMJUPY_TEST_URL
	$(error EMJUPY_TEST_URL is not set, so the integration tests would be \
skipped.  Set EMJUPY_TEST_URL, EMJUPY_TEST_TOKEN and EMJUPY_TEST_ROOT, or \
run `make test' if you only want the unit tests)
endif
	$(EMACS) -batch -Q $(LOADPATH) -l $(TEST)/emjupy-run-tests.el

# Line coverage, as lcov in coverage/lcov.info -- of the live tests too,
# given their variables.  Needs undercover.el, installed or in
# EMJUPY_UNDERCOVER_PATH; runs from .el files, which it instruments as
# they load.
coverage:
	@rm -f $(SRC)/*.elc
	@mkdir -p coverage
	@status=0; \
	EMJUPY_COVERAGE=$(abspath coverage/lcov.info) \
	  $(EMACS) -batch -Q $(LOADPATH) -l $(TEST)/emjupy-run-tests.el || status=$$?; \
	python3 tools/coverage-summary.py coverage/lcov.info; \
	exit $$status

# What a MELPA review checks, and what the code already follows by hand.
# Which Emacs is this, and is it the one development targets?
versions:
	@$(EMACS) -batch -Q --eval '(message "emacs %s (development targets $(EMACS_TARGET))" emacs-version)'
	@$(EMACS) -batch -Q --eval '(if (version< emacs-version "$(EMACS_TARGET)") (message "  older than the target: test on $(EMACS_TARGET) before trusting a result") (message "  ok"))'

# The dependency graph, drawn from the requires, into the site: docs draws
# it first, so the site's is always the code's.
dependency-graph:
	@python3 tools/dependency-graph.py html-content

# docs/*.org to the site, in html-content/.
docs: dependency-graph
	$(EMACS) -batch -Q -l publish.el all

# Every file the package ships: what the MELPA recipe must list.
print-package-files:
	@for f in $(SOURCES) $(filter-out $(SRC)/emjupy-pkg.el,$(PKGFILES)); do echo $$f; done | sort -u

# The MELPA recipe lists every file, and the package builds from it alone.
recipe-check:
	@tools/check-recipe.sh $(EMACS)

# The .el files the package ships, as named in the source directory.
LINT_FILES = $(notdir $(SOURCES))

lint: checkdoc package-lint

checkdoc:
	cd $(SRC) && $(EMACS) -batch -Q -L . $(if $(WEBSOCKET),-L $(abspath $(WEBSOCKET)),) \
	  -l ../tools/checkdoc.el $(LINT_FILES)

# package-lint as MELPA runs it, warnings failing too.  It has to be
# installed -- from MELPA, or PACKAGE_LINT_DIR naming a checkout -- and an
# archive must list the dependencies: without either, this fails.
package-lint:
	cd $(SRC) && $(EMACS) -batch -Q \
	  $(if $(PACKAGE_LINT_DIR),-L $(abspath $(PACKAGE_LINT_DIR)),) \
	  -l ../tools/lint-package.el $(LINT_FILES)

# Write a new version everywhere the current one is -- the header,
# `emjupy-version', the pkg file, the docs' sample: make set-version
# VERSION=0.1.3.  Also the "Set version" workflow.
set-version:
	@test -n "$(VERSION)" || { echo "usage: make set-version VERSION=X.Y.Z" >&2; exit 2; }
	@tools/set-version.sh "$(VERSION)"

# The Version: header, `emjupy-version' and, from the release workflow,
# the tag being released must agree.
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
