# wyo: tuna/ tree (the tuna history, full lineage, filter-repo'd under tuna/).
# Copy of record: scripts/monorepo-tuna.Makefile, promoted to tuna/Makefile by
# dune (@ci-dirs). Law (2): a push touching tuna/** runs THIS flow only --
# never PP's build, never sundial's rc.d install.
TUNA_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
REPO_ROOT := $(abspath $(TUNA_DIR)/..)
MIDDEN_PREFIX := $(TUNA_DIR)/_build/midden-prefix

.PHONY: all build test deploy midden-stage

all: build

# Stage the workspace's midden package into a local prefix so tuna builds
# directly against the current worktree's midden without mutating the switch.
midden-stage:
	@if [ -f "$(REPO_ROOT)/midden/dune-project" ]; then \
	  dune build --root=$(REPO_ROOT) @midden/install && \
	  dune install --root=$(REPO_ROOT) --prefix=$(MIDDEN_PREFIX) midden >/dev/null; \
	fi

build: midden-stage
	OCAMLPATH="$(MIDDEN_PREFIX)/lib$${OCAMLPATH:+:$$OCAMLPATH}" dune build --root=$(TUNA_DIR)

test: midden-stage
	OCAMLPATH="$(MIDDEN_PREFIX)/lib$${OCAMLPATH:+:$$OCAMLPATH}" dune runtest --root=$(TUNA_DIR)

deploy:
	@echo "tuna: deploy $(TUNA_DIR) (path-filtered: tuna/** only)"
