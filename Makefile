# wyo: tuna/ tree (the tuna history, full lineage, filter-repo'd under tuna/).
# Copy of record: scripts/monorepo-tuna.Makefile, promoted to tuna/Makefile by
# dune (@ci-dirs). Law (2): a push touching tuna/** runs THIS flow only --
# never PP's build, never sundial's rc.d install.
TUNA_DIR := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

.PHONY: all build test deploy

all: build

build:
	dune build --root=$(TUNA_DIR)

test:
	dune runtest --root=$(TUNA_DIR)

deploy:
	@echo "tuna: deploy $(TUNA_DIR) (path-filtered: tuna/** only)"
