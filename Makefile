# ========= Configuration =========

FILE ?=
#DIR ?= tests/integration
DIR ?= bench/problems/TPTP-v9.2.1/Problems/SYN/
MODE ?= ordered-fallback
TIME_LIMIT ?= 3
MAX_CLAUSES ?=

PROVER = dune exec -- bin/main.exe
BENCH = dune exec -- bench/run_bench.exe
BENCH_COMPARE = dune exec bench/run_bench_compare.exe

# ========= Helpers =========

define build_args
$(if $(TIME_LIMIT),--time-limit $(TIME_LIMIT)) \
$(if $(MAX_CLAUSES),--max-clauses $(MAX_CLAUSES)) \
--mode $(MODE)
endef

# ========= Targets =========

.PHONY: all
all: build install

.PHONY: build
build:
	dune build

.PHONY: install
install:
	dune install --prefix .

.PHONY: run
run:
	@if [ -z "$(FILE)" ]; then \
	  echo "Usage: make run FILE=path/to/file.p"; \
	  exit 1; \
	fi
	$(PROVER) $(call build_args) $(FILE)

.PHONY: test
test:
	$(PROVER) tests/integration/simple_unsat.p

.PHONY: bench
bench:
	$(BENCH) --dir $(DIR) $(call build_args)

.PHONY: bench-dir
bench-dir:
	@if [ -z "$(DIR)" ]; then \
	  echo "Usage: make bench-dir DIR=path"; \
	  exit 1; \
	fi
	$(BENCH) --dir $(DIR) $(call build_args)

.PHONY: bench-compare
bench-compare:
	$(BENCH_COMPARE) -- $(DIR) $(TIME_LIMIT)

.PHONY: benchs
benchs:
	$(BENCH_COMPARE) -- $(DIR) $(TIME_LIMIT)

.PHONY: clean
clean:
	dune clean
	find . -type f -name '*~' -delete
