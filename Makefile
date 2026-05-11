# ========= Configuration =========

FILE ?=
#DIR ?= tests/integration
DIR ?= bench/problems/TPTP-v9.2.1/Problems/SYN/
MODE ?= ordered-fallback
TIME_LIMIT ?= 3
ROBUST_TIME_LIMIT ?=
MAX_CLAUSES ?=
CASC ?=

PROVER = dune exec -- bin/main.exe
BENCH = dune exec -- bench/run_bench.exe
REGRESSION = dune exec bench/check_regression.exe

# ========= Helpers =========

define build_args
$(if $(TIME_LIMIT),--time-limit $(TIME_LIMIT)) \
$(if $(ROBUST_TIME_LIMIT),--robust-time-limit) \
$(if $(MAX_CLAUSES),--max-clauses $(MAX_CLAUSES)) \
--mode $(MODE)
endef

# ========= Targets =========

.PHONY: all
all: build install

.PHONY: build
build:
	@echo -n "==> Building project: "
	dune build

.PHONY: install
install:
	@echo -n "==> Installing project (in local): "
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
	$(BENCH) --onlyip --dir $(DIR) $(call build_args)

.PHONY: bench-dir
bench-dir:
	@if [ -z "$(DIR)" ]; then \
	  echo "Usage: make bench-dir DIR=path"; \
	  exit 1; \
	fi
	$(BENCH) --onlyip --dir $(DIR) $(call build_args)

.PHONY: benchs
benchs:
	$(BENCH) --dir $(DIR) $(call build_args)

.PHONY: benchs-dir
benchs-dir:
	@if [ -z "$(DIR)" ]; then \
	  echo "Usage: make bench-dir DIR=path"; \
	  exit 1; \
	fi
	$(BENCH) --dir $(DIR) $(call build_args)

.PHONY: bench-regression
bench-regression:
	$(REGRESSION) -- $(OLD) $(NEW)

.PHONY: bench-casc
bench-casc:
	$(BENCH) --onlyip --dir $(DIR) --casc $(CASC) $(call build_args)

.PHONY: benchs-casc
benchs-casc:
	$(BENCH) --dir $(DIR) --casc $(CASC) $(call build_args)

.PHONY: clean
clean:
	dune clean
	find . -type f -name '*~' -delete
