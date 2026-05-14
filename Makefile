# ========= Configuration =========

FILE ?=
#DIR ?= tests/integration
DIR ?= bench/problems/TPTP-v9.2.1/Problems/SYN/
MODE ?= ordered-fallback
TIME_LIMIT ?= 3
ROBUST_TIME_LIMIT ?=
MAX_CLAUSES ?=
CASC ?=
BENCH_LOGS=bench/logs
MESO=delahayed@io-login.meso.umontpellier.fr
MESO_BENCH_LOGS=benchs-ip/logs

PROVER = dune exec -- bin/main.exe
BENCH = dune exec -- bench/run_bench.exe
REGRESSION = dune exec bench/check_regression.exe

# ========= Helpers =========

define build_args
$(if $(TIME_LIMIT),--time-limit $(TIME_LIMIT)) \
$(if $(ROBUST_TIME_LIMIT),--robust-time-limit) \
$(if $(MAX_CLAUSES),--max-clauses $(MAX_CLAUSES)) \
$(if $(LOGS),--logs $(LOGS)) \
$(if $(HOME_DIR),--home $(HOME_DIR)) \
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
	$(PROVER) $(call build_args) $(FILE)

.PHONY: test
test:
	$(PROVER) tests/integration/simple_unsat.p

.PHONY: bench
bench:
	$(BENCH) --onlyip --dir $(DIR) $(call build_args)

.PHONY: benchs
benchs:
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

.PHONY: fetch_logs
fetch-logs:
	scp $(MESO):$(MESO_BENCH_LOGS)/* $(BENCH_LOGS)

.PHONY: clean
clean:
	dune clean
	find . -type f -name '*~' -delete
