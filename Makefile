# ========= Configuration =========

FILE ?=
#DIR ?= tests/integration
DIR ?= bench/problems/TPTP-v9.2.1/Problems/SYN/
MODE ?= ordered-fallback
PORTFOLIO ?=
TIME_LIMIT ?= 3
ROBUST_TIME_LIMIT ?=
MAX_CLAUSES ?=
CASC ?=
BENCH_LOGS=bench/logs
MESO=nokranii@io-login.meso.umontpellier.fr
MESO_BENCH_LOGS=benchs-vip/logs
LOCAL_PROJECT_DIR ?= ip
REMOTE_PROJECT_DIR ?= vip

PROVER = dune exec -- src/main.exe
BENCH = dune exec -- bench/run_bench.exe
REGRESSION = dune exec -- bench/check_regression.exe

# ========= Helpers =========

define build_args
$(if $(TIME_LIMIT),--time-limit $(TIME_LIMIT)) \
$(if $(ROBUST_TIME_LIMIT),--robust-time-limit) \
$(if $(MAX_CLAUSES),--max-clauses $(MAX_CLAUSES)) \
$(if $(PORTFOLIO),--portfolio $(PORTFOLIO)) \
$(if $(LOGS_DIR),--logs $(LOGS_DIR)) \
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
	$(BENCH) --onlyvip --dir $(DIR) $(call build_args)

.PHONY: benchs
benchs:
	$(BENCH) --dir $(DIR) $(call build_args)

.PHONY: bench-regression
bench-regression:
	$(REGRESSION) --home $(HOME_DIR) $(OLD) $(NEW)

.PHONY: bench-casc
bench-casc:
	$(BENCH) --onlyvip --dir $(DIR) --casc $(CASC) $(call build_args)

.PHONY: benchs-casc
benchs-casc:
	$(BENCH) --dir $(DIR) --casc $(CASC) $(call build_args)

.PHONY: fetch_logs
fetch-logs:
	scp -rp $(MESO):$(MESO_BENCH_LOGS)/* $(BENCH_LOGS)

.PHONY: copy-local
copy-local:
	cd .. ; \
	tar --exclude='$(LOCAL_PROJECT_DIR)/.git' \
	--exclude='$(LOCAL_PROJECT_DIR)/_build' \
	--exclude='$(LOCAL_PROJECT_DIR)/_opam' \
	--exclude='$(LOCAL_PROJECT_DIR)/_opam' \
	--exclude='$(LOCAL_PROJECT_DIR)/bench/problems' \
	-cf - $(LOCAL_PROJECT_DIR) \
	| ssh $(MESO) 'cd benchs-vip ; tar -xf - ; if [ "$(LOCAL_PROJECT_DIR)" != "$(REMOTE_PROJECT_DIR)" ]; then rm -rf "$(REMOTE_PROJECT_DIR)" && mv "$(LOCAL_PROJECT_DIR)" "$(REMOTE_PROJECT_DIR)"; fi'

.PHONY: clean
clean:
	dune clean
	find . -type f -name '*~' -delete
