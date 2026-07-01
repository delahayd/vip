#!/usr/bin/env bash
set -euo pipefail

ROOT=${VIP_PROBLEM_ROOT:-/home/etudiant/Cours/stage/casc}
TPTP=${VIP_TPTP_ROOT:-$ROOT}
VIP_BIN=${VIP_BIN:-./_build/default/src/main.exe}
TPTP4X=${TPTP4X:-/home/etudiant/Cours/stage/TPTP-v9.2.1/Scripts/tptp4X}
GDV=${GDV:-/home/etudiant/Cours/stage/GDV/GDV}
OUT_DIR=${OUT_DIR:-/tmp/vip_tstp_validation_$(date +%Y%m%d_%H%M%S)}
PROVER_TIMEOUT=${PROVER_TIMEOUT:-60}
PROVER_WALL_TIMEOUT=${PROVER_WALL_TIMEOUT:-$((PROVER_TIMEOUT + 30))}
GDV_TIMEOUT=${GDV_TIMEOUT:-180}
PORTFOLIO=${PORTFOLIO:-casc-150}
MAX_CLAUSES=${MAX_CLAUSES:-75000}
GDV_FLAGS=${GDV_FLAGS:--q2 -d -u}

if [ "$#" -eq 0 ]; then
  cat >&2 <<'EOF'
Usage:
  bench/validate_tstp_proofs.sh PROBLEM...

Problems may be absolute paths or paths relative to $VIP_PROBLEM_ROOT/FOF.

Useful environment variables:
  VIP_PROBLEM_ROOT  CASC/TPTP problem root containing FOF/ (default: /home/etudiant/Cours/stage/casc)
  VIP_TPTP_ROOT     include root passed to VIP through --tptp (default: VIP_PROBLEM_ROOT)
  VIP_BIN           prover binary (default: ./_build/default/src/main.exe)
  TPTP4X            tptp4X executable
  GDV               GDV executable
  GDV_FLAGS         GDV flags (default: -q2 -d -u)
  OUT_DIR           validation output directory
  PROVER_TIMEOUT    VIP internal timeout in seconds
  GDV_TIMEOUT       GDV wall timeout in seconds
  PORTFOLIO         VIP portfolio
  MAX_CLAUSES       VIP max clauses
EOF
  exit 2
fi

mkdir -p "$OUT_DIR"
failures=0

resolve_problem() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    *) printf '%s/FOF/%s\n' "$ROOT" "$1" ;;
  esac
}

for problem_arg in "$@"; do
  problem=$(resolve_problem "$problem_arg")
  base=$(basename "$problem" .p)
  proof="$OUT_DIR/$base.out"
  prover_log="$OUT_DIR/$base.prover.err"
  tptp4x_log="$OUT_DIR/$base.tptp4x.log"
  gdv_log="$OUT_DIR/$base.gdv.log"

  echo "== $problem_arg"

  if ! timeout "$PROVER_WALL_TIMEOUT" "$VIP_BIN" \
      --competition-output \
      --proof-tstp \
      --portfolio "$PORTFOLIO" \
      --time-limit "$PROVER_TIMEOUT" \
      --max-clauses "$MAX_CLAUSES" \
      --tptp "$TPTP" \
      "$problem" > "$proof" 2> "$prover_log"; then
    echo "  prover=FAIL"
    failures=$((failures + 1))
    continue
  fi

  status=$(sed -n 's/^% SZS status \([^ ]*\).*/\1/p' "$proof" | head -1)
  echo "  status=${status:-missing}"

  if ! grep -q '^% SZS output start' "$proof"; then
    echo "  proof=MISSING"
    failures=$((failures + 1))
    continue
  fi

  if "$TPTP4X" -q1 "$proof" > "$tptp4x_log" 2>&1; then
    echo "  tptp4x=OK"
  else
    echo "  tptp4x=FAIL"
    tail -20 "$tptp4x_log" | sed 's/^/    /'
    failures=$((failures + 1))
    continue
  fi

  if TPTP="$TPTP" timeout "$GDV_TIMEOUT" "$GDV" $GDV_FLAGS -p "$problem" "$proof" > "$gdv_log" 2>&1; then
    verdict=$(grep 'SZS status Verified' "$gdv_log" | tail -1 || true)
    if printf '%s\n' "$verdict" | grep -q 'VerifiedGood'; then
      echo "  gdv=$verdict"
    else
      echo "  gdv=NO_VERIFIEDGOOD"
      grep -E 'SZS status|FAILURE|WARNING: Leaf|ERROR:|Not verified' "$gdv_log" | tail -30 | sed 's/^/    /' || true
      failures=$((failures + 1))
    fi
  else
    rc=$?
    echo "  gdv=FAIL rc=$rc"
    grep -E 'SZS status Verified|FAILURE|WARNING: Leaf|ERROR:' "$gdv_log" | tail -30 | sed 's/^/    /' || true
    failures=$((failures + 1))
  fi
done

echo "outputs=$OUT_DIR"
exit "$failures"
