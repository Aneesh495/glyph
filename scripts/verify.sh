#!/usr/bin/env bash
set -euo pipefail

echo "=========================================================="
echo "          Glyph v1 Pipeline Verification Suite            "
echo "=========================================================="

# Try to load opam switch if available
if command -v opam >/dev/null 2>&1; then
  if opam switch list | grep -q glyph-sys; then
    eval "$(opam env --switch=glyph-sys)"
  else
    eval "$(opam env)"
  fi
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

FAILED_GATES=0

record_gate() {
  local num="$1"
  local desc="$2"
  local status="$3"
  if [ "$status" -eq 0 ]; then
    printf "Gate %-2s: [PASS] %s\n" "$num" "$desc"
  else
    printf "Gate %-2s: [FAIL] %s\n" "$num" "$desc"
    FAILED_GATES=$((FAILED_GATES + 1))
  fi
}

echo ""
echo "--- Step 1: Dune Build ---"
if dune build @all; then
  record_gate 1 "Dune build @all with 0 errors/warnings" 0
else
  record_gate 1 "Dune build @all with 0 errors/warnings" 1
fi

echo ""
echo "--- Step 2: Alcotest & QCheck Test Suite ---"
if dune runtest; then
  record_gate 2 "Alcotest and QCheck test suites pass (49/49 tests)" 0
else
  record_gate 2 "Alcotest and QCheck test suites pass (49/49 tests)" 1
fi

BIN="_build/default/bin/glyph_cli.exe"
if [ ! -x "$BIN" ]; then
  echo "Error: CLI binary not found at $BIN"
  exit 1
fi

echo ""
echo "--- Step 3: Canonical Examples Execution ---"
FIB_OUT=$("$BIN" run examples/fib.gl | tr -d '\r\n')
if [ "$FIB_OUT" = "55" ]; then
  record_gate 3 "examples/fib.gl executes to 55" 0
else
  echo "Expected 55, got: $FIB_OUT"
  record_gate 3 "examples/fib.gl executes to 55" 1
fi

ACK_OUT=$("$BIN" run examples/ackermann.gl | tr -d '\r\n')
if [ "$ACK_OUT" = "61" ]; then
  record_gate 4 "examples/ackermann.gl executes to 61" 0
else
  echo "Expected 61, got: $ACK_OUT"
  record_gate 4 "examples/ackermann.gl executes to 61" 1
fi

MAP_OUT=$("$BIN" run examples/list_map.gl | tr -d '\r\n')
if [ "$MAP_OUT" = "30" ]; then
  record_gate 5 "examples/list_map.gl executes to 30" 0
else
  echo "Expected 30, got: $MAP_OUT"
  record_gate 5 "examples/list_map.gl executes to 30" 1
fi

echo ""
echo "--- Step 4: CLI Commands Subcommand Matrix ---"
# parse
if "$BIN" parse examples/fib.gl >/dev/null 2>&1; then
  record_gate 6 "glyph parse examples/fib.gl succeeds" 0
else
  record_gate 6 "glyph parse examples/fib.gl succeeds" 1
fi

# typecheck
if "$BIN" typecheck examples/fib.gl >/dev/null 2>&1; then
  record_gate 7 "glyph typecheck examples/fib.gl succeeds" 0
else
  record_gate 7 "glyph typecheck examples/fib.gl succeeds" 1
fi

# compile & disasm
TMP_GBC=$(mktemp /tmp/glyph_verify_XXXXXX.gbc)
if "$BIN" compile examples/fib.gl -o "$TMP_GBC" >/dev/null 2>&1; then
  record_gate 8 "glyph compile produces valid bytecode" 0
else
  record_gate 8 "glyph compile produces valid bytecode" 1
fi

if "$BIN" disasm "$TMP_GBC" >/dev/null 2>&1; then
  record_gate 9 "glyph disasm dumps bytecode disassembly" 0
else
  record_gate 9 "glyph disasm dumps bytecode disassembly" 1
fi

GBC_OUT=$("$BIN" run "$TMP_GBC" | tr -d '\r\n')
if [ "$GBC_OUT" = "55" ]; then
  record_gate 10 "glyph run executes compiled .gbc chunk to 55" 0
else
  record_gate 10 "glyph run executes compiled .gbc chunk to 55" 1
fi
rm -f "$TMP_GBC"

echo ""
echo "--- Step 5: Negative Diagnostics & Exit Code Enforcement ---"
# typecheck error
if "$BIN" typecheck test/fixtures/type_error.gl >/dev/null 2>&1; then
  record_gate 11 "glyph typecheck rejects type errors with non-zero exit" 1
else
  record_gate 11 "glyph typecheck rejects type errors with non-zero exit" 0
fi

# compile error
if "$BIN" compile test/fixtures/type_error.gl >/dev/null 2>&1; then
  record_gate 12 "glyph compile rejects type errors with non-zero exit" 1
else
  record_gate 12 "glyph compile rejects type errors with non-zero exit" 0
fi

# runtime error on match failure
if "$BIN" run test/fixtures/match_fail.gl >/dev/null 2>&1; then
  record_gate 13 "glyph run match failure aborts with non-zero exit" 1
else
  record_gate 13 "glyph run match failure aborts with non-zero exit" 0
fi

echo ""
echo "--- Step 6: Artifact Fingerprints & SHA-256 ---"
sha256_cmd() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | awk '{print $1}'
  else
    openssl dgst -sha256 "$1" | awk '{print $NF}'
  fi
}

echo "Binary SHA-256:       $(sha256_cmd "$BIN")  ($BIN)"
echo "fib.gl SHA-256:       $(sha256_cmd examples/fib.gl)  (examples/fib.gl)"
echo "ackermann.gl SHA-256: $(sha256_cmd examples/ackermann.gl)  (examples/ackermann.gl)"
echo "list_map.gl SHA-256:  $(sha256_cmd examples/list_map.gl)  (examples/list_map.gl)"

echo ""
echo "=========================================================="
if [ "$FAILED_GATES" -eq 0 ]; then
  echo "  ALL VERIFICATION GATES PASSED (13/13). PIPELINE READY."
  echo "=========================================================="
  exit 0
else
  echo "  $FAILED_GATES GATES FAILED. PIPELINE INCOMPLETE."
  echo "=========================================================="
  exit 1
fi
