#!/usr/bin/env bash
# =============================================================================
# Dual Compliance Harness — Gate A + Gate B
# =============================================================================
# Runs both compliance gates sequentially:
#   Gate A — OAS composition fixtures (generic, no OpenAI dependency)
#   Gate B — OpenAI corpus (pinned OpenAI openapi.yaml)
#
# Usage:
#   ./run-dual-gates.sh               # Full run: both gates
#   ./run-dual-gates.sh --skip-build   # Skip generator jar rebuild
#   ./run-dual-gates.sh --gate-b-only  # Run only Gate B (OpenAI corpus)
#   ./run-dual-gates.sh --gate-a-only  # Run only Gate A (OAS fixtures)
#
# Exit codes:
#   0 — both gates pass
#   1 — one or both gates have failures
# =============================================================================

set -euo pipefail

IFS=$'\n\t'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

GATE_A="${SCRIPT_DIR}/oas-compliance/gate-a.sh"
GATE_B="${SCRIPT_DIR}/openai/compliance/harness.sh"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

pass_msg() { echo -e "  ${GREEN}PASS${NC}  $1"; }
fail_msg() { echo -e "  ${RED}FAIL${NC}  $1"; }
info_msg() { echo -e "  ${BLUE}INFO${NC}  $1"; }
header()   { echo -e "\n${BOLD}═══ $1 ═══${NC}\n"; }

main() {
    local skip_build=false
    local run_gate_a=true
    local run_gate_b=true

    for arg in "$@"; do
        case "$arg" in
            --skip-build)   skip_build=true ;;
            --gate-a-only)  run_gate_b=false ;;
            --gate-b-only)  run_gate_a=false ;;
            --help|-h)
                echo "Usage: $0 [flags]"
                echo ""
                echo "Flags:"
                echo "  --skip-build       Skip generator jar Maven build"
                echo "  --gate-a-only      Run only Gate A (OAS fixtures)"
                echo "  --gate-b-only      Run only Gate B (OpenAI corpus)"
                exit 0
                ;;
        esac
    done

    echo ""
    echo "╔══════════════════════════════════════════════════════════════════╗"
    echo "║        cpp-boost-beast-client — Dual Compliance Harness        ║"
    echo "╚══════════════════════════════════════════════════════════════════╝"
    echo ""

    local build_flags=""
    if [[ "${skip_build}" == "true" ]]; then
        build_flags="--skip-build"
    fi

    local overall_exit=0

    if [[ "${run_gate_a}" == "true" ]]; then
        header "Gate A — OAS Composition Fixtures"
        info_msg "Running: ${GATE_A} ${build_flags}"
        echo ""

        if [[ -x "${GATE_A}" ]]; then
            if ${GATE_A} ${build_flags}; then
                pass_msg "Gate A passed"
            else
                fail_msg "Gate A failed"
                overall_exit=1
            fi
        else
            echo "  ${RED}ERROR${NC}  ${GATE_A} not found or not executable"
            overall_exit=1
        fi
    fi

    if [[ "${run_gate_b}" == "true" ]]; then
        header "Gate B — OpenAI Corpus"
        info_msg "Running: ${GATE_B} ${build_flags}"
        echo ""

        if [[ -x "${GATE_B}" ]]; then
            if ${GATE_B} ${build_flags} --skip-compile; then
                pass_msg "Gate B passed"
            else
                fail_msg "Gate B failed"
                overall_exit=1
            fi
        else
            echo "  ${RED}ERROR${NC}  ${GATE_B} not found or not executable"
            overall_exit=1
        fi
    fi

    header "Dual Gate Summary"

    if [[ "${overall_exit}" -eq 0 ]]; then
        echo "  ${GREEN}PASS${NC}  All gates passed."
    else
        echo "  ${RED}FAIL${NC}  One or more gates failed."
    fi
    echo ""

    exit ${overall_exit}
}

main "$@"
