#!/usr/bin/env bash
# =============================================================================
# Dual Compliance Harness — Gate A + Gate B
# =============================================================================
# Runs both compliance gates sequentially:
#   Gate A — OAS composition fixtures (generic, no OpenAI dependency)
#   Gate B — OpenAI corpus (pinned OpenAI openapi.yaml)
#
# By default Gate B compiles the generated client (CMake + Make).  Use
# --skip-compile to skip compilation for faster iterations.
#
# Usage:
#   ./run-dual-gates.sh                     # Full run: both gates + compile
#   ./run-dual-gates.sh --skip-build        # Skip generator jar rebuild
#   ./run-dual-gates.sh --skip-compile      # Skip Gate B CMake/Make compile
#   ./run-dual-gates.sh --gate-b-only       # Run only Gate B (OpenAI corpus)
#   ./run-dual-gates.sh --gate-a-only       # Run only Gate A (OAS fixtures)
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
    local skip_compile=false
    local run_gate_a=true
    local run_gate_b=true

    for arg in "$@"; do
        case "$arg" in
            --skip-build)   skip_build=true ;;
            --skip-compile) skip_compile=true ;;
            --gate-a-only)  run_gate_b=false ;;
            --gate-b-only)  run_gate_a=false ;;
            --help|-h)
                echo "Usage: $0 [flags]"
                echo ""
                echo "Flags:"
                echo "  --skip-build       Skip generator jar Maven build"
                echo "  --skip-compile     Skip Gate B CMake/Make compilation"
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

    if [[ "${skip_compile}" == "true" ]]; then
        build_flags="${build_flags} --skip-compile"
    fi

    local overall_exit=0

    if [[ "${run_gate_a}" == "true" ]]; then
        header "Gate A — OAS Composition Fixtures"
        info_msg "Running: ${GATE_A} ${build_flags}"
        echo ""

        # Gate A doesn't have --skip-compile, strip that flag if present
        local gate_a_flags="${build_flags/ --skip-compile/}"
        gate_a_flags="${gate_a_flags/--skip-compile /}"

        if [[ -x "${GATE_A}" ]]; then
            # shellcheck disable=SC2086
            if ${GATE_A} ${gate_a_flags}; then
                pass_msg "Gate A passed"
            else
                fail_msg "Gate A failed"
                overall_exit=1
            fi
        else
            echo -e "  ${RED}ERROR${NC}  ${GATE_A} not found or not executable"
            overall_exit=1
        fi
    fi

    if [[ "${run_gate_b}" == "true" ]]; then
        header "Gate B — OpenAI Corpus"
        info_msg "Running: ${GATE_B} ${build_flags}"
        echo ""

        if [[ -x "${GATE_B}" ]]; then
            # shellcheck disable=SC2086
            if ${GATE_B} ${build_flags}; then
                pass_msg "Gate B passed"
            else
                fail_msg "Gate B failed"
                overall_exit=1
            fi
        else
            echo -e "  ${RED}ERROR${NC}  ${GATE_B} not found or not executable"
            overall_exit=1
        fi
    fi

    header "Dual Gate Summary"

    if [[ "${overall_exit}" -eq 0 ]]; then
        echo -e "  ${GREEN}PASS${NC}  All gates passed."
    else
        echo -e "  ${RED}FAIL${NC}  One or more gates failed."
    fi
    echo ""

    exit ${overall_exit}
}

main "$@"
