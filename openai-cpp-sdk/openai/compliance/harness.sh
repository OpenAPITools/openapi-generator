#!/usr/bin/env bash
# =============================================================================
# OpenAI Compliance Harness — Phase 0 (Gate B)
# =============================================================================
# Pins the spec version, generates the full OpenAI client, compiles it,
# inventories all composed schemas, runs golden encode/decode cases,
# and fails on any empty-shell composed model.
#
# This is Gate B of the Dual Compliance Harness.  Gate A lives in
# ../../oas-compliance/gate-a.sh and tests generic OAS composition
# fixtures independent of the OpenAI spec.
#
# Usage:
#   ./harness.sh                    # Full run (build + generate + compile + verify)
#   ./harness.sh --skip-build       # Skip generator jar build
#   ./harness.sh --skip-generate    # Skip code generation
#   ./harness.sh --skip-compile     # Skip compilation
#   ./harness.sh --run-gate-a      # Run Gate A first, then Gate B
#   ./harness.sh inventory          # Inventory + shell check only (no build/gen/compile)
#   ./harness.sh golden             # Golden case encode/decode only
#
# Exit codes:
#   0 — all checks pass
#   1 — inventory or golden case failure
#   2 — generation / tooling failure
#   3 — compilation failure
#   4 — spec pin mismatch
# =============================================================================

set -euo pipefail

# Upstream URL for the pinned OpenAI API definition.
# Downloaded only when openapi.yaml does not exist locally.
SPEC_URL="https://platform.openai.com/docs/static/api-definition.yaml"
CURL_TIMEOUT=60
IFS=$'\n\t'

# ---- Paths (all absolute, derived from script location) ----------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OPENAI_SDK_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Determine the real project root (where pom.xml / mvnw lives)
if [[ -f "${OPENAI_SDK_DIR}/../mvnw" ]]; then
    PROJECT_ROOT="$(cd "${OPENAI_SDK_DIR}/.." && pwd)"
else
    CANDIDATE="${SCRIPT_DIR}"
    while [[ "${CANDIDATE}" != "/" ]]; do
        if [[ -f "${CANDIDATE}/mvnw" ]]; then
            PROJECT_ROOT="${CANDIDATE}"
            break
        fi
        CANDIDATE="$(dirname "${CANDIDATE}")"
    done
fi

# Export for all child processes (Python scripts, etc.)
export OPENAI_SDK_DIR
export PROJECT_ROOT

SPEC="${OPENAI_SDK_DIR}/openapi.yaml"
GENERATED_DIR="${OPENAI_SDK_DIR}/generated"
BUILD_DIR="${OPENAI_SDK_DIR}/build-generated"
COMPLIANCE_DIR="${OPENAI_SDK_DIR}/openai/compliance"
JAR_DIR="${PROJECT_ROOT}/modules/openapi-generator-cli/target"
JAR="${JAR_DIR}/openapi-generator-cli.jar"
MVNW="${PROJECT_ROOT}/mvnw"
MANIFEST="${COMPLIANCE_DIR}/MANIFEST.md"

# ---- Colors -----------------------------------------------------------------
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

pass_msg() { echo -e "  ${GREEN}PASS${NC}  $1"; }
fail_msg() { echo -e "  ${RED}FAIL${NC}  $1"; }
info_msg() { echo -e "  ${BLUE}INFO${NC}  $1"; }
warn_msg() { echo -e "  ${YELLOW}WARN${NC}  $1"; }
header()   { echo -e "\n${BOLD}═══ $1 ═══${NC}\n"; }

# =============================================================================
# Step 0a — Spec fetch (if missing)
# =============================================================================
fetch_spec() {
    if [[ -f "${SPEC}" ]]; then
        return 0
    fi

    header "Step 0a: Fetch spec from upstream"

    if ! command -v curl &>/dev/null; then
        echo "  ${RED}ERROR${NC}  curl is required to fetch the spec"
        exit 4
    fi

    info_msg "Downloading from ${SPEC_URL} ..."
    mkdir -p "$(dirname "${SPEC}")"
    curl -sSL --max-time "${CURL_TIMEOUT}" -o "${SPEC}.tmp" "${SPEC_URL}" 2>&1

    if [[ ! -f "${SPEC}.tmp" ]] || [[ ! -s "${SPEC}.tmp" ]]; then
        echo "  ${RED}ERROR${NC}  Download failed or returned empty file"
        rm -f "${SPEC}.tmp"
        exit 4
    fi

    mv "${SPEC}.tmp" "${SPEC}"
    info_msg "Downloaded $(wc -c < "${SPEC}") bytes to ${SPEC}"

    # Verify hash against manifest immediately after download
    local manifest_hash
    manifest_hash=$(awk -F'[|]' '/SHA-256/ {gsub(/[`| ]/, "", $3); print $3}' "${MANIFEST}" 2>/dev/null || echo "")
    if [[ -z "${manifest_hash}" ]]; then
        echo "  ${YELLOW}WARN${NC}  Cannot verify hash — MANIFEST missing or unreadable"
        return 0
    fi

    local actual_hash
    actual_hash="$(shasum -a 256 "${SPEC}" | cut -d' ' -f1 || true)"

    if [[ "${actual_hash}" != "${manifest_hash}" ]]; then
        echo "  ${RED}FAIL${NC}  Downloaded spec hash does not match MANIFEST"
        echo "         Expected: ${manifest_hash}"
        echo "         Actual:   ${actual_hash}"
        echo "  The upstream spec has changed. Update MANIFEST.md to re-pin."
        rm -f "${SPEC}"
        exit 4
    fi

    pass_msg "Downloaded spec matches pinned hash"
}

# =============================================================================
# Step 0b — Spec pin verification
# =============================================================================
verify_pin() {
    header "Step 0: Verify spec pin"

    if [[ ! -f "${MANIFEST}" ]]; then
        echo "  ${RED}ERROR${NC}  MANIFEST.md not found at ${MANIFEST}"
        exit 4
    fi

    if [[ ! -f "${SPEC}" ]]; then
        echo "  ${RED}ERROR${NC}  Spec not found at ${SPEC}"
        exit 4
    fi

    # Extract expected values from MANIFEST.md (markdown table: | field | value |)
    local expected_hash expected_version expected_title
    expected_hash=$(awk -F'[|]' '/SHA-256/ {gsub(/[`| ]/, "", $3); print $3}' "${MANIFEST}")
    expected_version=$(awk -F'[|]' '/API version/ {gsub(/[`| ]/, "", $3); print $3}' "${MANIFEST}")
    expected_title=$(awk -F'[|]' '/API title/ {gsub(/^[[:space:]]+|[[:space:]]+$/, "", $3); print $3}' "${MANIFEST}")

    # Compute actual hash
    local actual_hash
    actual_hash="$(shasum -a 256 "${SPEC}" | cut -d' ' -f1 || true)"

    # Extract actual version/title from YAML header (nested under "info:")
    local actual_version actual_title
    actual_version="$(grep -E '^\s+version:' "${SPEC}" | head -1 | sed 's/.*version: *//;s/"//g;s/ //g' || true)"
    actual_title="$(grep -E '^\s+title:' "${SPEC}" | head -1 | sed 's/.*title: *//;s/"//g' || true)"

    local pin_ok=true

    if [[ "${actual_hash}" != "${expected_hash}" ]]; then
        echo "  ${RED}FAIL${NC}  Spec hash mismatch"
        echo "         Expected: ${expected_hash}"
        echo "         Actual:   ${actual_hash}"
        pin_ok=false
    fi

    if [[ "${actual_version}" != "${expected_version}" ]]; then
        echo "  ${RED}FAIL${NC}  API version mismatch"
        echo "         Expected: ${expected_version}"
        echo "         Actual:   ${actual_version}"
        pin_ok=false
    fi

    if [[ "${actual_title}" != "${expected_title}" ]]; then
        echo "  ${RED}FAIL${NC}  API title mismatch"
        echo "         Expected: ${expected_title}"
        echo "         Actual:   ${actual_title}"
        pin_ok=false
    fi

    if [[ "${pin_ok}" == "true" ]]; then
        pass_msg "Spec pin verified: ${expected_title} v${expected_version}"
    else
        echo ""
        echo "  ${RED}ERROR${NC}  Spec has changed since MANIFEST was written."
        echo "  Regenerate and update MANIFEST.md, then re-verify."
        exit 4
    fi
}

# =============================================================================
# Step 1 — Build generator jar (with freshness check)
# =============================================================================
build_generator() {
    header "Step 1: Build generator CLI jar"

    # Check jar freshness: rebuild if any .java source is newer than the jar
    if [[ -f "${JAR}" ]]; then
        local need_rebuild=false

        for src_dir in "${PROJECT_ROOT}/modules/openapi-generator/src" \
                       "${PROJECT_ROOT}/modules/openapi-generator-cli/src" \
                       "${PROJECT_ROOT}/modules/openapi-generator-core/src"; do
            if [[ -d "${src_dir}" ]]; then
                local newest
                newest=$(find "${src_dir}" -name '*.java' -newer "${JAR}" -print -quit 2>/dev/null || true)
                if [[ -n "${newest}" ]]; then
                    need_rebuild=true
                    break
                fi
            fi
        done

        if [[ "${need_rebuild}" == "false" ]]; then
            info_msg "Jar is up to date: ${JAR}"
            return 0
        fi
        info_msg "Generator sources newer than jar — rebuilding..."
    fi

    if [[ ! -f "${MVNW}" ]]; then
        echo "  ${RED}ERROR${NC}  mvnw not found at ${MVNW}"
        exit 2
    fi

    pushd "${PROJECT_ROOT}" >/dev/null
    "${MVNW}" -pl modules/openapi-generator-cli -am compile package -DskipTests -Dmaven.test.skip=true -q 2>&1
    popd >/dev/null

    if [[ ! -f "${JAR}" ]]; then
        echo "  ${RED}ERROR${NC}  Jar was not created at ${JAR}"
        exit 2
    fi
    info_msg "Generator jar ready: ${JAR}"
}

# =============================================================================
# Step 2 — Generate OpenAI client
# =============================================================================
generate_client() {
    header "Step 2: Generate OpenAI client"

    if [[ ! -f "${JAR}" ]]; then
        echo "  ${RED}ERROR${NC}  Generator jar not found. Run without --skip-build or build manually."
        exit 2
    fi

    if [[ ! -f "${SPEC}" ]]; then
        echo "  ${RED}ERROR${NC}  Spec not found at ${SPEC}"
        exit 2
    fi

    info_msg "Generating cpp-boost-beast-client from ${SPEC}..."

    java -jar "${JAR}" generate \
        --generator-name cpp-boost-beast-client \
        --input-spec "${SPEC}" \
        --output "${GENERATED_DIR}" \
        --additional-properties packageName=CppBoostBeastOpenAPIClient \
        --additional-properties apiPackage=api \
        --additional-properties modelPackage=model \
        2>&1

    info_msg "Generation complete: $(find "${GENERATED_DIR}/model" -name '*.h' | wc -l) model headers"
}

# =============================================================================
# Step 3 — Compile generated code
# =============================================================================
compile_client() {
    header "Step 3: Compile generated client"

    if [[ ! -d "${GENERATED_DIR}" ]]; then
        echo "  ${RED}ERROR${NC}  Generated directory not found. Run generator first."
        exit 3
    fi

    mkdir -p "${BUILD_DIR}"
    pushd "${BUILD_DIR}" >/dev/null

    info_msg "Configuring with CMake..."
    cmake "${GENERATED_DIR}" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-Werror" 2>&1

    info_msg "Building with Make..."
    make -j"$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)" 2>&1

    popd >/dev/null
    info_msg "Compilation complete"
}

# =============================================================================
# Step 4 — Inventory composed schemas
# =============================================================================
inventory_schemas() {
    header "Step 4: Inventory composed schemas"

    local inventory_py
    inventory_py="$(mktemp)" || { echo "  ${RED}ERROR${NC}  mktemp failed"; exit 2; }
    cat > "${inventory_py}" << 'INVEOF'
#!/usr/bin/env python3
"""
Parse openapi.yaml with PyYAML to find root-composed component schemas,
analyze emitted C++ headers, enforce expected-types.yaml, and detect empty shells.
"""
import os, re, sys, yaml

sdk_dir = os.environ.get("OPENAI_SDK_DIR", os.path.abspath("."))
generated_dir = os.path.join(sdk_dir, "generated")
spec_file = os.path.join(sdk_dir, "openapi.yaml")
compliance_dir = os.path.join(sdk_dir, "openai", "compliance")
inventory_file = os.path.join(compliance_dir, "schema-inventory.tsv")
expected_types_file = os.path.join(compliance_dir, "expected-types.yaml")
deferred_allowlist_file = os.path.join(compliance_dir, ".not-found-allowlist")

if not os.path.exists(generated_dir):
    print(f"ERROR: Generated directory not found: {generated_dir}")
    sys.exit(1)

# ---------------------------------------------------------------------------
# 1. Parse spec with PyYAML to find root-composed schemas
# ---------------------------------------------------------------------------
with open(spec_file, "r") as f:
    spec = yaml.safe_load(f)

schemas = spec.get("components", {}).get("schemas", {})
composed_schemas = []  # (schema_name, composition_type)

for name, schema in schemas.items():
    if not isinstance(schema, dict):
        continue
    # Skip past non-composition keys that commonly appear before oneOf/anyOf/allOf
    root = dict(schema)  # shallow copy
    skip_keys = {"description", "title", "deprecated", "example", "default", "nullable"}
    for skip_key in skip_keys:
        root.pop(skip_key, None)
    # If after removing skip-keys there's exactly one composition key, it's root-composed
    for comp_key in ("oneOf", "anyOf", "allOf"):
        if comp_key in root:
            composed_schemas.append((name, comp_key))
            break

print(f"\nFound {len(composed_schemas)} root-composed schema definitions in spec.")

# ---------------------------------------------------------------------------
# 2. Load expected-types manifest
# ---------------------------------------------------------------------------
expected_types = {}
if os.path.exists(expected_types_file):
    with open(expected_types_file) as f:
        et_raw = yaml.safe_load(f) or {}
    for k, v in et_raw.items():
        expected_types[k] = str(v)

print(f"Loaded {len(expected_types)} expected-type entries.")

# ---------------------------------------------------------------------------
# 3. Load NOT_FOUND deferred allowlist
# ---------------------------------------------------------------------------
deferred_schemas = set()
if os.path.exists(deferred_allowlist_file):
    with open(deferred_allowlist_file) as f:
        for line in f:
            stripped = line.strip()
            if stripped and not stripped.startswith("#"):
                deferred_schemas.add(stripped)

# ---------------------------------------------------------------------------
# 4. For each composed schema, analyze the generated C++ header
# ---------------------------------------------------------------------------
model_dir = os.path.join(generated_dir, "model")

def analyze_model_header(model_name):
    """Analyze generated model header, return (cpp_type, is_empty_shell, member_count)."""
    header_file = os.path.join(model_dir, f"{model_name}.h")
    if not os.path.exists(header_file):
        return ("NOT_FOUND", False, -1)

    with open(header_file) as f:
        content = f.read()

    # Type alias?
    if "using " in content and "= boost::json::value" in content:
        return ("boost::json::value", False, 0)

    # Count actual member variables (exclude *IsSet booleans)
    member_decls = re.findall(r"m_\w+(?:\s*=\s*[^;]+)?;", content)
    actual_members = [m for m in member_decls if not re.match(r"m_\w+IsSet\b", m)]
    member_count = len(actual_members)

    # Match class declaration at line start
    class_match = re.search(r"^class\s+(\w+)", content, re.MULTILINE)
    if class_match:
        cpp_type = f"org::openapitools::client::model::{class_match.group(1)}"
    else:
        cpp_type = model_name

    has_to_json = "toJsonValue()" in content or "toJsonObject_internal()" in content
    has_from_json = "fromJsonValue(" in content or "fromJsonObject_internal(" in content
    is_empty_shell = has_to_json and has_from_json and member_count == 0 and class_match is not None

    return (cpp_type, is_empty_shell, member_count)

# ---------------------------------------------------------------------------
# 5. Build inventory
# ---------------------------------------------------------------------------
results = []
empty_shells = []
expected_mismatches = []
not_found_unless_deferred = []

for schema_name, comp_type in composed_schemas:
    cpp_type, is_empty, member_count = analyze_model_header(schema_name)

    if cpp_type == "NOT_FOUND":
        if schema_name in deferred_schemas:
            notes = f"Deferred (on allowlist)"
            result = "DEFERRED"
        else:
            notes = f"Header not found — use .not-found-allowlist to defer"
            result = "NOT_FOUND"
            not_found_unless_deferred.append(schema_name)
    elif is_empty:
        notes = f"EMPTY SHELL ({comp_type}, {member_count} members)"
        result = "FAIL"
        empty_shells.append((schema_name, comp_type))
    else:
        notes = f"{comp_type}, {member_count} members"
        result = "PASS"

    # Check against expected-types manifest
    if schema_name in expected_types:
        expected = expected_types[schema_name]
        # Normalize both for comparison
        actual_normalized = cpp_type.replace("org::openapitools::client::model::", "")
        expected_normalized = expected.replace("org::openapitools::client::model::", "")
        if actual_normalized != expected_normalized and expected_normalized != "NOT_FOUND":
            # Allow namespace-qualified vs. short
            if cpp_type != expected and actual_normalized != expected_normalized:
                expected_mismatches.append((schema_name, expected, cpp_type))
                notes += f" | EXPECTED {expected}"
                result = "FAIL"

    results.append((schema_name, result, cpp_type, notes))

# ---------------------------------------------------------------------------
# 6. Write TSV
# ---------------------------------------------------------------------------
os.makedirs(compliance_dir, exist_ok=True)

with open(inventory_file, "w") as f:
    f.write("schema\tresult\tcpp_type\tnotes\n")
    for schema_name, result, cpp_type, notes in results:
        f.write(f"{schema_name}\t{result}\t{cpp_type}\t{notes}\n")

# ---------------------------------------------------------------------------
# 7. Report
# ---------------------------------------------------------------------------
pass_count = sum(1 for _, r, _, _ in results if r == "PASS")
fail_count = sum(1 for _, r, _, _ in results if r == "FAIL")
not_found_count = sum(1 for _, r, _, _ in results if r == "NOT_FOUND")
deferred_count = sum(1 for _, r, _, _ in results if r == "DEFERRED")

print(f"\nInventory: {pass_count} PASS, {fail_count} FAIL, {not_found_count} NOT_FOUND, {deferred_count} deferred")
print(f"TSV: {inventory_file}")

errors = 0

if empty_shells:
    print(f"\n{'='*60}")
    print("EMPTY SHELL COMPOSED MODELS DETECTED:")
    for name, comp in empty_shells:
        print(f"  - {name} ({comp})")
    print(f"{'='*60}")
    print(f"Total: {len(empty_shells)} empty shell(s)\n")
    errors += len(empty_shells)

if expected_mismatches:
    print(f"\n{'='*60}")
    print("EXPECTED-TYPE MISMATCHES:")
    for name, expected, actual in expected_mismatches:
        print(f"  - {name}: expected {expected}, got {actual}")
    print(f"{'='*60}\n")
    errors += len(expected_mismatches)

if not_found_unless_deferred:
    print(f"\n{'='*60}")
    print("NOT_FOUND SCHEMAS (not in .not-found-allowlist):")
    for name in not_found_unless_deferred:
        print(f"  - {name}")
    print(f"{'='*60}")
    print("Add to .not-found-allowlist to defer, or fix the header generation.\n")
    errors += len(not_found_unless_deferred)

print(f"__EMPTY_SHELL_COUNT__={len(empty_shells)}")
print(f"__PASS_COUNT__={pass_count}")
print(f"__FAIL_COUNT__={fail_count}")
print(f"__NOT_FOUND_COUNT__={not_found_count}")
print(f"__MISMATCH_COUNT__={len(expected_mismatches)}")

if errors > 0:
    sys.exit(1)

INVEOF

    python3 "${inventory_py}"
    local py_rc=$?
    rm -f "${inventory_py}"
    return ${py_rc}
}

# =============================================================================
# Step 5 — Golden encode/decode cases
# =============================================================================
run_golden_cases() {
    header "Step 5: Golden case encode/decode"

    local golden_file="${COMPLIANCE_DIR}/golden-cases.json"
    if [[ ! -f "${golden_file}" ]]; then
        echo "  ${RED}ERROR${NC}  Golden cases file not found: ${golden_file}"
        return 1
    fi

    local runner_script="${COMPLIANCE_DIR}/golden_runner_gen.py"
    if [[ ! -f "${runner_script}" ]]; then
        echo "  ${RED}ERROR${NC}  Golden runner generator not found: ${runner_script}"
        return 1
    fi

    python3 "${runner_script}"
}

# =============================================================================
# Main execution
# =============================================================================
main() {
    local skip_build=false
    local skip_generate=false
    local skip_compile=false
    local only_inventory=false
    local only_golden=false
    local run_gate_a=false

    # Parse arguments
    for arg in "$@"; do
        case "$arg" in
            --skip-build)    skip_build=true ;;
            --skip-generate) skip_generate=true ;;
            --skip-compile)  skip_compile=true ;;
            --run-gate-a)    run_gate_a=true ;;
            inventory)       only_inventory=true ;;
            golden)          only_golden=true ;;
            --help|-h)
                echo "Usage: $0 [flags] [command]"
                echo ""
                echo "Flags:"
                echo "  --skip-build       Skip generator jar Maven build"
                echo "  --skip-generate    Skip code generation from spec"
                echo "  --skip-compile     Skip CMake + Make compilation"
                echo "  --run-gate-a       Run Gate A (OAS fixtures) before Gate B"
                echo ""
                echo "Commands (run in isolation, skip all other steps):"
                echo "  inventory          Inventory composed schemas + empty-shell check"
                echo "  golden             Golden case encode/decode round-trip test"
                echo ""
                echo "Examples:"
                echo "  $0                              Full pipeline"
                echo "  $0 --skip-build --skip-compile  Generate + inventory + golden"
                echo "  $0 inventory                    Schema inventory only"
                echo "  $0 golden                       Golden cases only"
                exit 0
                ;;
        esac
    done

    echo ""
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║        OpenAI C++ SDK — Compliance Harness                  ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "  Spec:     ${SPEC}"
    echo "  SDK dir:  ${OPENAI_SDK_DIR}"
    echo "  Project:  ${PROJECT_ROOT}"
    echo "  Jar:      ${JAR}"
    echo ""

    if [[ "${only_golden}" == "true" ]]; then
        fetch_spec
        verify_pin
        run_golden_cases
        exit $?
    fi

    if [[ "${only_inventory}" == "true" ]]; then
        fetch_spec
        verify_pin
        inventory_schemas
        exit $?
    fi

    # Run Gate A first if requested
    if [[ "${run_gate_a}" == "true" ]]; then
        header "Gate A — OAS Composition Fixtures"
        local gate_a_script="${OPENAI_SDK_DIR}/oas-compliance/gate-a.sh"
        if [[ -x "${gate_a_script}" ]]; then
            local gate_a_build_flags=""
            if [[ "${skip_build}" == "true" ]]; then
                gate_a_build_flags="--skip-build"
            fi
            if ! ${gate_a_script} ${gate_a_build_flags}; then
                echo ""
                echo "  ${RED}FAIL${NC}  Gate A failed — aborting Gate B"
                exit 1
            fi
            pass_msg "Gate A passed"
        else
            echo "  ${YELLOW}WARN${NC}  Gate A script not found at ${gate_a_script}"
        fi
    fi

    # Full pipeline
    fetch_spec
    verify_pin

    if [[ "${skip_build}" == "false" ]]; then
        build_generator
    else
        header "Step 1: Build generator (skipped)"
    fi

    if [[ "${skip_generate}" == "false" ]]; then
        generate_client
    else
        header "Step 2: Generate OpenAI client (skipped)"
    fi

    if [[ "${skip_compile}" == "false" ]]; then
        compile_client
    else
        header "Step 3: Compile (skipped)"
    fi

    INVENTORY_EXIT=0
    inventory_schemas || INVENTORY_EXIT=$?

    GOLDEN_EXIT=0
    run_golden_cases || GOLDEN_EXIT=$?

    # Extract counts from inventory TSV
    local shell_count=0
    if [[ -f "${COMPLIANCE_DIR}/schema-inventory.tsv" ]]; then
        shell_count=$(grep -c $'\tFAIL\t' "${COMPLIANCE_DIR}/schema-inventory.tsv" 2>/dev/null || echo 0)
    fi

    # Final summary
    header "Summary"
    echo ""
    echo "  Inventory: ${COMPLIANCE_DIR}/schema-inventory.tsv"
    echo "  Expected types: ${COMPLIANCE_DIR}/expected-types.yaml"
    echo "  Golden cases: ${COMPLIANCE_DIR}/golden-cases.json"
    echo ""

    if [[ "${INVENTORY_EXIT}" -ne 0 ]]; then
        echo "  ${RED}FAIL${NC}  ${shell_count} empty-shell / mismatch / not-found issues."
        echo ""
        echo "  Empty-shell models have oneOf/anyOf/allOf but generate as"
        echo "  wrappers with toJsonValue()/fromJsonValue() and no members."
        echo "  These must be fixed before proceeding."
        exit 1
    fi

    if [[ "${GOLDEN_EXIT}" -ne 0 ]]; then
        echo "  ${RED}FAIL${NC}  Golden case round-trip tests failed."
        exit 1
    fi

    echo "  ${GREEN}PASS${NC}  All compliance checks passed."
    echo ""
}

main "$@"
