#!/usr/bin/env bash
# =============================================================================
# OAS Gate A — Composition Compliance Harness
# =============================================================================
# Generates from generic OAS 3.1 fixtures (no OpenAI dependency), inventories
# all composed schemas, checks for empty shells, and writes the TSV report.
#
# Usage:
#   ./gate-a.sh                   # Full run (build + generate + inventory)
#   ./gate-a.sh --skip-build      # Skip generator jar build
#   ./gate-a.sh inventory         # Inventory only (requires generated output)
#
# Exit codes:
#   0 — all checks pass
#   1 — empty-shell or type-mismatch failures
#   2 — generation / tooling failure
# =============================================================================

set -euo pipefail

IFS=$'\n\t'

# ---- Paths (all absolute, derived from script location) ----------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OPENAI_SDK_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

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

# Export for child processes
export OPENAI_SDK_DIR
export PROJECT_ROOT

FIXTURES="${SCRIPT_DIR}/fixtures.yaml"
OUTPUT_DIR="${OPENAI_SDK_DIR}/generated"
JAR_DIR="${PROJECT_ROOT}/modules/openapi-generator-cli/target"
JAR="${JAR_DIR}/openapi-generator-cli.jar"
MVNW="${PROJECT_ROOT}/mvnw"

# ---- Colors ------------------------------------------------------------------
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
# Step 1 — Build generator jar (with freshness check)
# =============================================================================
build_generator() {
    header "Step 1: Build generator CLI jar"

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
# Step 2 — Generate from fixtures
# =============================================================================
generate_client() {
    header "Step 2: Generate from OAS fixtures"

    if [[ ! -f "${JAR}" ]]; then
        echo "  ${RED}ERROR${NC}  Generator jar not found. Run without --skip-build or build manually."
        exit 2
    fi

    if [[ ! -f "${FIXTURES}" ]]; then
        echo "  ${RED}ERROR${NC}  Fixtures not found at ${FIXTURES}"
        exit 2
    fi

    info_msg "Generating cpp-boost-beast-client from ${FIXTURES}..."

    # Remove previous output to avoid stale artifacts
    rm -rf "${OUTPUT_DIR}"

    java -jar "${JAR}" generate \
        --generator-name cpp-boost-beast-client \
        --input-spec "${FIXTURES}" \
        --output "${OUTPUT_DIR}" \
        --additional-properties packageName=CppBoostBeastOasFixtures \
        --additional-properties apiPackage=api \
        --additional-properties modelPackage=model \
        2>&1

    info_msg "Generation complete: $(find "${OUTPUT_DIR}/model" -name '*.h' 2>/dev/null | wc -l) model headers"
}

# =============================================================================
# Step 3 — Inventory composed schemas
# =============================================================================
inventory_schemas() {
    header "Step 3: Inventory composed schemas"

    if [[ ! -d "${OUTPUT_DIR}" ]]; then
        echo "  ${RED}ERROR${NC}  Generated directory not found at ${OUTPUT_DIR}. Run generator first."
        exit 2
    fi

    local inventory_py
    inventory_py="$(mktemp)" || { echo "  ${RED}ERROR${NC}  mktemp failed"; exit 2; }
    cat > "${inventory_py}" << 'INVEOF'
#!/usr/bin/env python3
"""
Parse fixtures.yaml with PyYAML to find root-composed component schemas,
analyze emitted C++ headers, detect empty shells, and write the TSV.

Gate A — OAS composition fixtures (no OpenAI dependency).
"""
import os, re, sys, yaml

sdk_dir = os.environ.get("OPENAI_SDK_DIR", os.path.abspath("."))
output_dir = os.path.join(sdk_dir, "generated")
fixtures_file = os.path.join(sdk_dir, "oas-compliance", "fixtures.yaml")
compliance_dir = os.path.join(sdk_dir, "oas-compliance")
tsv_file = os.path.join(compliance_dir, "composed-schemas.tsv")

if not os.path.exists(output_dir):
    print(f"ERROR: Generated directory not found: {output_dir}")
    sys.exit(1)

# ---------------------------------------------------------------------------
# 1. Parse fixtures.yaml to find root-composed schemas
# ---------------------------------------------------------------------------
with open(fixtures_file, "r") as f:
    spec = yaml.safe_load(f)

schemas = spec.get("components", {}).get("schemas", {})
composed_schemas = []  # (schema_name, composition_type)

for name, schema in schemas.items():
    if not isinstance(schema, dict):
        continue
    # Skip past non-composition keys that commonly appear before oneOf/anyOf/allOf
    root = dict(schema)
    skip_keys = {"description", "title", "deprecated", "example", "default", "nullable"}
    for skip_key in skip_keys:
        root.pop(skip_key, None)
    for comp_key in ("oneOf", "anyOf", "allOf"):
        if comp_key in root:
            composed_schemas.append((name, comp_key))
            break

print(f"\nFound {len(composed_schemas)} root-composed schema definitions in fixtures.")

# ---------------------------------------------------------------------------
# 2. For each composed schema, analyze the generated C++ header
# ---------------------------------------------------------------------------
model_dir = os.path.join(output_dir, "model")

def analyze_model_header(model_name):
    """Analyze generated model header, return (cpp_type, is_empty_shell, member_count, notes)."""
    header_file = os.path.join(model_dir, f"{model_name}.h")
    if not os.path.exists(header_file):
        return ("NOT_FOUND", False, -1, "")

    with open(header_file) as f:
        content = f.read()

    # Type alias to boost::json::value?
    if "using " in content and "= boost::json::value" in content:
        # Check if it's a using alias
        for line in content.split("\n"):
            if "using " in line and "= boost::json::value" in line:
                alias_name = line.split("using ")[1].split(" =")[0].strip()
                return (f"boost::json::value (alias {alias_name})", False, 0, "json value fallback")

    # Type alias to another type?
    for line in content.split("\n"):
        stripped = line.strip()
        if stripped.startswith("using ") and "=" in stripped and ";" in stripped:
            alias_match = re.match(r'using\s+(\w+)\s*=\s*([^;]+);', stripped)
            if alias_match and not stripped.startswith("using namespace"):
                alias_target = alias_match.group(2).strip()
                # Exclude self-referential using
                if alias_target != model_name:
                    return (alias_target, False, 0, "type alias")

    # Check if it looks like an enum
    if re.search(r'enum\s+class\s+\w+', content):
        enum_match = re.search(r'enum\s+class\s+(\w+)', content)
        if enum_match:
            cpp_type = f"org::openapitools::client::model::{enum_match.group(1)}"
            return (cpp_type, False, 0, "enum")

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

    return (cpp_type, is_empty_shell, member_count, "")


# ---------------------------------------------------------------------------
# 3. Known codegen-error cases (negative tests)
# ---------------------------------------------------------------------------
# These are schemas where the correct behavior is for codegen to FAIL,
# e.g. allOf type conflicts.  If the header exists, that's a BUG
# (codegen silently produced invalid code).
known_codegen_error_schemas = {
    "AllOfScalarConflict": "allOf with conflicting types (string + int32) — codegen MUST error",
}

# ---------------------------------------------------------------------------
# 4. Build inventory
# ---------------------------------------------------------------------------
results = []
empty_shells = []
codegen_errors_unexpected = []  # produced code when it should have errored

for schema_name, comp_type in composed_schemas:
    expected_error = known_codegen_error_schemas.get(schema_name, None)

    cpp_type, is_empty, member_count, extra_notes = analyze_model_header(schema_name)

    if expected_error:
        # This schema SHOULD cause a codegen error.  If the header exists,
        # the generator silently produced bad code — that's a FAIL.
        if cpp_type != "NOT_FOUND":
            notes = f"NEGATIVE TEST FAIL: {expected_error}"
            result = "FAIL"
            codegen_errors_unexpected.append((schema_name, notes))
        else:
            notes = f"PASS (negative test): {expected_error}"
            result = "PASS"
            cpp_type = "codegen_error (expected)"
    elif cpp_type == "NOT_FOUND":
        notes = f"Header not found ({comp_type})"
        result = "NOT_FOUND"
    elif is_empty:
        notes = f"EMPTY SHELL ({comp_type}, {member_count} members)"
        result = "FAIL"
        empty_shells.append((schema_name, comp_type))
    else:
        member_str = f"{member_count} members" if member_count >= 0 else "type alias"
        notes = f"{comp_type}, {member_str}"
        if extra_notes:
            notes += f", {extra_notes}"
        result = "PASS"

    results.append((schema_name, result, cpp_type, notes))

# ---------------------------------------------------------------------------
# 5. Write TSV
# ---------------------------------------------------------------------------
os.makedirs(compliance_dir, exist_ok=True)

with open(tsv_file, "w") as f:
    f.write("case\tresult\tcpp_type\tnotes\n")
    for schema_name, result, cpp_type, notes in results:
        f.write(f"{schema_name}\t{result}\t{cpp_type}\t{notes}\n")

# ---------------------------------------------------------------------------
# 6. Report
# ---------------------------------------------------------------------------
pass_count = sum(1 for _, r, _, _ in results if r == "PASS")
fail_count = sum(1 for _, r, _, _ in results if r == "FAIL")
not_found_count = sum(1 for _, r, _, _ in results if r == "NOT_FOUND")

print(f"\nInventory: {pass_count} PASS, {fail_count} FAIL, {not_found_count} NOT_FOUND")
print(f"TSV: {tsv_file}")

errors = 0

if empty_shells:
    print(f"\n{'='*60}")
    print("EMPTY SHELL COMPOSED MODELS DETECTED:")
    for name, comp in empty_shells:
        print(f"  - {name} ({comp})")
    print(f"{'='*60}")
    print(f"Total: {len(empty_shells)} empty shell(s)\n")
    errors += len(empty_shells)

if codegen_errors_unexpected:
    print(f"\n{'='*60}")
    print("NEGATIVE TEST FAILURES (codegen should have errored but produced output):")
    for name, notes in codegen_errors_unexpected:
        print(f"  - {name}: {notes}")
    print(f"{'='*60}\n")
    errors += len(codegen_errors_unexpected)

if not_found_count > 0:
    not_found_names = [n for n, r, _, _ in results if r == "NOT_FOUND"]
    print(f"\n{'='*60}")
    print("NOT_FOUND SCHEMAS (header not generated):")
    for name in not_found_names:
        comp_type = next((c for sn, c in composed_schemas if sn == name), "?")
        print(f"  - {name} ({comp_type})")
    print(f"{'='*60}\n")
    errors += len(not_found_names)

print(f"__EMPTY_SHELL_COUNT__={len(empty_shells)}")
print(f"__PASS_COUNT__={pass_count}")
print(f"__FAIL_COUNT__={fail_count}")
print(f"__NOT_FOUND_COUNT__={not_found_count}")

if errors > 0:
    sys.exit(1)

INVEOF

    python3 "${inventory_py}"
    local py_rc=$?
    rm -f "${inventory_py}"
    return ${py_rc}
}

# =============================================================================
# Main execution
# =============================================================================
main() {
    local skip_build=false
    local only_inventory=false

    for arg in "$@"; do
        case "$arg" in
            --skip-build)    skip_build=true ;;
            inventory)       only_inventory=true ;;
            --help|-h)
                echo "Usage: $0 [flags] [command]"
                echo ""
                echo "Flags:"
                echo "  --skip-build       Skip generator jar Maven build"
                echo ""
                echo "Commands (run in isolation, skip all other steps):"
                echo "  inventory          Inventory composed schemas + empty-shell check"
                echo ""
                echo "Examples:"
                echo "  $0                           Full pipeline"
                echo "  $0 --skip-build              Generate + inventory"
                echo "  $0 inventory                 Schema inventory only"
                exit 0
                ;;
        esac
    done

    echo ""
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║        OAS Gate A — Composition Compliance Harness          ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""
    echo "  Fixtures: ${FIXTURES}"
    echo "  SDK dir:  ${OPENAI_SDK_DIR}"
    echo "  Project:  ${PROJECT_ROOT}"
    echo "  Jar:      ${JAR}"
    echo ""

    if [[ "${only_inventory}" == "true" ]]; then
        inventory_schemas
        exit $?
    fi

    if [[ "${skip_build}" == "false" ]]; then
        build_generator
    else
        header "Step 1: Build generator (skipped)"
    fi

    generate_client

    INVENTORY_EXIT=0
    inventory_schemas || INVENTORY_EXIT=$?

    header "Summary"
    echo ""
    echo "  TSV: ${SCRIPT_DIR}/composed-schemas.tsv"
    echo ""

    if [[ "${INVENTORY_EXIT}" -ne 0 ]]; then
        echo "  ${RED}FAIL${NC}  Composition checks failed."
        echo ""
        exit 1
    fi

    echo "  ${GREEN}PASS${NC}  All Gate A compliance checks passed."
    echo ""
}

main "$@"
