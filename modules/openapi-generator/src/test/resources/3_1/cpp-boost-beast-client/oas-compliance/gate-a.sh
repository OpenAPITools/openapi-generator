#!/usr/bin/env bash
# =============================================================================
# OAS Gate A — Composition Compliance Harness
# =============================================================================
# Generates from generic OAS 3.1 composition fixtures, inventories all composed
# schemas, compares against expected-types.yaml, and reports failures.
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

# Walk up to find the project root (where mvnw lives)
CANDIDATE="${SCRIPT_DIR}"
PROJECT_ROOT=""
while [[ "${CANDIDATE}" != "/" ]]; do
    if [[ -f "${CANDIDATE}/mvnw" ]]; then
        PROJECT_ROOT="${CANDIDATE}"
        break
    fi
    CANDIDATE="$(dirname "${CANDIDATE}")"
done

if [[ -z "${PROJECT_ROOT}" ]]; then
    echo "ERROR: Could not find project root (mvnw)" >&2
    exit 2
fi

FIXTURES="${SCRIPT_DIR}/fixtures.yaml"
NEGATIVE_FIXTURES="${SCRIPT_DIR}/fixtures-negative.yaml"
OUTPUT_DIR="${PROJECT_ROOT}/generated-oas-client"
JAR_DIR="${PROJECT_ROOT}/modules/openapi-generator-cli/target"
JAR="${JAR_DIR}/openapi-generator-cli.jar"
MVNW="${PROJECT_ROOT}/mvnw"

export PROJECT_ROOT

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
        echo -e "  ${RED}ERROR${NC}  mvnw not found at ${MVNW}"
        exit 2
    fi

    pushd "${PROJECT_ROOT}" >/dev/null
    "${MVNW}" -pl modules/openapi-generator-cli -am compile package -DskipTests -Dmaven.test.skip=true -q 2>&1
    popd >/dev/null

    if [[ ! -f "${JAR}" ]]; then
        echo -e "  ${RED}ERROR${NC}  Jar was not created at ${JAR}"
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
        echo -e "  ${RED}ERROR${NC}  Generator jar not found. Run without --skip-build or build manually."
        exit 2
    fi

    if [[ ! -f "${FIXTURES}" ]]; then
        echo -e "  ${RED}ERROR${NC}  Fixtures not found at ${FIXTURES}"
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
# Step 2b — Generate from negative fixtures
# =============================================================================
generate_negative_fixtures() {
    header "Step 2b: Generate from negative fixtures"

    if [[ ! -f "${JAR}" ]]; then
        echo -e "  ${RED}ERROR${NC}  Generator jar not found."
        exit 2
    fi

    if [[ ! -f "${NEGATIVE_FIXTURES}" ]]; then
        warn_msg "Negative fixtures not found at ${NEGATIVE_FIXTURES} — skipping"
        return 0
    fi

    local negative_output_dir="${PROJECT_ROOT}/generated-oas-client-negative"

    info_msg "Generating from negative fixtures (expecting codegen issues)..."

    rm -rf "${negative_output_dir}"

    set +e
    java -jar "${JAR}" generate \
        --generator-name cpp-boost-beast-client \
        --input-spec "${NEGATIVE_FIXTURES}" \
        --output "${negative_output_dir}" \
        --additional-properties packageName=CppBoostBeastNegativeFixtures \
        --additional-properties apiPackage=api \
        --additional-properties modelPackage=model \
        2>&1
    local gen_exit=$?
    set -e

    local negative_tsv="${SCRIPT_DIR}/composed-schemas.tsv"
    local found_issues=0

    # Parse negative fixtures to find all root-composed schemas
    local parse_py
    parse_py="$(mktemp)" || { echo -e "  ${RED}ERROR${NC}  mktemp failed"; exit 2; }
    cat > "${parse_py}" << 'NEGEOF'
#!/usr/bin/env python3
"""Parse negative fixtures and check: generate exit code > 0 OR header absent."""
import os, re, sys, yaml

negative_fixtures = os.environ.get("NEGATIVE_FIXTURES_PATH", "")
negative_output_dir = os.environ.get("NEGATIVE_OUTPUT_DIR", "")
gen_exit = int(os.environ.get("GEN_EXIT", "0"))

if not negative_fixtures or not os.path.exists(negative_fixtures):
    sys.exit(0)

with open(negative_fixtures) as f:
    spec = yaml.safe_load(f)

schemas = spec.get("components", {}).get("schemas", {})
found_failures = 0

for name, schema in schemas.items():
    if not isinstance(schema, dict):
        continue
    root = dict(schema)
    for skip_key in ("description", "title", "deprecated", "example", "default", "nullable"):
        root.pop(skip_key, None)
    is_composed = any(k in root for k in ("oneOf", "anyOf", "allOf"))
    if not is_composed:
        continue

    header_file = os.path.join(negative_output_dir, "model", f"{name}.h")
    header_exists = os.path.exists(header_file)

    if gen_exit != 0 or not header_exists:
        print(f"  PASS  {name}: codegen {'exited with ' + str(gen_exit) if gen_exit != 0 else 'did not produce header'}")
    else:
        cpp_type = "?"
        try:
            with open(header_file) as hf:
                content = hf.read()
            if "using " in content and "= " in content and ";" in content:
                for line in content.split("\n"):
                    stripped = line.strip()
                    if stripped.startswith("using ") and "=" in stripped and ";" in stripped:
                        m = re.match(r'using\s+\w+\s*=\s*([^;]+);', stripped)
                        if m:
                            cpp_type = m.group(1).strip()
                            break
        except Exception:
            pass
        print(f"  FAIL  {name}: codegen should have errored but produced type {cpp_type}")
        found_failures += 1

if found_failures > 0:
    sys.exit(1)

NEGEOF

    export NEGATIVE_FIXTURES_PATH="${NEGATIVE_FIXTURES}"
    export NEGATIVE_OUTPUT_DIR="${negative_output_dir}"
    export GEN_EXIT="${gen_exit}"

    local neg_exit=0
    python3 "${parse_py}" || neg_exit=$?
    rm -f "${parse_py}"

    # Upsert negative test result in the TSV
    local negative_result_line=""
    if [[ "${gen_exit}" -ne 0 ]]; then
        negative_result_line="AllOfScalarConflict\tPASS\tcodegen_error\tallOf conflict (negative test, codegen errored)"
    elif [[ -d "${negative_output_dir}/model" ]] && [[ "$(ls "${negative_output_dir}/model"/*.h 2>/dev/null | wc -l)" -eq 0 ]]; then
        negative_result_line="AllOfScalarConflict\tPASS\tcodegen_error\tallOf conflict (negative test, header not produced)"
    else
        negative_result_line="AllOfScalarConflict\tFAIL\tstd::string\tallOf conflict silently produced code (negative test)"
        neg_exit=1
    fi

    if grep -q "^AllOfScalarConflict" "${negative_tsv}" 2>/dev/null; then
        local tmp_tsv
        tmp_tsv="$(mktemp)"
        sed "s/^AllOfScalarConflict.*/${negative_result_line}/" "${negative_tsv}" > "${tmp_tsv}"
        mv "${tmp_tsv}" "${negative_tsv}"
    else
        echo -e "${negative_result_line}" >> "${negative_tsv}"
    fi

    rm -rf "${negative_output_dir}"

    if [[ "${neg_exit}" -ne 0 ]]; then
        return ${neg_exit}
    fi

    info_msg "Negative fixture checks: AllOfScalarConflict"
}

# =============================================================================
# Step 3 — Inventory composed schemas
# =============================================================================
inventory_schemas() {
    header "Step 3: Inventory composed schemas"

    if [[ ! -d "${OUTPUT_DIR}" ]]; then
        echo -e "  ${RED}ERROR${NC}  Generated directory not found at ${OUTPUT_DIR}. Run generator first."
        exit 2
    fi

    local inventory_py
    inventory_py="$(mktemp)" || { echo -e "  ${RED}ERROR${NC}  mktemp failed"; exit 2; }
    cat > "${inventory_py}" << 'INVEOF'
#!/usr/bin/env python3
"""
Parse fixtures.yaml with PyYAML to find root-composed component schemas,
analyze emitted C++ headers, detect empty shells, and write the TSV.
"""
import os, re, sys, yaml

compliance_dir = os.environ.get("SCRIPT_DIR", os.path.abspath("."))
output_dir = os.environ.get("OUTPUT_DIR", os.path.join(os.path.dirname(compliance_dir), "generated"))
fixtures_file = os.path.join(compliance_dir, "fixtures.yaml")
tsv_file = os.path.join(compliance_dir, "composed-schemas.tsv")
expected_types_file = os.path.join(compliance_dir, "expected-types.yaml")
deferred_allowlist_file = os.path.join(compliance_dir, ".not-found-allowlist")

if not os.path.exists(output_dir):
    print(f"ERROR: Generated directory not found: {output_dir}")
    sys.exit(1)

# 1. Parse fixtures.yaml to find root-composed schemas
with open(fixtures_file, "r") as f:
    spec = yaml.safe_load(f)

schemas = spec.get("components", {}).get("schemas", {})
composed_schemas = []

for name, schema in schemas.items():
    if not isinstance(schema, dict):
        continue
    root = dict(schema)
    skip_keys = {"description", "title", "deprecated", "example", "default", "nullable"}
    for skip_key in skip_keys:
        root.pop(skip_key, None)
    for comp_key in ("oneOf", "anyOf", "allOf"):
        if comp_key in root:
            composed_schemas.append((name, comp_key))
            break

print(f"\nFound {len(composed_schemas)} root-composed schema definitions in fixtures.")

# 2. Load expected-types manifest
expected_types = {}
if os.path.exists(expected_types_file):
    with open(expected_types_file) as f:
        et_raw = yaml.safe_load(f) or {}
    for k, v in et_raw.items():
        expected_types[k] = str(v)

print(f"Loaded {len(expected_types)} expected-type entries.")

# 3. Load NOT_FOUND deferred allowlist
deferred_schemas = set()
if os.path.exists(deferred_allowlist_file):
    with open(deferred_allowlist_file) as f:
        for line in f:
            stripped = line.strip()
            if stripped and not stripped.startswith("#"):
                deferred_schemas.add(stripped)

# 4. For each composed schema, analyze the generated C++ header
model_dir = os.path.join(output_dir, "model")

def analyze_model_header(model_name):
    header_file = os.path.join(model_dir, f"{model_name}.h")
    if not os.path.exists(header_file):
        return ("NOT_FOUND", False, -1, "")

    with open(header_file) as f:
        content = f.read()

    if "using " in content and "= boost::json::value" in content:
        for line in content.split("\n"):
            if "using " in line and "= boost::json::value" in line:
                alias_name = line.split("using ")[1].split(" =")[0].strip()
                return ("boost::json::value", False, 0, f"json value fallback alias {alias_name}")

    for line in content.split("\n"):
        stripped = line.strip()
        if stripped.startswith("using ") and "=" in stripped and ";" in stripped:
            alias_match = re.match(r'using\s+(\w+)\s*=\s*([^;]+);', stripped)
            if alias_match and not stripped.startswith("using namespace"):
                alias_target = alias_match.group(2).strip()
                if alias_target != model_name:
                    return (alias_target, False, 0, "type alias")

    if re.search(r'enum\s+class\s+\w+', content):
        enum_match = re.search(r'enum\s+class\s+(\w+)', content)
        if enum_match:
            cpp_type = f"org::openapitools::client::model::{enum_match.group(1)}"
            return (cpp_type, False, 0, "enum")

    member_decls = re.findall(r"m_\w+(?:\s*=\s*[^;]+)?;", content)
    actual_members = [m for m in member_decls if not re.match(r"m_\w+IsSet\b", m)]
    member_count = len(actual_members)

    class_match = re.search(r"^class\s+(\w+)", content, re.MULTILINE)
    if class_match:
        cpp_type = f"org::openapitools::client::model::{class_match.group(1)}"
    else:
        cpp_type = model_name

    has_to_json = "toJsonValue()" in content or "toJsonObject_internal()" in content
    has_from_json = "fromJsonValue(" in content or "fromJsonObject_internal(" in content
    is_empty_shell = has_to_json and has_from_json and member_count == 0 and class_match is not None

    return (cpp_type, is_empty_shell, member_count, "")

# 5. Build inventory
results = []
empty_shells = []
expected_mismatches = []
not_found_unless_deferred = []

for schema_name, comp_type in composed_schemas:
    cpp_type, is_empty, member_count, extra_notes = analyze_model_header(schema_name)

    if cpp_type == "NOT_FOUND":
        if schema_name in deferred_schemas:
            notes = f"Deferred (on allowlist)"
            result = "DEFERRED"
        else:
            notes = f"Header not found ({comp_type})"
            result = "NOT_FOUND"
            not_found_unless_deferred.append(schema_name)
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

    if result != "DEFERRED" and schema_name in expected_types:
        expected = expected_types[schema_name]
        actual_normalized = cpp_type.replace("org::openapitools::client::model::", "")
        expected_normalized = expected.replace("org::openapitools::client::model::", "")
        if actual_normalized != expected_normalized and expected_normalized != "NOT_FOUND":
            if cpp_type != expected and actual_normalized != expected_normalized:
                expected_mismatches.append((schema_name, expected, cpp_type))
                notes += f" | EXPECTED {expected}"
                result = "FAIL"

    results.append((schema_name, result, cpp_type, notes))

# 6. Write TSV
os.makedirs(compliance_dir, exist_ok=True)

with open(tsv_file, "w") as f:
    f.write("case\tresult\tcpp_type\tnotes\n")
    for schema_name, result, cpp_type, notes in results:
        f.write(f"{schema_name}\t{result}\t{cpp_type}\t{notes}\n")

# 7. Report
pass_count = sum(1 for _, r, _, _ in results if r == "PASS")
fail_count = sum(1 for _, r, _, _ in results if r == "FAIL")
not_found_count = sum(1 for _, r, _, _ in results if r == "NOT_FOUND")
deferred_count = sum(1 for _, r, _, _ in results if r == "DEFERRED")

print(f"\nInventory: {pass_count} PASS, {fail_count} FAIL, {not_found_count} NOT_FOUND, {deferred_count} deferred")
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
        comp_type = next((c for sn, c in composed_schemas if sn == name), "?")
        print(f"  - {name} ({comp_type})")
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

    local py_rc=0
    python3 "${inventory_py}" || py_rc=$?
    rm -f "${inventory_py}"
    return ${py_rc}
}

# =============================================================================
# Main execution
# =============================================================================
main() {
    local skip_build=false
    local only_inventory=false
    local INVENTORY_EXIT=0
    local NEGATIVE_EXIT=0

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
    echo "  Output:   ${OUTPUT_DIR}"
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

    # Export env vars used by the embedded Python inventory script
    export SCRIPT_DIR="${SCRIPT_DIR}"
    export OUTPUT_DIR="${OUTPUT_DIR}"

    inventory_schemas || INVENTORY_EXIT=$?
    generate_negative_fixtures || NEGATIVE_EXIT=$?

    header "Summary"
    echo ""
    echo "  TSV: ${SCRIPT_DIR}/composed-schemas.tsv"
    echo ""

    if [[ "${INVENTORY_EXIT}" -ne 0 ]] || [[ "${NEGATIVE_EXIT}" -ne 0 ]]; then
        echo -e "  ${RED}FAIL${NC}  Composition checks failed."
        if [[ "${NEGATIVE_EXIT}" -ne 0 ]]; then
            echo "       Negative fixture test(s) failed (codegen should have errored)."
        fi
        echo ""
        exit 1
    fi

    echo -e "  ${GREEN}PASS${NC}  All Gate A compliance checks passed."
    echo ""
}

main "$@"
