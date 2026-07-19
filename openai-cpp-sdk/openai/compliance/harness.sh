#!/usr/bin/env bash
# =============================================================================
# OpenAI Compliance Harness — Phase 0
# =============================================================================
# Pins the spec version, generates the full OpenAI client, compiles it,
# inventories all composed schemas, runs golden encode/decode cases,
# and fails on any empty-shell composed model.
#
# Usage:
#   ./harness.sh                    # Full run (build + generate + compile + verify)
#   ./harness.sh --skip-build       # Skip generator jar build
#   ./harness.sh --skip-generate    # Skip code generation
#   ./harness.sh --skip-compile     # Skip compilation
#   ./harness.sh inventory          # Inventory + shell check only (no build/gen/compile)
#   ./harness.sh golden             # Golden case encode/decode only
#
# Exit codes:
#   0 — all checks pass
#   1 — inventory or golden case failure
#   2 — generation failure
#   3 — compilation failure
# =============================================================================

set -euo pipefail
IFS=$'\n\t'

# ---- Paths (all relative to the script location) ----------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OPENAI_SDK_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"  # may be the monorepo root or sdk root

# Determine the real project root (where pom.xml / mvnw lives)
if [[ -f "${PROJECT_ROOT}/mvnw" ]]; then
    PROJECT_ROOT="${PROJECT_ROOT}"
elif [[ -f "${OPENAI_SDK_DIR}/../mvnw" ]]; then
    PROJECT_ROOT="$(cd "${OPENAI_SDK_DIR}/.." && pwd)"
else
    # Fall back to scanning upward
    CANDIDATE="${SCRIPT_DIR}"
    while [[ "${CANDY}" != "/" ]]; do
        if [[ -f "${CANDIDATE}/mvnw" ]]; then
            PROJECT_ROOT="${CANDIDATE}"
            break
        fi
        CANDIDATE="$(dirname "${CANDIDATE}")"
    done
fi

SPEC="${OPENAI_SDK_DIR}/openapi.yaml"
GENERATED_DIR="${OPENAI_SDK_DIR}/generated"
BUILD_DIR="${OPENAI_SDK_DIR}/build-generated"
COMPLIANCE_DIR="${OPENAI_SDK_DIR}/openai/compliance"
JAR_DIR="${PROJECT_ROOT}/modules/openapi-generator-cli/target"
JAR="${JAR_DIR}/openapi-generator-cli.jar"
MVNW="${PROJECT_ROOT}/mvnw"

# ---- Colors -----------------------------------------------------------------
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

pass_msg() { echo -e "  ${GREEN}PASS${NC}  $1"; }
fail_msg() { echo -e "  ${RED}FAIL${NC}  $1"; }
info_msg() { echo -e "  ${BLUE}INFO${NC}  $1"; }
warn_msg() { echo -e "  ${YELLOW}WARN${NC}  $1"; }
header()   { echo -e "\n${BOLD}═══ $1 ═══${NC}\n"; }

# ---- Global counters --------------------------------------------------------
PASS_COUNT=0
FAIL_COUNT=0

# ---- Step 1: Build generator jar --------------------------------------------
build_generator() {
    header "Step 1: Build generator CLI jar"
    if [[ -f "${JAR}" ]]; then
        info_msg "Jar already exists at ${JAR}"
        info_msg "Delete it or run with --skip-build to skip rebuild"
        return 0
    fi

    info_msg "Building openapi-generator-cli.jar (this may take a while)..."
    cd "${PROJECT_ROOT}"

    if [[ ! -f "${MVNW}" ]]; then
        echo "  ${RED}ERROR${NC}  mvnw not found at ${MVNW}"
        exit 2
    fi

    "${MVNW}" -pl modules/openapi-generator-cli -am compile package -DskipTests -Dmaven.test.skip=true -q 2>&1

    if [[ ! -f "${JAR}" ]]; then
        echo "  ${RED}ERROR${NC}  Jar was not created at ${JAR}"
        exit 2
    fi
    info_msg "Generator jar ready: ${JAR}"
}

# ---- Step 2: Generate OpenAI client ------------------------------------------
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
    
    cd "${PROJECT_ROOT}"

    # Preserve the FILES manifest so we can detect what changed
    if [[ -f "${GENERATED_DIR}/.openapi-generator/FILES" ]]; then
        cp "${GENERATED_DIR}/.openapi-generator/FILES" /tmp/openapi-files-backup.txt 2>/dev/null || true
    fi

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

# ---- Step 3: Compile generated code ------------------------------------------
compile_client() {
    header "Step 3: Compile generated client"

    if [[ ! -d "${GENERATED_DIR}" ]]; then
        echo "  ${RED}ERROR${NC}  Generated directory not found. Run generator first."
        exit 3
    fi

    mkdir -p "${BUILD_DIR}"
    cd "${BUILD_DIR}"

    info_msg "Configuring with CMake..."
    cmake "${GENERATED_DIR}" -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_FLAGS="-Werror" 2>&1

    info_msg "Building with Make..."
    make -j"$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)" 2>&1

    info_msg "Compilation complete"
}

# ---- Step 4: Inventory composed schemas --------------------------------------
inventory_schemas() {
    header "Step 4: Inventory composed schemas"

    local inventory_file="${COMPLIANCE_DIR}/schema-inventory.tsv"
    
    # Use Python for robust YAML parsing and C++ header analysis
    python3 <<'PYEOF'
import os, re, json, sys

sdk_dir = os.environ.get('OPENAI_SDK_DIR', os.path.abspath('.'))
generated_dir = os.path.join(sdk_dir, 'generated')
spec_file = os.path.join(sdk_dir, 'openapi.yaml')
compliance_dir = os.path.join(sdk_dir, 'openai', 'compliance')
inventory_file = os.path.join(compliance_dir, 'schema-inventory.tsv')

if not os.path.exists(generated_dir):
    print(f"ERROR: Generated directory not found: {generated_dir}")
    sys.exit(1)

# ---------------------------------------------------------------------------
# 1. Parse the YAML spec to find all schema names with oneOf/anyOf/allOf
# ---------------------------------------------------------------------------
# We use line-based detection rather than a full YAML parser for speed on ~84K lines
composed_schemas = []  # list of (schema_name, composition_type)
current_schema = None

schema_section = False
with open(spec_file, 'r', encoding='utf-8') as f:
    lines = f.readlines()

for i, line in enumerate(lines):
    # Detect start of components/schemas
    if re.match(r'^\s*components:', line):
        continue
    if re.match(r'^\s*schemas:', line):
        schema_section = True
        continue
    
    if not schema_section:
        continue
    
    # End of schemas section (next top-level key)
    if schema_section and line and not line.startswith(' ') and not line.startswith('\t') and line.strip():
        if any(line.startswith(k) for k in ('openapi:', 'info:', 'servers:', 'security:', 'tags:', 'paths:', 'externalDocs:', 'webhooks:')):
            # We've left schemas section (unlikely in OAS but handle it)
            pass
    
    # Detect schema name (indented 4 spaces, followed by ':')
    m = re.match(r'^    (\w+):', line)
    if m:
        current_schema = m.group(1)
        # Look ahead for oneOf/anyOf/allOf
        for j in range(i + 1, min(i + 30, len(lines))):
            comp_match = re.match(r'^\s+(oneOf|anyOf|allOf):', lines[j])
            if comp_match:
                composed_schemas.append((current_schema, comp_match.group(1)))
                break

print(f"\nFound {len(composed_schemas)} composed schema definitions in spec.")

# ---------------------------------------------------------------------------
# 2. For each composed schema, analyze the generated C++ header
# ---------------------------------------------------------------------------
model_dir = os.path.join(generated_dir, 'model')

def analyze_model_header(model_name):
    """Analyze a generated model header and return (cpp_type, is_empty_shell, member_count)."""
    header_file = os.path.join(model_dir, f"{model_name}.h")
    if not os.path.exists(header_file):
        return ("NOT_FOUND", False, -1)
    
    with open(header_file, 'r') as f:
        content = f.read()
    
    # Check if it's a type alias (e.g., using AnyType = boost::json::value)
    if 'using ' in content and '= boost::json::value' in content:
        return ("boost::json::value", False, 0)
    
    # Check if it inherits from another class
    inherit_match = re.search(r'class\s+\w+\s*:\s*public\s+(\w+)', content)
    
    # Count member variables (lines with m_ prefix in protected section)
    # Only count the declarations, not the *IsSet companion booleans
    member_decls = re.findall(r'm_\w+(?:\s*=\s*[^;]+)?;', content)
    # Remove *IsSet booleans from count
    actual_members = [m for m in member_decls if not re.match(r'm_\w+IsSet\b', m)]
    member_count = len(actual_members)
    
    # Determine the emitted C++ type name
    # Match the actual class declaration (not comments like "This class is...")
    # Look for a class declaration at line start (possibly indented)
    class_match = re.search(r'^class\s+(\w+)', content, re.MULTILINE)
    if class_match:
        cpp_type = f"org::openapitools::client::model::{class_match.group(1)}"
    else:
        cpp_type = model_name
    
    # Empty shell = has toJsonValue/fromJsonValue but NO member variables
    has_to_json = 'toJsonValue()' in content or 'toJsonObject_internal()' in content
    has_from_json = 'fromJsonValue(' in content or 'fromJsonObject_internal(' in content
    
    is_empty_shell = has_to_json and has_from_json and member_count == 0 and class_match is not None
    
    return (cpp_type, is_empty_shell, member_count)

# ---------------------------------------------------------------------------
# 3. Build the inventory
# ---------------------------------------------------------------------------
results = []
empty_shells = []

for schema_name, comp_type in composed_schemas:
    cpp_type, is_empty, member_count = analyze_model_header(schema_name)
    
    if cpp_type == "NOT_FOUND":
        notes = f"Header not found in generated output"
        result = "NOT_FOUND"
    elif is_empty:
        notes = f"EMPTY SHELL ({comp_type}, {member_count} members)"
        result = "FAIL"
        empty_shells.append((schema_name, comp_type))
    else:
        notes = f"{comp_type}, {member_count} members"
        result = "PASS"
    
    results.append((schema_name, result, cpp_type, notes))

# ---------------------------------------------------------------------------
# 4. Write inventory TSV
# ---------------------------------------------------------------------------
os.makedirs(compliance_dir, exist_ok=True)

with open(inventory_file, 'w') as f:
    f.write("schema\tresult\tcpp_type\tnotes\n")
    for schema_name, result, cpp_type, notes in results:
        f.write(f"{schema_name}\t{result}\t{cpp_type}\t{notes}\n")

# ---------------------------------------------------------------------------
# 5. Report
# ---------------------------------------------------------------------------
pass_count = sum(1 for _, r, _, _ in results if r == "PASS")
fail_count = sum(1 for _, r, _, _ in results if r == "FAIL")
not_found_count = sum(1 for _, r, _, _ in results if r == "NOT_FOUND")

print(f"\nInventory complete: {pass_count} PASS, {fail_count} FAIL, {not_found_count} not found")
print(f"TSV written to: {inventory_file}")

if empty_shells:
    print(f"\n{''.join(['='*60]):s}")
    print(f"EMPTY SHELL COMPOSED MODELS DETECTED:")
    for name, comp in empty_shells:
        print(f"  - {name} ({comp})")
    print(f"{''.join(['='*60]):s}")
    print(f"Total: {len(empty_shells)} empty shell(s)\n")

# Output machine-readable summary
print(f"__EMPTY_SHELL_COUNT__={len(empty_shells)}")
print(f"__PASS_COUNT__={pass_count}")
print(f"__FAIL_COUNT__={fail_count}")

if empty_shells:
    sys.exit(1)

PYEOF
}

# ---- Step 5: Golden encode/decode cases -------------------------------------
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

    OPENAI_SDK_DIR="${OPENAI_SDK_DIR}" python3 "${runner_script}"
    return 0
}

# ---- Main execution ---------------------------------------------------------
main() {
    local skip_build=false
    local skip_generate=false
    local skip_compile=false
    local only_inventory=false
    local only_golden=false

    # Parse arguments
    for arg in "$@"; do
        case "$arg" in
            --skip-build)    skip_build=true ;;
            --skip-generate) skip_generate=true ;;
            --skip-compile)  skip_compile=true ;;
            inventory)       only_inventory=true ;;
            golden)          only_golden=true ;;
            --help|-h)
                echo "Usage: $0 [--skip-build] [--skip-generate] [--skip-compile] [inventory|golden]"
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

    if [[ ! -f "${SPEC}" ]]; then
        echo "  ${RED}ERROR${NC}  Spec not found at ${SPEC}"
        exit 1
    fi

    if [[ "${only_golden}" == "true" ]]; then
        run_golden_cases
        exit $?
    fi

    if [[ "${only_inventory}" == "true" ]]; then
        inventory_schemas
        exit $?
    fi

    # Full pipeline
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

    # Extract empty shell count from inventory TSV (last line of inventory output has it)
    local shell_count=0
    if [[ -f "${COMPLIANCE_DIR}/schema-inventory.tsv" ]]; then
        shell_count=$(grep -c $'\tFAIL\t' "${COMPLIANCE_DIR}/schema-inventory.tsv" 2>/dev/null || echo 0)
    fi

    # Final summary
    header "Summary"
    echo ""
    echo "  Inventory: See ${COMPLIANCE_DIR}/schema-inventory.tsv"
    echo "  Expected types: ${COMPLIANCE_DIR}/expected-types.yaml"
    echo "  Golden cases: ${COMPLIANCE_DIR}/golden-cases.json"
    echo ""

    if [[ "${INVENTORY_EXIT}" -ne 0 ]]; then
        echo "  ${RED}FAIL${NC}  ${shell_count} empty-shell composed model(s) detected."
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
