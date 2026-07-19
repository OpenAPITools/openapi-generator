#!/usr/bin/env python3
"""
Generate, compile, and run golden case test runner.

Called by harness.sh. Exits:
  0 — all cases PASS
  1 — one or more cases FAIL at runtime
  2 — no compiled library found (cannot run)
  3 — test runner compilation failed
"""

import json, os, subprocess, sys, tempfile

sdk_dir = os.environ.get("OPENAI_SDK_DIR", os.path.abspath("."))
compliance_dir = os.path.join(sdk_dir, "openai", "compliance")
golden_file = os.path.join(compliance_dir, "golden-cases.json")
generated_dir = os.path.join(sdk_dir, "generated")
build_dir = os.path.join(sdk_dir, "build-generated")

if not os.path.exists(golden_file):
    print(f"ERROR: golden-cases.json not found at {golden_file}")
    sys.exit(1)

with open(golden_file) as f:
    cases = json.load(f)

print(f"\nLoaded {len(cases)} golden test case(s)")

# Determine library file
lib_path = None
for ext in [".dylib", ".so", ".dll"]:
    candidate = os.path.join(build_dir, f"libCppBoostBeastOpenAPIClient{ext}")
    if os.path.exists(candidate):
        lib_path = candidate
        break

if lib_path is None:
    print(f"ERROR: No compiled library found in {build_dir}")
    print(f"  Run the harness compile step first, or run the full pipeline.")
    sys.exit(2)

# Build a test runner
test_runner_dir = tempfile.mkdtemp(prefix="openai-golden-")
test_runner_src = os.path.join(test_runner_dir, "golden_runner.cpp")
test_runner_bin = os.path.join(test_runner_dir, "golden_runner")

schemas = set(c["schema"] for c in cases)

with open(test_runner_src, "w") as f:
    f.write("""// Auto-generated golden case test runner
#include <boost/json.hpp>
#include <iostream>
#include <string>
#include <vector>
#include <cstdlib>
#include <sstream>

""")
    for schema in sorted(schemas):
        f.write(f'#include "{schema}.h"\n')
    f.write("""
using namespace org::openapitools::client::model;

struct TestCase {
    std::string name;
    std::string schema;
    std::string json_input;
    std::string description;
};

struct TestResult {
    std::string name;
    bool passed;
    std::string error;
};

std::vector<TestCase> parseCases() {
    std::vector<TestCase> cases;
""")
    golden_json_str = json.dumps(cases, indent=2)
    f.write(f'    const char* raw = R"RAW_JSON(\n{golden_json_str}\n)RAW_JSON";\n')
    f.write("""
    auto parsed = boost::json::parse(raw);
    for (const auto& entry : parsed.as_array()) {
        TestCase tc;
        tc.name = boost::json::value_to<std::string>(entry.at("name"));
        tc.schema = boost::json::value_to<std::string>(entry.at("schema"));
        tc.json_input = boost::json::value_to<std::string>(entry.at("json"));
        if (entry.as_object().if_contains("description")) {
            tc.description = boost::json::value_to<std::string>(entry.at("description"));
        }
        cases.push_back(std::move(tc));
    }
    return cases;
}

template<typename ModelT>
TestResult testRoundTrip(const TestCase& tc) {
    TestResult result{tc.name, false, ""};
    try {
        auto json_val = boost::json::parse(tc.json_input);
        ModelT model(json_val);
        auto output_val = model.toJsonValue();
        std::string input_str = boost::json::serialize(json_val);
        std::string output_str = boost::json::serialize(output_val);
        auto input_reparsed = boost::json::parse(input_str);
        auto output_reparsed = boost::json::parse(output_str);
        if (input_reparsed == output_reparsed) {
            result.passed = true;
        } else {
            std::ostringstream err;
            err << "Round-trip mismatch"
                << "  Input:  " << input_str.substr(0, 200)
                << "  Output: " << output_str.substr(0, 200);
            result.error = err.str();
        }
    } catch (const std::exception& e) {
        std::ostringstream err;
        err << "Exception: " << e.what();
        result.error = err.str();
    }
    return result;
}

int main() {
    auto cases = parseCases();
    int passed = 0;
    int failed = 0;

    for (const auto& tc : cases) {
        TestResult result;
""")
    for case_entry in cases:
        schema = case_entry["schema"]
        f.write(
            f'        if (tc.schema == "{schema}") result = testRoundTrip<{schema}>(tc); else\n'
        )
    f.write("""        {
            result.name = tc.name;
            result.passed = false;
            result.error = "Unknown schema: " + tc.schema;
        }

        if (result.passed) {
            std::cout << "  PASS  " << result.name;
            if (!tc.description.empty()) std::cout << " - " << tc.description;
            std::cout << std::endl;
            passed++;
        } else {
            std::cout << "  FAIL  " << result.name;
            if (!tc.description.empty()) std::cout << " - " << tc.description;
            std::cout << std::endl;
            if (!result.error.empty()) std::cout << "        " << result.error << std::endl;
            failed++;
        }
    }
    std::cout << std::endl << "Results: " << (passed+failed) << " total, "
              << passed << " passed, " << failed << " failed" << std::endl;
    return failed > 0 ? 1 : 0;
}
""")

# Compile the test runner
boost_include = (
    "-I/opt/homebrew/include" if os.path.exists("/opt/homebrew/include") else ""
)
include_flags = f"-I{generated_dir} -I{generated_dir}/model -I{generated_dir}/api"

compile_cmd = (
    f"c++ -std=c++17 {include_flags} {boost_include} "
    f"-o {test_runner_bin} {test_runner_src} "
    f"-L{build_dir} -lCppBoostBeastOpenAPIClient "
    f"-Wl,-rpath,{build_dir}"
)

print(f"\nCompiling golden test runner...")

result = subprocess.run(compile_cmd, shell=True, capture_output=True, text=True)

if result.returncode != 0:
    print(f"ERROR: Could not compile golden test runner (code {result.returncode}):")
    stderr_lines = result.stderr.split("\n")
    for line in stderr_lines[:30]:
        if line.strip():
            print(f"  | {line}")
    sys.exit(3)

# Run the test runner
print(f"Running golden cases...")
run_result = subprocess.run([test_runner_bin], capture_output=True, text=True)
print(run_result.stdout)

# Cleanup
import shutil

shutil.rmtree(test_runner_dir, ignore_errors=True)

# Exit with the runner's return code (non-zero = cases failed)
sys.exit(run_result.returncode)
