# Gate A — OAS Composition Compliance Harness

This directory contains **Gate A** of the dual compliance harness for
`cpp-boost-beast-client`.  Gate A exercises generic OpenAPI 3.1 composed-schema
constructs without any dependency on the OpenAI specification.

## Quick start

```bash
# Full pipeline: build generator → generate → inventory → report
./gate-a.sh

# Skip the generator jar build if it is already up to date
./gate-a.sh --skip-build

# Inventory-only (requires generated output to exist)
./gate-a.sh inventory
```

## What it tests

| Fixture | Covers |
|---------|--------|
| `AnyOfStringStringEnum` | anyOf string + string-enum → `std::string` collapse |
| `OneOfStringArray` | oneOf string \| array → `std::variant` |
| `AllOfObjectMerge` | compatible allOf object flattening |
| `AllOfScalarConflict` | allOf type conflict → codegen **error** (negative test) |
| `NullableString` / `NullableEnum` | anyOf + null → `std::optional<T>` |
| `DiscriminatorOneOf` | discriminator + oneOf → variant dispatch |
| `/multi-content` | same-status `application/json` + `text/event-stream` |
| `/multipart-upload` | multipart with binary\|object oneOf field |
| `TimestampedEvent` | `format: unixtime` → `std::int64_t` |

## Output

The script produces `composed-schemas.tsv` with the results:

```text
case    result  cpp_type  notes
AnyOfStringStringEnum   PASS    std::string     anyOf string+enum collapse
OneOfStringArray        PASS    std::variant<std::string,std::vector<ItemStruct>>     oneOf
AllOfObjectMerge        PASS    org::openapitools::client::model::AllOfObjectMerge    allOf merged, N members
AllOfScalarConflict     PASS    codegen_error   allOf conflict detected
...
```

**Exit codes:**

| Code | Meaning |
|------|---------|
| 0 | All checks pass |
| 1 | Empty-shell or type-mismatch failures |
| 2 | Generation / tooling failure |

## Design reference

The full design rationale for variant-first composed-schema lowering,
nullability policy, and media-type-driven streaming is documented in the
generator README template:

- `modules/openapi-generator/src/main/resources/cpp-boost-beast-client/README.mustache`
  (shipped as `README.md` in every generated client)

Key sections:
- **Breaking model strategy** — why empty shells were replaced with
  `std::variant`/`std::optional`/collapsed types.
- **Nullability: omit vs null** — spec-aware field encoding policy
  distinguishing absent from JSON null.
- **Dual compliance gates** — the role of Gate A vs Gate B in the
  release pipeline.
- **Media-type-driven streaming** — why streaming is driven by the
  `content` map, not boolean parameters.
- **Non-standard format mappings** — `unixtime` → `std::int64_t` as
  documented convenience mapping.
- **Large breaking PR** — why this is acceptable and how to review it.

## Running both gates

Use `./run-dual-gates.sh` from the `openai-cpp-sdk/` root to run Gate A
and Gate B sequentially:

```bash
./run-dual-gates.sh
```

Or invoke each individually:

```bash
# Gate A only
./gate-a.sh

# Gate B only
cd ../openai/compliance && ./harness.sh
```

## Adding a new fixture

1. Add the schema to `fixtures.yaml`.
2. Add an entry to `MANIFEST.md`.
3. Run `./gate-a.sh` and verify the TSV output.
4. Commit `fixtures.yaml`, `MANIFEST.md`, and updated `composed-schemas.tsv`.
