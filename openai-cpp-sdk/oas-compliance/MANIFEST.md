# OAS Composition Fixtures — Gate A Manifest

| Field          | Value                                 |
|----------------|---------------------------------------|
| Fixtures path  | `fixtures.yaml`                       |
| Spec format    | OpenAPI 3.1.0                         |
| API title      | OAS Composition Fixtures              |
| API version    | `1.0.0`                               |
| Commit date    | 2026-07-20                            |

**Purpose:** Gate A is an *OAS-first* compliance harness that exercises
every composed-schema lowering rule that `cpp-boost-beast-client` must
support — **without** any dependency on the OpenAI OpenAPI document or
OpenAI-specific type names, vendor extensions, or patterns.

**Relationship to Gate B:**

| Gate | Role | Dependency |
|------|------|------------|
| A — OAS composition fixtures | Spec correctness. Small, named fixtures for every lowering rule. Must pass independently of OpenAI. | **None** (OAS only) |
| B — OpenAI corpus | Scale and realism. Full generate/compile/round-trip against the pinned OpenAI document. | OpenAI `openapi.yaml` (pinned) |

Release is blocked while either gate has non-deferred `FAIL`s.

## Fixture inventory

All fixtures live in `fixtures.yaml` under `components/schemas/` using
**generic** names (no `OpenAI` prefix or vendor-specific identifiers).

| Case ID                        | Schema                 | Composition   | Expected lowering                | Negative? |
|--------------------------------|------------------------|---------------|----------------------------------|-----------|
| ANYOF-STRING-ENUM-COLLAPSE-001 | `AnyOfStringStringEnum`| anyOf         | `std::string`                    |           |
| ONEOF-STRING-ARRAY-001         | `OneOfStringArray`     | oneOf         | `std::variant<string, vector>`   |           |
| ALLOF-OBJECT-MERGE-001         | `AllOfObjectMerge`     | allOf         | Flattened struct                 |           |
| ALLOF-SCALAR-CONFLICT-001      | `AllOfScalarConflict`  | allOf         | Codegen error (not silent)       | ✓         |
| NULLABLE-001                   | `NullableString`       | anyOf + null  | `std::optional<std::string>`     |           |
| NULLABLE-002                   | `NullableEnum`         | anyOf + null  | `std::optional<std::string>`     |           |
| DISCRIMINATOR-ONEOF-001        | `DiscriminatorOneOf`   | oneOf + disc  | `std::variant<Mammal, Bird>`     |           |
| MULTI-CONTENT-001              | `/multi-content` resp  | multi-media   | Accept-driven dual schema         |           |
| MULTIPART-001                  | `/multipart-upload`    | multipart     | Variant binary\|object parts     |           |
| UNXTIME-001                    | `TimestampedEvent`     | object        | `std::int64_t` for unixtime      |           |

Each change to `fixtures.yaml` must update this manifest and re-verify the
`composed-schemas.tsv`.
