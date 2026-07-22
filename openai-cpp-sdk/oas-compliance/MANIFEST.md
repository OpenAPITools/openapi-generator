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

## Schema inventory (checked by gate-a.sh `composed-schemas.tsv`)

The table below lists only **root-composed component schemas** (oneOf, anyOf,
allOf as the defining keyword).  These are discovered, generated, and verified
against `expected-types.yaml` by the inventory step.  A mismatch between the
actual C++ type and the expected type produces a FAIL.

| Case ID                        | Schema                 | Composition    | Expected lowering                          | Negative? |
|--------------------------------|------------------------|----------------|--------------------------------------------|-----------|
| ANYOF-STRING-ENUM-COLLAPSE-001 | `AnyOfStringStringEnum`| anyOf          | `std::string`                              |           |
| ONEOF-STRING-ENUM-NO-COLLAPSE-001 | `OneOfStringStringEnum` | oneOf       | `boost::json::value` (no blind string collapse) |     |
| ONEOF-STRING-ARRAY-001         | `OneOfStringArray`     | oneOf          | `std::variant<std::string, std::vector<ItemStruct>>` |     |
| ALLOF-OBJECT-MERGE-001         | `AllOfObjectMerge`     | allOf          | Flattened struct (merged class)            |           |
| ALLOF-SCALAR-CONFLICT-001      | `AllOfScalarConflict`  | allOf          | Codegen error (not silent) — in `fixtures-negative.yaml` | ✓ |
| NULLABLE-001                   | `NullableString`       | anyOf + null   | `std::optional<std::string>`               |           |
| NULLABLE-002                   | `NullableEnum`         | anyOf + null   | `std::optional<std::string>`               |           |
| DISCRIMINATOR-ONEOF-001        | `DiscriminatorOneOf`   | oneOf + disc.  | `std::variant<Mammal, Bird>`               |           |
| MULTI-CONTENT-001              | `MultiContentStreamEvent`| oneOf         | `std::variant<StreamProgress, StreamComplete, StreamError>` | |
| MULTIPART-001                  | `MultipartMetaField`   | oneOf          | `std::variant<std::vector<std::uint8_t>, MetaRecord>` |    |

Each change to `fixtures.yaml` must update this manifest and re-verify the
`composed-schemas.tsv`.

## Non-inventory fixtures (path/media-type — checked in later phases)

The following constructs exist in `fixtures.yaml` but are NOT yet verified by
the `composed-schemas.tsv` inventory step.  They serve as structural placeholders
for later-phase compliance checks (media-type-driven responses, multipart
serialization, format mappings).

| Case ID               | Location                     | What it tests                              |
|-----------------------|------------------------------|--------------------------------------------|
| MULTI-CONTENT-001     | `GET /multi-content`         | Same-status `application/json` + `text/event-stream` |
| MULTIPART-001         | `POST /multipart-upload`     | Multipart with binary\|object oneOf field  |
| UNXTIME-001           | `TimestampedEvent` schema    | `format: unixtime` → `std::int64_t`        |

These will be activated as the compliance scope expands in later phases.
