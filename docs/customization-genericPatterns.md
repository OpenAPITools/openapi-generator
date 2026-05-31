---
id: customization-generic-patterns
title: Generic Schema Substitution (Spring / Kotlin-Spring)
---

# Generic Schema Substitution (`genericPatterns`)

> Applies to the `spring` and `kotlin-spring` generators.

OpenAPI 3 has no native way to express *parametric* schemas — schemas that share a common shape but differ in one type. In practice many specs end up duplicating wrapper schemas:

```yaml
UserResponse:
  type: object
  properties:
    data: { $ref: '#/components/schemas/User' }
    requestId: { type: string }
OrderResponse:
  type: object
  properties:
    data: { $ref: '#/components/schemas/Order' }
    requestId: { type: string }
PetResponse:
  type: object
  properties:
    data: { $ref: '#/components/schemas/Pet' }
    requestId: { type: string }
```

Without this feature, the generator creates three nearly-identical `*Response` classes. With `genericPatterns`, the generator detects the family and replaces all references with a single `ApiResponse<T>` (either imported from your codebase or generated for you), and the redundant wrapper schemas are removed from the generated model package.

## Quick start

```yaml
# openapitools-generator-maven-plugin config (or YAML config file)
additionalProperties:
  genericPatterns:
    - suffix: Response
      genericClass: com.example.ApiResponse
      slot: data
```

After generation, every operation that previously returned `UserResponse` now returns `ApiResponse<User>`, properties of type `UserResponse` are rewritten too, and the three `*Response` model classes are no longer generated.

> ⚠️ Substitution and suppression of wrapper schemas only happen when `annotationLibrary=none` (Swagger / OpenAPI annotations on generated models reference the concrete classes, so they must be kept). Return-type substitution itself happens regardless.

## Pattern matching

Each entry in `genericPatterns` matches schemas by *name*:

| Field | Purpose | Required? |
|---|---|---|
| `suffix` | Schema name ends with this string (e.g. `Response` matches `UserResponse`) | exactly one of `suffix` or `prefix` |
| `prefix` | Schema name starts with this string (e.g. `Api` matches `ApiUser`) | exactly one of `suffix` or `prefix` |
| `genericClass` | Target generic class. Mode A (FQN) imports an external class; Mode B (simple name) generates a class in `configPackage`. | yes |
| `slot` | Property name whose `$ref` becomes `T`. Single type parameter. | one of `slot` / `slotArray` / `slots` |
| `slotArray` | Array property name whose `items.$ref` becomes `T`. Single type parameter. | one of `slot` / `slotArray` / `slots` |
| `slots` | Map of `propertyName: typeParamName` for multi-parameter generics (e.g. `{data: T, error: E}`). | one of `slot` / `slotArray` / `slots` |

## Mode A vs Mode B

The form of `genericClass` decides whether a class file is generated:

* **Mode A** — `genericClass` contains a dot (`.`). Treated as a fully-qualified class name; only an `importMapping` entry is added. **Use this when the class already exists** in your codebase or in a library:

  ```yaml
  - suffix: Response
    genericClass: com.acme.api.ApiResponse   # already exists in com.acme.api
    slot: data
  ```

* **Mode B** — `genericClass` is a simple name (no dot). A new source file is generated in `configPackage` (defaults to `org.openapitools.configuration`). The generated class mirrors the non-slot properties of the matched schemas and declares the configured type parameters:

  ```yaml
  - suffix: Page
    genericClass: ApiPage          # creates ApiPage.java/kt with the common props + <T>
    slotArray: content
  ```

## Multi-slot generics

Use `slots` (instead of `slot` / `slotArray`) to map multiple properties to multiple type parameters:

```yaml
genericPatterns:
  - suffix: ErrorResult
    genericClass: Result           # generated class Result<T, E>
    slots:
      data: T                      # 'data' property → T
      error: E                     # 'error' property → E
```

A spec schema `UserValidationErrorResult` (with `data: $ref User`, `error: $ref ValidationError`) becomes `Result<User, ValidationError>` everywhere it appears.

Array-ness of each slot property is auto-detected from the matched schema — you do not need to declare it.

## Vendor-extension overrides

Patterns are a heuristic. If a single schema needs to be substituted differently (or excluded), declare the substitution inline:

```yaml
components:
  schemas:
    SearchPage:
      x-generic:
        class: org.springframework.data.domain.Slice
        slot: content
      type: object
      properties:
        content:
          type: array
          items: { $ref: '#/components/schemas/SearchResult' }
        hasNext: { type: boolean }
```

Vendor-extension declarations take precedence over both name-pattern matches and structurally-detected paged models.

## Discovery (`discoverGenericPatterns`)

If you don't yet know which schemas in your spec are good candidates, enable:

```yaml
additionalProperties:
  discoverGenericPatterns: true
```

During the next generation, the tool scans for **structural clusters** — groups of 2+ schemas with identical property structure except for one varying `$ref` property — and logs a ready-to-paste `genericPatterns:` YAML block at **INFO** level. No substitution is applied; it is purely a suggestion.

> ℹ️ To see the suggestions, ensure your logging configuration emits INFO-level messages for `org.openapitools.codegen.languages.GenericSchemaScanUtils`. The Maven plugin shows them by default; CLI users may need `--verbose`.

### What discovery does *not* find

Discovery is intentionally limited to **single-`$ref` slot** patterns on **flat-object** schemas. It will **not** suggest:

* **`Page<T>` / `PagedModel<T>`-style schemas** — the varying property in a paged response is typically an `array` of `$ref` (e.g. `content: { type: array, items: { $ref: ... } }`), which Tier-3 discovery skips. Schemas defined via `allOf` are also skipped. For these, enable [`substituteGenericPagedModel`](#companion-feature-substitutegenericpagedmodel) instead — it performs *structural* paged-model detection that handles both the flat-object and `allOf` forms and the `array[$ref] + metadata-$ref` shape.
* **Multi-slot generics** (e.g. `Result<T, E>`) — only single-slot families are auto-detected.
* **Schemas that don't share a common name suffix** — clustering also needs a stable naming convention to suggest a usable pattern.

For any of these, fall back to a hand-written `genericPatterns` entry or a `x-generic` vendor extension on the individual schema.

## Companion feature: `substituteGenericPagedModel`

A purely structural variant exists for the very common Spring `PagedModel<T>` case:

```yaml
additionalProperties:
  substituteGenericPagedModel: true
```

This requires *no* pattern config and *no* naming convention — the generator detects any schema with a `content` array property and a pagination-metadata `$ref` (e.g. a `PageMetadata`-style sibling), in both flat-object and `allOf` forms. The detected paged schemas are replaced with `PagedModel<T>` and the orphaned metadata schemas are suppressed.

Internally this routes through the same substitution engine as `genericPatterns`, so all the interactions described below apply.

## Interaction with other options

| Option | Effect |
|---|---|
| `schemaMapping` | A schema name present in `schemaMapping` is **never** substituted by `genericPatterns` (the user-declared mapping wins). The corresponding companion meta-schema is kept alive too if any sibling main is still mapped. |
| `importMapping` | Mode A registers its FQN here automatically. For `substituteGenericPagedModel`, set `importMapping.PagedModel` (or any custom name) to override the default Spring class. |
| `modelNameSuffix` / `modelNamePrefix` / `modelNameMapping` | Fully supported. Registry keys are re-keyed via `toModelName()` so lookups by the transformed name work. Two raw names collapsing to the same transformed name will emit a `WARN` log and only one substitution will apply. |
| `annotationLibrary != none` | Return-type substitution still runs, but wrapper / meta schemas are kept (annotations like `@ApiResponse`, `@Schema` reference them by class). |

## Examples in the repository

* `bin/configs/spring-boot-generics.yaml` and `bin/configs/kotlin-spring-boot-generics.yaml` — runnable end-to-end configurations covering all three pattern forms plus discovery.
* `samples/server/petstore/springboot-generics/` — generated output from the above config.
* The input spec at `modules/openapi-generator/src/test/resources/3_0/spring/petstore-generics.yaml` exercises single-slot, multi-slot, array-slot, vendor-extension, and paged-model variants together.

## Limitations and notes

* The features are currently implemented only for the `spring` and `kotlin-spring` generators.
* Pattern matching is purely name-based — schemas that do not share a naming convention are not detected by tier-2 patterns (use vendor extensions on those schemas, or rely on `discoverGenericPatterns` / `substituteGenericPagedModel`).
* Suppression of substituted schemas is gated on `annotationLibrary=none` (see above).
* The data class powering this is documented in detail in [`GenericPatternConfig.java`](https://github.com/OpenAPITools/openapi-generator/blob/master/modules/openapi-generator/src/main/java/org/openapitools/codegen/languages/GenericPatternConfig.java).
