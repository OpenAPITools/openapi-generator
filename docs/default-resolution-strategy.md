# DefaultResolutionStrategy — how `default` values are resolved in `allOf` schemas

OpenAPI does not define precedence rules for `default` values that appear in multiple
branches of an `allOf` composition. `ModelUtils.resolveDefault()` fills that gap with a
configurable **`DefaultResolutionStrategy`** enum. This document explains the four
strategies using a concrete nested schema so you can choose the one that best matches your
needs.

---

## Background: the collection phase

Before any strategy is applied, `ModelUtils` runs a single **post-order DFS** traversal of
the fully-resolved schema tree (all `$ref`s expanded, all `allOf` levels recursively
flattened). The traversal produces an ordered list of **`DefaultCandidate`** objects, one
for every schema node that carries a non-null `default:`. Each candidate records:

| Field | Meaning |
|---|---|
| `value` | The raw default value found on that schema node |
| `depth` | `0` for the root schema, `1` for its direct `allOf` items, `2` for their `allOf` items, … |
| `visitOrder` | Monotonically increasing post-order DFS index (children before parent, so the root's own `default:` is **always last**) |

Each strategy below is a pure **selector** over that candidate list; the collection phase is
identical for all of them.

---

## Canonical example schema

The following three-schema chain is used throughout this document.

```yaml
components:
  schemas:

    Base:
      type: string
      default: "base"        # depth 2 when reached from Root

    Middle:
      allOf:
        - $ref: '#/components/schemas/Base'
      default: "middle"      # depth 1 when reached from Root

    Root:
      allOf:
        - $ref: '#/components/schemas/Middle'
      default: "root"        # depth 0 — the root schema passed to resolveDefault
```

A second **"no-root-default" / "conflict"** variant is also used to show how strategies
diverge when `Root` has no direct default:

```yaml
    # Same as above but Root carries NO default:
    RootNoDefault:
      allOf:
        - $ref: '#/components/schemas/Middle'
        #  ↑ Middle itself contains Base, which has default "base"
        #  ↑ Middle also has default "middle"
        # Root itself has no default here
```

---

## DFS candidate walk

### Variant A — Root has a direct `default: "root"`

When `resolveDefault(openAPI, Root, strategy)` is called, the DFS visits:

| Step | Schema visited | depth | Own `default:` | visitOrder assigned |
|------|---------------|-------|----------------|---------------------|
| 1    | `Base` (leaf, reached via Middle's allOf) | 2 | `"base"` | 0 |
| 2    | `Middle` (after its allOf child) | 1 | `"middle"` | 1 |
| 3    | `Root` (after its allOf child Middle) | 0 | `"root"` | 2 |

**Candidate list (post-order):**

```
[ {value="base",   depth=2, visitOrder=0},
  {value="middle", depth=1, visitOrder=1},
  {value="root",   depth=0, visitOrder=2} ]
```

> The root schema's own `default:` is always appended **last** (highest `visitOrder`)
> because the traversal is post-order (children before parent).

### Variant B — Root has **no** direct `default:`

| Step | Schema visited | depth | Own `default:` | visitOrder assigned |
|------|---------------|-------|----------------|---------------------|
| 1    | `Base` | 2 | `"base"` | 0 |
| 2    | `Middle` | 1 | `"middle"` | 1 |
| 3    | `RootNoDefault` | 0 | _(none)_ | _(not added)_ |

**Candidate list (post-order):**

```
[ {value="base",   depth=2, visitOrder=0},
  {value="middle", depth=1, visitOrder=1} ]
```

---

## Strategies

### `LAST_WINS` *(backward-compatible default)*

> **Rule:** pick the candidate with the **highest `visitOrder`**.

Because the root's own `default:` is always assigned the highest `visitOrder` in the
post-order traversal, a direct root default always wins when present. When the root has no
direct default, the **last `allOf` branch** (recursively resolved) wins.

| Variant | Selected candidate | Result |
|---|---|---|
| A (Root has `default: "root"`) | `{value="root", visitOrder=2}` — highest | **`"root"`** |
| B (no root default) | `{value="middle", visitOrder=1}` — highest remaining | **`"middle"`** |

This strategy is used by the zero-argument
`ModelUtils.resolveDefault(openAPI, schema)` overload for backward compatibility.

---

### `NEAREST_WINS`

> **Rule:** pick the candidate with the **smallest `depth`**; break ties by smallest
> `visitOrder` (leftmost at the same depth).
>
> Emits `LOGGER.warn` when more than one distinct value is found.

"Nearest to the consumer" is treated as "most specific". A root direct default (depth 0)
therefore always wins when present. If the root has no direct default, the leftmost
`allOf` item at depth 1 is chosen over anything deeper.

| Variant | Selected candidate | Result | Warning logged? |
|---|---|---|---|
| A (Root has `default: "root"`) | `{value="root", depth=0}` — shallowest | **`"root"`** | Yes (3 distinct-ish values) |
| B (no root default) | `{value="middle", depth=1}` — shallowest remaining | **`"middle"`** | Yes (`"base"` vs `"middle"`) |

Compare with `LAST_WINS` for Variant B: both return `"middle"` here, but would diverge
if `Middle` listed `Base` **after** another sibling with a different default, because
`NEAREST_WINS` picks the *leftmost* sibling while `LAST_WINS` picks the *rightmost*.

---

### `ROOT_WINS`

> **Rule:** only `depth == 0` candidates count; all nested defaults are ignored.
> Returns `null` when the root schema has no direct `default:`.
>
> Emits `LOGGER.debug` when `allOf`-branch defaults are being ignored.

This is the most conservative heuristic. It intentionally discards everything inherited
from `allOf` branches, making the generator's output independent of what base schemas
define.

| Variant | Selected candidate | Result | Log emitted |
|---|---|---|---|
| A (Root has `default: "root"`) | `{value="root", depth=0}` | **`"root"`** | `DEBUG`: 2 branch defaults ignored |
| B (no root default) | _(none at depth 0)_ | **`null`** | `DEBUG`: 2 branch defaults ignored |

---

### `STRICT`

> **Rule:** if there are **2 or more distinct values** in the candidate list, log a `WARN`
> and return `null`. Otherwise return the single unambiguous value.
>
> Identical duplicates across branches are **not** a conflict — they resolve safely.

`STRICT` is the most spec-faithful mode. OpenAPI itself provides no precedence rule for
`default` in `allOf`, so `STRICT` refuses to silently choose a winner. Use it when you
want to detect ambiguity early and fix the spec rather than rely on a heuristic.

| Variant | Distinct values | Result | Warning logged? |
|---|---|---|---|
| A (Root has `default: "root"`) | 3 (`"base"`, `"middle"`, `"root"`) | **`null`** | Yes — lists all conflicting values |
| B (no root default) | 2 (`"base"`, `"middle"`) | **`null`** | Yes |
| All branches share the same value (e.g. every node says `"shared"`) | 1 | **`"shared"`** | No |

---

## Quick-reference comparison

The table below summarises all four strategies applied to both variants of the example
schema:

| Strategy | Variant A result (`Root` has `default: "root"`) | Variant B result (no root default) | Conflict logged? |
|---|---|---|---|
| `LAST_WINS` | `"root"` | `"middle"` | No |
| `NEAREST_WINS` | `"root"` | `"middle"` | Yes (`WARN`) |
| `ROOT_WINS` | `"root"` | `null` | No (`DEBUG` only) |
| `STRICT` | `null` | `null` | Yes (`WARN`) |

> **When there is only one distinct default value** across the entire candidate list, all
> four strategies agree and return that value.

---

## Choosing a strategy

| Goal | Recommended strategy |
|---|---|
| Keep existing generator behaviour | `LAST_WINS` *(default)* |
| "Most specific wins" — root overrides base, leftmost sibling wins ties | `NEAREST_WINS` |
| Ignore inherited defaults entirely; only trust an explicit root `default:` | `ROOT_WINS` |
| Fail fast on any ambiguity; enforce a single source of truth in the spec | `STRICT` |

---

## API

```java
// Default (LAST_WINS) — backward-compatible overload
Object value = ModelUtils.resolveDefault(openAPI, schema);

// Explicit strategy
Object value = ModelUtils.resolveDefault(openAPI, schema, DefaultResolutionStrategy.STRICT);
```

Source: [`DefaultResolutionStrategy.java`](../modules/openapi-generator/src/main/java/org/openapitools/codegen/utils/DefaultResolutionStrategy.java),
[`ModelUtils.java`](../modules/openapi-generator/src/main/java/org/openapitools/codegen/utils/ModelUtils.java)
