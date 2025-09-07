# Rust Generator: oneOf vs anyOf Semantics

## Overview

The Rust OpenAPI generator properly implements the semantic differences between `oneOf` and `anyOf` schemas as defined in the OpenAPI specification:

- **oneOf (XOR)**: Exactly one of the schemas must validate
- **anyOf (OR)**: One or more of the schemas must validate

### OpenAPI Specification References

From the [OpenAPI 3.1.0 Specification](https://spec.openapis.org/oas/v3.1.0#schema-object):

- **[oneOf](https://spec.openapis.org/oas/v3.1.0#composition-and-inheritance-polymorphism)**: "Validates the value against exactly one of the subschemas"
- **[anyOf](https://spec.openapis.org/oas/v3.1.0#composition-and-inheritance-polymorphism)**: "Validates the value against any (one or more) of the subschemas"

These keywords come from [JSON Schema](https://json-schema.org/understanding-json-schema/reference/combining.html) and maintain the same semantics.

## Implementation Details

### oneOf - Untagged Enums

For `oneOf` schemas without a discriminator, the generator creates untagged enums using Serde's `#[serde(untagged)]` attribute:

```yaml
# OpenAPI Schema
SimpleOneOf:
  oneOf:
    - type: string
    - type: number
```

```rust
// Generated Rust Code
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SimpleOneOf {
    String(String),
    Number(f64),
}
```

**Behavior**: When deserializing, Serde tries each variant in order and stops at the first match. This ensures exactly one variant is selected.

### anyOf - Structs with Optional Fields

For `anyOf` schemas, the generator creates structs with optional fields, allowing multiple schemas to be valid simultaneously:

```yaml
# OpenAPI Schema
SimpleAnyOf:
  anyOf:
    - type: string
    - type: number
```

```rust
// Generated Rust Code
#[derive(Clone, Default, Debug, PartialEq, Serialize, Deserialize)]
pub struct SimpleAnyOf {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub as_String: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub as_Number: Option<f64>,
}

impl SimpleAnyOf {
    pub fn validate_any_of(&self) -> Result<(), String> {
        if self.as_String.is_none() && self.as_Number.is_none() {
            return Err("At least one anyOf field must be set".to_string());
        }
        Ok(())
    }
}
```

**Behavior**: Multiple fields can be set, properly implementing OR semantics where data can match multiple schemas.

## Practical Examples

### Example 1: Person or Company (oneOf)

```yaml
PersonOrCompany:
  oneOf:
    - $ref: '#/components/schemas/Person'
    - $ref: '#/components/schemas/Company'
```

```rust
// Usage
let data = PersonOrCompany::Person(Box::new(Person {
    first_name: "John".to_string(),
    last_name: "Doe".to_string(),
}));
// Can ONLY be a Person OR a Company, not both
```

### Example 2: Person and/or Company (anyOf)

```yaml
PersonAndOrCompany:
  anyOf:
    - $ref: '#/components/schemas/Person'
    - $ref: '#/components/schemas/Company'
```

```rust
// Usage
let mut data = PersonAndOrCompany::default();
data.as_Person = Some(Box::new(Person {
    first_name: "John".to_string(),
    last_name: "Doe".to_string(),
}));
data.as_Company = Some(Box::new(Company {
    company_name: "Acme Corp".to_string(),
}));
// Can be BOTH a Person AND a Company simultaneously
data.validate_any_of()?; // Ensures at least one is set
```

### Example 3: Content Types (anyOf)

```yaml
MixedContent:
  anyOf:
    - type: object
      properties:
        text:
          type: string
    - type: object
      properties:
        html:
          type: string
    - type: object
      properties:
        markdown:
          type: string
```

```rust
// Can have multiple content representations
let mut content = MixedContent::default();
content.as_text = Some("Plain text content".to_string());
content.as_html = Some("<p>HTML content</p>".to_string());
content.as_markdown = Some("**Markdown** content".to_string());
// All three formats can coexist
```

## oneOf with Discriminator

When a discriminator is present, `oneOf` generates a tagged enum:

```yaml
ShapeOneOf:
  oneOf:
    - $ref: '#/components/schemas/Circle'
    - $ref: '#/components/schemas/Rectangle'
  discriminator:
    propertyName: shapeType
```

```rust
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "shapeType")]
pub enum ShapeOneOf {
    #[serde(rename = "circle")]
    Circle(Circle),
    #[serde(rename = "rectangle")]
    Rectangle(Rectangle),
}
```

## How Other Languages Handle oneOf/anyOf

### Java (with Gson)
- **oneOf**: Uses `AbstractOpenApiSchema` base class with custom type adapters
- **anyOf**: Similar to oneOf but allows multiple matches in validation
- **Approach**: Runtime type checking with reflection, tries to deserialize into each type
- **Untagged Issue**: Handled via custom `TypeAdapter` that attempts each type sequentially

### TypeScript
- **oneOf**: Simple union types using `|` operator (e.g., `string | number | Person`)
- **anyOf**: Same as oneOf - TypeScript union types
- **Approach**: Type unions are natural in TypeScript, runtime validation depends on library
- **Untagged Issue**: Not an issue - TypeScript's structural typing handles this naturally

### Python (Pydantic)
- **oneOf**: Uses `Union` types with custom validation
- **anyOf**: Separate class with `actual_instance` that validates against multiple schemas
- **Approach**: Runtime validation with explicit checks for which schemas match
- **Untagged Issue**: Custom deserializer tries each type and keeps track of matches

### Go
- **oneOf**: Struct with pointer fields for each option, custom `UnmarshalJSON`
- **anyOf**: Similar structure but allows multiple fields to be non-nil
- **Approach**: All options as pointers, unmarshal attempts to populate each
- **Untagged Issue**: Custom unmarshaler tries each type, oneOf ensures only one succeeds

### C#
- **oneOf**: Uses inheritance with base class and custom JSON converters
- **anyOf**: Similar to oneOf but validation allows multiple matches
- **Approach**: Abstract base class with derived types, custom converters handle deserialization
- **Untagged Issue**: Custom converters attempt deserialization in order

### Comparison with Rust

| Language | oneOf Implementation | anyOf Implementation | Untagged Handling |
|----------|---------------------|---------------------|-------------------|
| **Rust** | Untagged enum | Struct with optional fields | Serde's `#[serde(untagged)]` |
| **Java** | Abstract class + adapters | Abstract class + adapters | Custom TypeAdapter |
| **TypeScript** | Union type `A \| B` | Union type `A \| B` | Native support |
| **Python** | Union with validation | Class with multiple validators | Custom validation |
| **Go** | Struct with pointers | Struct with pointers | Custom UnmarshalJSON |
| **C#** | Base class + converters | Base class + converters | Custom JsonConverter |

### Key Observations

1. **Type System Limitations**: Languages without union types (Java, C#, Go) use wrapper classes/structs
2. **Runtime vs Compile Time**: Most languages handle this at runtime, Rust leverages Serde for compile-time generation
3. **anyOf Semantics**: Only Rust and Python truly differentiate anyOf (multiple matches) from oneOf (single match)
4. **Deserialization Order**: All implementations try options in order for untagged unions, which can lead to ambiguity

## Key Differences Summary

| Aspect | oneOf (XOR) | anyOf (OR) |
|--------|-------------|------------|
| **Rust Type** | Enum | Struct with Optional Fields |
| **Validation** | Exactly one variant | At least one field |
| **Multiple Matches** | Not possible | Allowed |
| **Serde Attribute** | `#[serde(untagged)]` or `#[serde(tag = "...")]` | Standard struct |
| **Use Case** | Mutually exclusive choices | Multiple valid representations |

## Migration from Previous Behavior

Previously, the Rust generator treated `anyOf` the same as `oneOf`, generating enums for both. This was semantically incorrect. With the new implementation:

1. **oneOf remains unchanged**: Still generates enums
2. **anyOf now generates structs**: Breaking change but semantically correct

To migrate existing code:
- Replace enum pattern matching with struct field access
- Use the `validate_any_of()` method to ensure at least one field is set
- Access individual options via the `as_*` fields