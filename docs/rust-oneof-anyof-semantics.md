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

## Should Untagged Enums Be Allowed? A Spec Analysis

### The Discriminator Dilemma

The OpenAPI specification states:

> "To support polymorphism, the OpenAPI Specification adds the discriminator field. When used, the discriminator will be the name of the property that decides which schema definition validates the structure of the model. As such, the discriminator field MUST be a required field."

However, this raises important questions:

1. **Is discriminator required for all oneOf schemas?** No, the spec says "when used" - it's optional.
2. **Does oneOf without discriminator violate the spec?** No, but it may violate the intent.

### JSON Schema vs OpenAPI Semantics

**JSON Schema requirement** (which OpenAPI inherits):
- oneOf: "The given data must be valid against **exactly one** of the given subschemas"
- This requires checking ALL subschemas to ensure only one matches

**Implementation reality**:
- Most generators use "first match wins" for untagged unions
- This violates the strict oneOf semantics unless additional validation is performed

### The Case for Validation Errors

**You're correct that strictly speaking, generators should validate that exactly one schema matches for oneOf.** This means:

1. **Untagged enums are technically non-compliant** if they don't validate exclusivity
2. **Validation errors should be thrown** when multiple schemas match
3. **"First match wins" is a pragmatic compromise** that violates the spec

### Current Implementations vs Spec Compliance

| Approach | Spec Compliant? | Used By |
|----------|----------------|---------|
| First match wins (no validation) | ❌ No | Rust, Java, C# |
| Validate exactly one matches | ✅ Yes | Python (Pydantic) |
| Require discriminator | ✅ Yes (conservative) | None (but recommended) |
| Generate error for ambiguous schemas | ✅ Yes (conservative) | None currently |

### Implications for Rust Implementation

The current Rust implementation using untagged enums is **pragmatic but not strictly compliant** because:

1. Serde's `#[serde(untagged)]` stops at first match
2. No validation that other variants wouldn't also match
3. Could silently accept invalid data that matches multiple schemas

**To be fully compliant**, Rust would need to:
```rust
// Validate against all variants
impl<'de> Deserialize<'de> for OneOfExample {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> {
        let value = Value::deserialize(deserializer)?;
        let mut matches = 0;
        
        if let Ok(_) = Type1::deserialize(&value) { matches += 1; }
        if let Ok(_) = Type2::deserialize(&value) { matches += 1; }
        
        if matches != 1 {
            return Err(Error::custom("Must match exactly one schema"));
        }
        
        // Then do actual deserialization
    }
}
```

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

## Ambiguity Handling Strategies

### Do Any Languages Refuse to Generate?

**No language generator completely refuses to generate code for ambiguous schemas.** All have fallback strategies:

### Language-Specific Ambiguity Handling

#### **Swift**
- **Strategy**: Provides `oneOfUnknownDefaultCase` option
- **Behavior**: Can generate an `unknownDefaultOpenApi` case for unmatched values
- **Without Option**: Throws `DecodingError.typeMismatch` at runtime
- **Philosophy**: Fail at runtime rather than compile time

#### **Python (Pydantic)**
- **Strategy**: Generates validation code with `ValidationError`
- **Behavior**: Validates all options and tracks which ones match
- **For oneOf**: Ensures exactly one matches, raises `ValidationError` if multiple match
- **Philosophy**: Strict runtime validation with clear error messages

#### **Java**
- **Strategy**: Custom TypeAdapters try each type sequentially
- **Behavior**: First successful deserialization wins
- **Ambiguity**: No validation that only one matches for oneOf
- **Philosophy**: Pragmatic "first match wins" approach

#### **TypeScript**
- **Strategy**: Union types with no runtime validation by default
- **Behavior**: Structural typing means any matching shape is accepted
- **Ambiguity**: Completely permissive - type system doesn't enforce exclusivity
- **Philosophy**: Trust the data or add runtime validation separately

#### **Go**
- **Strategy**: Custom UnmarshalJSON tries to populate all fields
- **Behavior**: For oneOf, additional validation ensures only one is non-nil
- **Ambiguity**: Returns error if multiple match for oneOf
- **Philosophy**: Explicit validation after unmarshaling

#### **Rust**
- **Strategy**: Untagged enums for oneOf, struct with options for anyOf
- **Behavior**: Serde tries variants in order (first match wins for oneOf)
- **Ambiguity**: No compile-time detection of overlapping variants
- **Philosophy**: Leverage existing serialization framework

### Common Warnings and Limitations

From the OpenAPI Generator codebase:

1. **Self-referencing schemas**: Detected and removed to prevent infinite loops
2. **Inline objects in oneOf with discriminator**: Warned and ignored
3. **Conflicting composition**: Error logged when schema has incorrect anyOf/allOf/oneOf combination
4. **Missing discriminator**: Most generators work but with "first match wins" semantics

### Best Practices for Avoiding Ambiguity

1. **Use discriminators**: When possible, add a discriminator property for oneOf
   ```yaml
   oneOf:
     - $ref: '#/components/schemas/Cat'
     - $ref: '#/components/schemas/Dog'
   discriminator:
     propertyName: petType
   ```

2. **Make schemas mutually exclusive**: Design schemas that don't overlap
   ```yaml
   oneOf:
     - type: object
       required: [foo]
       properties:
         foo: {type: string}
     - type: object
       required: [bar]
       properties:
         bar: {type: number}
   ```

3. **Order matters**: Place more specific schemas first
   ```yaml
   oneOf:
     - type: object
       required: [a, b, c]  # More specific
     - type: object
       required: [a]        # Less specific
   ```

### Why Don't Generators Refuse?

1. **Pragmatism**: Real-world APIs often have imperfect schemas
2. **Backwards compatibility**: Existing APIs shouldn't break
3. **Runtime nature**: Many ambiguities only manifest with specific data
4. **User choice**: Developers can add additional validation if needed
5. **OpenAPI spec**: The spec itself doesn't forbid ambiguous schemas

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

## Real Example: Wing328's Test Case

I merged wing328's PR #21911 which has a perfect test case showing the difference. Let me walk you through what I found:

### The Test Schema
Wing328 created this anyOf schema:
```yaml
ModelIdentifier:
  description: Model identifier that can be a string or specific enum value
  anyOf:
    - type: string
      description: Any model name as string
    - type: string
      enum: [gpt-4, gpt-3.5-turbo, dall-e-3]
      description: Known model enum values
```

### What the Old Generator Would Produce
With the old (wrong) behavior, this would generate:
```rust
// OLD: Incorrectly treats anyOf as oneOf
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ModelIdentifier {
    String(String),  // First string option
    String(String),  // Second string option - DUPLICATE! This is broken!
}
```

See the problem? We'd have duplicate enum variants! The generator would actually produce invalid Rust code. Plus, even if it worked, you could only choose ONE option, not both.

### What Our New Generator Produces
With the correct anyOf implementation:
```rust
// NEW: Correctly treats anyOf as composition
#[derive(Clone, Default, Debug, PartialEq, Serialize, Deserialize)]
pub struct ModelIdentifier {
    #[serde(skip_serializing_if = "Option::is_none", rename = "as_any_of_0")]
    pub as_any_of_0: Option<String>,  // Any model name
    
    #[serde(skip_serializing_if = "Option::is_none", rename = "as_any_of_1")]
    pub as_any_of_1: Option<String>,  // Known enum values
}
```

Now both fields can be set! This is actually useful - imagine an API that accepts both a freeform model name AND validates against known models. With anyOf, you can validate against both schemas simultaneously.

### Another Example from Wing328's Tests
He also included this more complex anyOf:
```yaml
AnotherAnyOfTest:
  anyOf:
    - type: string
    - type: integer
    - type: array
      items:
        type: string
```

Old behavior would force you to choose: "Is this a string OR an integer OR an array?"

New behavior lets you have all three! Maybe it's a weird API, but that's what anyOf means - the data can match multiple schemas at once. The generator shouldn't make assumptions about what's "sensible" - it should implement the spec correctly.