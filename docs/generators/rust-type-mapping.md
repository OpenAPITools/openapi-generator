# Rust Generator Type Mapping Documentation

## Overview

This document comprehensively describes how the OpenAPI Generator maps OpenAPI/JSON Schema types to Rust types, including the current implementation decisions, alternatives considered, and future improvements.

## Current Implementation (As of v7.16.0)

### Basic Type Mappings

| OpenAPI Type | Format | Rust Type | Notes |
|-------------|---------|-----------|-------|
| `integer` | `int32` | `i32` | Default signed 32-bit integer |
| `integer` | `int64` | `i64` | Signed 64-bit integer |
| `integer` | (none) | `i32` | Default when no format specified |
| `integer` | (with minimum >= 0) | `u32` or `u64` | When `preferUnsignedInt=true` |
| `number` | `float` | `f32` | 32-bit floating point |
| `number` | `double` | `f64` | 64-bit floating point |
| `number` | (none) | `f32` | Default when no format specified |
| `string` | (none) | `String` | Heap-allocated string |
| `string` | `byte` | `Vec<u8>` | Base64 encoded bytes |
| `string` | `binary` | `Vec<u8>` | Binary data |
| `string` | `date` | `String` | ISO 8601 date (could use chrono::NaiveDate) |
| `string` | `date-time` | `String` | ISO 8601 datetime (could use chrono::DateTime) |
| `string` | `password` | `String` | Same as regular string |
| `string` | `uuid` | `uuid::Uuid` | When uuid crate is included |
| `boolean` | (none) | `bool` | Boolean value |
| `array` | (none) | `Vec<T>` | Dynamic array of type T |
| `object` | (none) | `HashMap<String, T>` | When additionalProperties defined |
| `object` | (none) | Generated struct | When properties are defined |

### Complex Schema Handling

#### Regular Objects
```yaml
# OpenAPI Schema
Person:
  type: object
  properties:
    name:
      type: string
    age:
      type: integer
  required:
    - name
```

```rust
// Generated Rust Code
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Person {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub age: Option<i32>,
}
```

#### Enums
```yaml
# OpenAPI Schema
Status:
  type: string
  enum:
    - pending
    - approved
    - rejected
```

```rust
// Generated Rust Code
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub enum Status {
    #[serde(rename = "pending")]
    Pending,
    #[serde(rename = "approved")]
    Approved,
    #[serde(rename = "rejected")]
    Rejected,
}
```

#### oneOf (Exclusive Choice - XOR)
```yaml
# OpenAPI Schema
Pet:
  oneOf:
    - $ref: '#/components/schemas/Cat'
    - $ref: '#/components/schemas/Dog'
```

```rust
// Generated Rust Code (without discriminator)
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Pet {
    Cat(Box<Cat>),
    Dog(Box<Dog>),
}

// Generated Rust Code (with discriminator)
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "petType")]
pub enum Pet {
    #[serde(rename = "cat")]
    Cat(Box<Cat>),
    #[serde(rename = "dog")]
    Dog(Box<Dog>),
}
```

#### anyOf (Currently treated as oneOf)
```yaml
# OpenAPI Schema
StringOrNumber:
  anyOf:
    - type: string
    - type: number
```

```rust
// Current Generated Rust Code (INCORRECT SEMANTICS)
// After PR #21896 - treats anyOf as oneOf
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum StringOrNumber {
    String(String),
    Number(f32),
}
```

**Problem**: This implementation enforces XOR semantics (exactly one must match) instead of OR semantics (one or more can match).

#### allOf (Not Supported)
```yaml
# OpenAPI Schema
Employee:
  allOf:
    - $ref: '#/components/schemas/Person'
    - type: object
      properties:
        employeeId:
          type: string
        department:
          type: string
```

```rust
// Option 1: What SHOULD ideally be generated (flattened struct)
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Employee {
    // Fields from Person
    pub name: String,
    pub age: Option<i32>,
    // Additional fields
    pub employee_id: String,
    pub department: String,
}

// Option 2: Alternative with composition (avoiding property conflicts)
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Employee {
    #[serde(flatten)]
    pub person: Person,
    
    #[serde(flatten)]
    pub object_1: EmployeeObject1,  // Anonymous object gets generated name
}

// Generated for the anonymous object in allOf
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct EmployeeObject1 {
    pub employee_id: String,
    pub department: String,
}
```

**Current behavior**: The generator fails or produces incorrect output for allOf schemas.

**Note**: Option 2 avoids property name conflicts that could occur if multiple schemas in allOf define the same property names. This approach maintains type safety while preserving the composition structure.

### Nullable Handling

```yaml
# OpenAPI 3.0 nullable
name:
  type: string
  nullable: true

# OpenAPI 3.1 nullable
name:
  type: ["string", "null"]
```

```rust
// Generated Rust Code
pub name: Option<String>,
```

### Box Usage for Recursive Types

The generator uses `Box<T>` to handle recursive types and prevent infinite size structs:

```rust
// Without avoidBoxedModels (default)
pub struct Node {
    pub children: Option<Vec<Box<Node>>>,  // Box prevents infinite size
}

// With avoidBoxedModels=true
pub struct Node {
    pub children: Option<Vec<Node>>,  // Will fail to compile if truly recursive
}
```

## Alternatives Considered (But Not Implemented)

### Alternative 1: Composition-based allOf Support

For allOf schemas, instead of trying to flatten all properties into a single struct (which can cause naming conflicts), use composition with serde's flatten:

```rust
// Alternative: Composition approach for allOf
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Employee {
    #[serde(flatten)]
    pub person: Person,
    
    #[serde(flatten)]
    pub additional_props: EmployeeAdditional,
}

// Benefits:
// 1. Avoids property name conflicts
// 2. Maintains clear composition structure
// 3. Works with serde's existing flatten feature
// 4. Each component can be validated independently

// Challenges:
// 1. Generated names for anonymous objects (Object$1, Object$2, etc.)
// 2. Requires serde flatten support
// 3. May need special handling for required vs optional fields
```

**Pros**: 
- Avoids property naming conflicts
- Clear composition structure
- Leverages serde's existing features

**Cons**: 
- Generated names for anonymous schemas
- More complex serialization
- Potential issues with nested flattening

### Alternative 2: True anyOf Support with Validation Trait

Instead of treating anyOf as oneOf, we could generate a validation trait:

```rust
// Alternative: Validation trait approach
pub trait ValidatesAnyOf {
    fn validate_schemas(&self) -> Vec<bool>;
    fn is_valid(&self) -> bool {
        self.validate_schemas().iter().any(|&v| v)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum StringOrNumber {
    String(String),
    Number(f32),
    Both { string_val: String, number_val: f32 }, // Allows both!
}

impl ValidatesAnyOf for StringOrNumber {
    fn validate_schemas(&self) -> Vec<bool> {
        match self {
            Self::String(_) => vec![true, false],
            Self::Number(_) => vec![false, true],
            Self::Both { .. } => vec![true, true],
        }
    }
}
```

**Pros**: Semantically correct
**Cons**: Complex, requires custom serde implementation

### Alternative 2: Tagged Unions for Better Error Messages

Instead of untagged enums, use internally tagged enums:

```rust
// Alternative: Internally tagged
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum Pet {
    #[serde(rename = "cat")]
    Cat { name: String, meow_volume: i32 },
    #[serde(rename = "dog")]
    Dog { name: String, bark_volume: i32 },
}
```

**Pros**: Better error messages, explicit type discrimination
**Cons**: Requires discriminator in schema, changes JSON structure

### Alternative 3: Wrapper Types for Semantic Clarity

```rust
// Alternative: Wrapper types
pub struct AnyOf<T>(pub T);
pub struct OneOf<T>(pub T);
pub struct AllOf<T>(pub T);

impl<T> AnyOf<T> {
    pub fn validate(&self) -> Result<(), ValidationError> {
        // Custom validation logic
    }
}
```

**Pros**: Clear semantic intent
**Cons**: Additional complexity, ergonomics issues

### Alternative 4: Macro-based Generation

```rust
// Alternative: Procedural macros
#[derive(OpenApiSchema)]
#[openapi(any_of = ["String", "Number"])]
pub struct StringOrNumber {
    #[openapi(schema = 0)]
    as_string: Option<String>,
    #[openapi(schema = 1)]
    as_number: Option<f32>,
}
```

**Pros**: Compile-time validation, flexible
**Cons**: Requires proc-macro crate, compilation overhead

## Configuration Options Impact

### preferUnsignedInt
When `true`, integers with `minimum >= 0` generate unsigned types:
```rust
// preferUnsignedInt=false (default)
pub age: i32,  // Even with minimum: 0

// preferUnsignedInt=true
pub age: u32,  // When minimum: 0
```

### bestFitInt
When `true`, chooses the smallest integer type that fits the constraints:
```rust
// bestFitInt=false (default)
pub small_number: i32,  // Even with min:0, max:100

// bestFitInt=true
pub small_number: u8,   // Fits in u8 (0-255)
```

### avoidBoxedModels
Controls whether to use `Box<T>` for nested models:
```rust
// avoidBoxedModels=false (default)
pub nested: Box<NestedModel>,

// avoidBoxedModels=true
pub nested: NestedModel,
```

## Known Limitations and Issues

1. **anyOf Semantics**: Currently generates XOR (enum) instead of OR (multiple valid)
2. **allOf Not Supported**: Cannot compose multiple schemas
3. **Discriminator Limitations**: Only supports property-based discriminators
4. **Date/Time Types**: Uses String instead of chrono types
5. **Validation**: No runtime validation generated
6. **Integer Overflow**: No automatic BigInteger support for large numbers
7. **Pattern Properties**: Not supported for dynamic object keys
8. **JSON Schema Keywords**: Many keywords ignored (minLength, pattern, etc.)

## Why These Decisions Were Made

### Why anyOf → oneOf Conversion?

The initial implementation chose to convert anyOf to oneOf because:

1. **Serde Limitations**: Rust's serde library naturally supports tagged/untagged enums for "one of" semantics
2. **Type Safety**: Rust's type system prefers sum types (enums) over union types
3. **Ergonomics**: Enums with pattern matching are idiomatic in Rust
4. **Complexity**: True anyOf support requires complex validation logic

### Why No allOf Support?

1. **Composition Complexity**: Rust doesn't have built-in struct composition/inheritance
2. **Serde Challenges**: While serde supports `#[serde(flatten)]`, handling it in code generation is complex
3. **Conflicting Fields**: When multiple schemas define the same property names, resolution is non-trivial:
   - Option A: Merge and error on conflicts (strict)
   - Option B: Last-wins override (loose but surprising)
   - Option C: Composition with generated names (Object$1, Object$2) - avoids conflicts but less ergonomic
4. **Anonymous Schema Naming**: allOf often contains inline anonymous schemas that need generated names
5. **Priority**: Less commonly used than oneOf in practice

## To-Be: Proposed Changes (PR #21915)

### True anyOf Support with Struct-Based Approach

Instead of converting anyOf to oneOf (enum), generate structs with optional fields:

#### Current (Incorrect)
```yaml
# Schema
ModelIdentifier:
  anyOf:
    - type: string
      description: Any model name
    - type: string
      enum: [gpt-4, gpt-3.5-turbo]
      description: Known models
```

```rust
// Current: WRONG - Generates enum (XOR semantics)
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ModelIdentifier {
    String(String),  // Problem: Duplicate variant names!
    String(String),  // This won't even compile!
}
```

#### Proposed (Correct)
```rust
// Proposed: Generates struct with optional fields (OR semantics)
#[derive(Clone, Default, Debug, PartialEq, Serialize, Deserialize)]
pub struct ModelIdentifier {
    /// Any model name as string (anyOf option)
    #[serde(skip_serializing_if = "Option::is_none", rename = "as_any_of_0")]
    pub as_any_of_0: Option<String>,
    
    /// Known model enum values (anyOf option)
    #[serde(skip_serializing_if = "Option::is_none", rename = "as_any_of_1")]
    pub as_any_of_1: Option<String>,
}

impl ModelIdentifier {
    /// Creates a new ModelIdentifier with all fields set to None
    pub fn new() -> Self {
        Default::default()
    }
    
    /// Validates that at least one anyOf field is set (OR semantics)
    pub fn validate_any_of(&self) -> Result<(), String> {
        if self.as_any_of_0.is_none() && self.as_any_of_1.is_none() {
            return Err("At least one anyOf field must be set".to_string());
        }
        Ok(())
    }
}
```

### Implementation Details

1. **Java Generator Changes** (`RustClientCodegen.java`):
   - Keep anyOf schemas separate from oneOf
   - Process anyOf with different template logic
   - Generate appropriate field names for anyOf options

2. **Mustache Template Changes** (`model.mustache`):
   - Add new anyOf section that generates structs
   - Include validation method for OR semantics
   - Use optional fields with serde skip attributes

3. **Benefits**:
   - **Semantically Correct**: Properly implements OR semantics
   - **No Compilation Errors**: Avoids duplicate enum variant issues
   - **Validation Support**: Includes validation method
   - **Extensible**: Can add more sophisticated validation later

4. **Trade-offs**:
   - **Breaking Change**: Existing code using anyOf will break
   - **Less Idiomatic**: Structs with optional fields less elegant than enums
   - **Manual Validation**: Requires calling validate_any_of() explicitly
   - **Serialization Complexity**: Multiple fields vs single value

### Migration Path

For users currently relying on anyOf → oneOf conversion:

```rust
// Old code (with enum)
match model_identifier {
    ModelIdentifier::Variant1(s) => println!("String: {}", s),
    ModelIdentifier::Variant2(n) => println!("Number: {}", n),
}

// New code (with struct)
if let Some(s) = &model_identifier.as_any_of_0 {
    println!("String: {}", s);
}
if let Some(n) = &model_identifier.as_any_of_1 {
    println!("Number: {}", n);
}
```

### Future Improvements

1. **Phase 1** (Current PR): Basic struct-based anyOf support
2. **Phase 2**: Add allOf support using struct flattening
3. **Phase 3**: Improve validation with custom derive macros
4. **Phase 4**: Add discriminator support for anyOf
5. **Phase 5**: Consider union type alternatives when Rust supports them

## Conclusion

The current Rust generator makes pragmatic choices favoring simplicity and Rust idioms over strict OpenAPI compliance. The proposed changes in PR #21915 move toward semantic correctness while maintaining reasonable ergonomics. Future work should focus on incremental improvements guided by user needs and Rust ecosystem evolution.