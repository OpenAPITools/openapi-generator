---
title: Documentation for the cpp-httplib-server Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | cpp-httplib-server | pass this to the generate command after -g |
| generator stability | STABLE | |
| generator type | SERVER | |
| generator language | C++ | |
| generator default templating engine | mustache | |
| helpTxt | Generates a C++ server using the httplib library. | |

## OVERVIEW

The cpp-httplib-server generator creates a modern C++17/20 server implementation using the [httplib](https://github.com/yhirose/cpp-httplib) library. This generator produces:

- **Type-safe API handlers** with std::variant-based response types
- **Model classes** with JSON serialization/deserialization using nlohmann::json
- **CMake build configuration** for easy compilation
- **Automatic error handling** with proper HTTP status codes
- **Namespace organization** for clean code structure

### Key Features

- **Modern C++ Standards**: Uses C++17+ features like std::variant, std::optional
- **Type Safety**: Compile-time type checking for request/response handling
- **JSON Integration**: Built-in JSON serialization with nlohmann::json
- **Error Handling**: Automatic HTTP error response generation
- **Lightweight**: Uses httplib for minimal dependencies
- **Cross-platform**: Works on Windows, Linux, and macOS

## GENERATED CODE STRUCTURE

```
generated/
├── CMakeLists.txt          # Build configuration
├── README.md              # Project documentation
├── api/                   # API handler classes
│   ├── PetApi.h          # API header with virtual methods
│   └── PetApi.cpp        # Route registration and request handling
└── model/                 # Data model classes
    ├── Pet.h             # Model header with JSON methods
    └── Pet.cpp           # Model implementation
```

### Generated API Handler Features
- **Type aliases**: Only generated for operations with response schemas
- **Virtual methods**: Return variants for schema-based operations, void for others
- **Route registration**: Automatic HTTP method and path binding
- **Exception handling**: Built-in JSON parsing and error response generation

## CONFIG OPTIONS

These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
| projectName | Name of the generated C++ project | string | cpp-httplib-server |
| modelNamespace | Namespace for model classes | string | model |
| apiNamespace | Namespace for API classes | string | api |
| enumNamespace | Namespace for enum classes | string | enum |
| licenseHeader | Custom license header for generated files | string | OpenAPI Generator default |
| stripPathFromClassName | Enable path-based class name generation | boolean | false |

## TYPE MAPPINGS

The generator maps OpenAPI types to modern C++ types:

| OpenAPI Type | C++ Type | Notes |
| ------------ | -------- | ----- |
| integer | int | 32-bit signed integer |
| long | long | 64-bit signed integer |
| float | float | 32-bit floating point |
| double | double | 64-bit floating point |
| number | double | Default numeric type |
| boolean | bool | C++ boolean |
| string | std::string | Standard string |
| byte | unsigned char | Single byte |
| binary | std::string | Binary data as string |
| date | std::string | ISO 8601 date string |
| date-time | std::string | ISO 8601 datetime string |
| password | std::string | Password field |
| object | nlohmann::json | Generic JSON object |
| array | std::vector\<T\> | Dynamic array |
| file | std::string | File path or content |

## ADVANCED TYPE FEATURES

### Nullable Types
Nullable properties are automatically wrapped in `std::optional<T>`:
```cpp
std::optional<std::string> optionalField;
```

### Response Types
Each API endpoint generates a type-safe response based on the OpenAPI schema:

**Operations with response schemas:**
```cpp
using PetResponse = std::variant<
    model::Pet,        // 200 success (custom model with schema)
    std::string        // 400 bad request (primitive type with schema)
>;
```

**Operations without response schemas:**
```cpp
// No response type alias generated - handler returns void
virtual void handleDeleteForPet(const model::DeletePetRequest& request) = 0;
```

### Container Types
- **Arrays**: `std::vector<ElementType>`
- **Maps**: `std::map<std::string, ValueType>`
- **oneOf**: `std::variant<Type1, Type2, ...>`
- **anyOf**: `std::any` (for flexible types)

## NAMESPACE ORGANIZATION

The generator creates a clean namespace hierarchy:

```cpp
namespace model {
    class Pet { /* ... */ };
    class User { /* ... */ };
}

namespace api {
    class PetApi { /* ... */ };
    class UserApi { /* ... */ };
}
```

## ERROR HANDLING INNOVATIONS

### Schema-Based Response Generation
The generator only includes responses that have defined schemas in the OpenAPI specification:

- **With custom schema**: Uses the specific model type (e.g., `model::ErrorResponse`)
- **With primitive schema**: Uses the primitive C++ type (e.g., `std::string`, `int`)
- **Without schema**: No response type generated - handler returns `void`

This approach ensures:
- **Type Safety**: Only meaningful data types are included in variants
- **Simplicity**: No artificial wrapper types for schema-less responses
- **Clarity**: Handlers only deal with actual response data structures defined in the API

### Response Handler Generation
- **Operations with schemas**: Generate `std::variant` return types and handler methods
- **Operations without schemas**: Generate `void` handler methods with automatic status code handling
- **Mixed operations**: Only schema-defined responses appear in the variant

### Automatic Exception Handling
Generated route handlers include comprehensive exception handling:
- JSON parse errors → 400 Bad Request
- General exceptions → 500 Internal Server Error
- Custom error responses based on OpenAPI spec

### Handler Method Patterns

**Variant-based handlers** (operations with response schemas):
```cpp
PetResponse handleGetForPet(const model::GetPetRequest& request) override {
    // Return success type or error types from the variant
    return model::Pet{};  // or return std::string{"error message"};
}
```

**Void handlers** (operations without response schemas):
```cpp
void handleDeleteForPet(const model::DeletePetRequest& request) override {
    // No response schemas defined - HTTP status handled automatically
    // Delete operation logic here
    deletePetFromDatabase(request.getPetId());
}
```

## CUSTOM MODIFICATIONS

### Path-Based Class Naming
When `stripPathFromClassName=true`, the generator intelligently extracts class names from API paths:

- `/api/v1/pets/{id}` → `PetsApi`, `handleGetForPets()`
- `/users/profile` → `ProfileApi`, `handlePostForProfile()`

### String Conversion Enhancements
The generator includes robust string conversion utilities:
- Handles camelCase, PascalCase, snake_case
- Splits on multiple delimiters: `/`, `_`, `-`, spaces, camelCase boundaries
- Sanitizes invalid characters for C++ identifiers

### Model Preprocessing
Automatic schema title generation for better class names:
- Inline request schemas: `{operationId}Request`
- Inline response schemas: `{operationId}Response{statusCode}`
- Nested object properties: `{parentName}_{propertyName}`

## IMPORT MAPPING

Standard C++ library includes are automatically generated:

| C++ Type | Include |
| -------- | ------- |
| std::string | #include \<string\> |
| std::vector | #include \<vector\> |
| std::map | #include \<map\> |
| std::optional | #include \<optional\> |
| std::variant | #include \<variant\> |
| nlohmann::json | (external dependency) |

## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |
| ResponseType | std::variant\<SuccessType, SchemaErrorTypes...\> (only when schemas exist) |
| ModelType | Generated C++ class with JSON methods |
| HandlerType | Virtual function returning ResponseType or void |

**Note**: Response type aliases are only generated for operations that have at least one response with a schema. Operations without response schemas generate void handler methods.

## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>bool</li>
<li>char</li>
<li>double</li>
<li>float</li>
<li>int</li>
<li>long</li>
<li>size_t</li>
<li>std::any</li>
<li>std::deque</li>
<li>std::list</li>
<li>std::map</li>
<li>std::optional</li>
<li>std::pair</li>
<li>std::queue</li>
<li>std::set</li>
<li>std::stack</li>
<li>std::string</li>
<li>std::tuple</li>
<li>std::unordered_map</li>
<li>std::unordered_set</li>
<li>std::variant</li>
<li>std::vector</li>
<li>unsigned char</li>
<li>unsigned int</li>
<li>unsigned long</li>
<li>void</li>
</ul>

## RESERVED WORDS

<ul class="column-ul">
<li>alignas</li>
<li>alignof</li>
<li>and</li>
<li>and_eq</li>
<li>asm</li>
<li>auto</li>
<li>bitand</li>
<li>bitor</li>
<li>bool</li>
<li>break</li>
<li>case</li>
<li>catch</li>
<li>char</li>
<li>char16_t</li>
<li>char32_t</li>
<li>class</li>
<li>compl</li>
<li>concept</li>
<li>const</li>
<li>const_cast</li>
<li>constexpr</li>
<li>continue</li>
<li>decltype</li>
<li>default</li>
<li>delete</li>
<li>do</li>
<li>double</li>
<li>dynamic_cast</li>
<li>else</li>
<li>enum</li>
<li>explicit</li>
<li>export</li>
<li>extern</li>
<li>false</li>
<li>float</li>
<li>for</li>
<li>friend</li>
<li>goto</li>
<li>if</li>
<li>inline</li>
<li>int</li>
<li>linux</li>
<li>long</li>
<li>mutable</li>
<li>namespace</li>
<li>new</li>
<li>noexcept</li>
<li>not</li>
<li>not_eq</li>
<li>nullptr</li>
<li>operator</li>
<li>or</li>
<li>or_eq</li>
<li>private</li>
<li>protected</li>
<li>public</li>
<li>register</li>
<li>reinterpret_cast</li>
<li>requires</li>
<li>return</li>
<li>short</li>
<li>signed</li>
<li>sizeof</li>
<li>static</li>
<li>static_assert</li>
<li>static_cast</li>
<li>struct</li>
<li>switch</li>
<li>template</li>
<li>this</li>
<li>thread_local</li>
<li>throw</li>
<li>true</li>
<li>try</li>
<li>typedef</li>
<li>typeid</li>
<li>typename</li>
<li>union</li>
<li>unsigned</li>
<li>using</li>
<li>virtual</li>
<li>void</li>
<li>volatile</li>
<li>wchar_t</li>
<li>while</li>
<li>xor</li>
<li>xor_eq</li>
</ul>

## USAGE EXAMPLE

### 1. Generate Server Code
```bash
openapi-generator generate \
  -i petstore.yaml \
  -g cpp-httplib-server \
  -o ./generated \
  --additional-properties=projectName=PetStore,modelNamespace=model,apiNamespace=api
```

### 2. Implement Handler
```cpp
#include "api/PetApi.h"

class PetApiImpl : public api::PetApi {
public:
    // Handler for operation with response schemas (variant return type)
    PetResponse handleGetForPet(const model::GetPetRequest& request) override {
        try {
            model::Pet pet;
            pet.setId(request.getPetId());
            pet.setName("Fluffy");
            return pet;  // 200 OK with Pet model
        } catch (const std::invalid_argument& e) {
            return std::string("Invalid pet ID");  // 400 with string schema
        }
    }
    
    // Handler for operation without response schemas (void return type)
    void handleDeleteForPet(const model::DeletePetRequest& request) override {
        // Delete the pet from database - no response schema defined
        deletePetFromDatabase(request.getPetId());
        // HTTP status handled automatically by framework
    }
};
```

### 3. Start Server
```cpp
#include <httplib.h>
#include "api/PetApi.h"

int main() {
    httplib::Server server;
    PetApiImpl petApi;
    
    petApi.RegisterRoutes(server);
    
    server.listen("localhost", 8080);
    return 0;
}
```

## DEPENDENCIES

### Required
- **C++17 or later**: For std::variant, std::optional
- **nlohmann/json**: For JSON serialization
- **httplib**: For HTTP server functionality

### CMake Integration
The generated CMakeLists.txt handles dependency management:
```cmake
find_package(nlohmann_json REQUIRED)
find_package(httplib REQUIRED)

target_link_libraries(${PROJECT_NAME} 
    nlohmann_json::nlohmann_json
    httplib::httplib
)
```

## FEATURE SET

### Client Modification Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasePath|✗|ToolingExtension
|Authorizations|✗|ToolingExtension
|UserAgent|✗|ToolingExtension
|MockServer|✗|ToolingExtension

### Data Type Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Custom|✓|OAS2,OAS3
|Int32|✓|OAS2,OAS3
|Int64|✓|OAS2,OAS3
|Float|✓|OAS2,OAS3
|Double|✓|OAS2,OAS3
|Decimal|✓|ToolingExtension
|String|✓|OAS2,OAS3
|Byte|✓|OAS2,OAS3
|Binary|✓|OAS2,OAS3
|Boolean|✓|OAS2,OAS3
|Date|✓|OAS2,OAS3
|DateTime|✓|OAS2,OAS3
|Password|✓|OAS2,OAS3
|File|✓|OAS2
|Uuid|✗|
|Array|✓|OAS2,OAS3
|Null|✓|OAS3
|AnyType|✓|OAS2,OAS3
|Object|✓|OAS2,OAS3
|Maps|✓|ToolingExtension
|CollectionFormat|✓|OAS2
|CollectionFormatMulti|✓|OAS2
|Enum|✓|OAS2,OAS3
|ArrayOfEnum|✓|ToolingExtension
|ArrayOfModel|✓|ToolingExtension
|ArrayOfCollectionOfPrimitives|✓|ToolingExtension
|ArrayOfCollectionOfModel|✓|ToolingExtension
|ArrayOfCollectionOfEnum|✓|ToolingExtension
|MapOfEnum|✓|ToolingExtension
|MapOfModel|✓|ToolingExtension
|MapOfCollectionOfPrimitives|✓|ToolingExtension
|MapOfCollectionOfModel|✓|ToolingExtension
|MapOfCollectionOfEnum|✓|ToolingExtension

### Documentation Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Readme|✓|ToolingExtension
|Model|✓|ToolingExtension
|Api|✓|ToolingExtension

### Global Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Host|✓|OAS2,OAS3
|BasePath|✓|OAS2,OAS3
|Info|✓|OAS2,OAS3
|Schemes|✗|OAS2,OAS3
|PartialSchemes|✓|OAS2,OAS3
|Consumes|✓|OAS2
|Produces|✓|OAS2
|ExternalDocumentation|✓|OAS2,OAS3
|Examples|✓|OAS2,OAS3
|XMLStructureDefinitions|✗|OAS2,OAS3
|MultiServer|✗|OAS3
|ParameterizedServer|✗|OAS3
|ParameterStyling|✗|OAS3
|Callbacks|✗|OAS3
|LinkObjects|✗|OAS3

### Parameter Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Path|✓|OAS2,OAS3
|Query|✓|OAS2,OAS3
|Header|✓|OAS2,OAS3
|Body|✓|OAS2
|FormUnencoded|✓|OAS2
|FormMultipart|✓|OAS2
|Cookie|✗|OAS3

### Schema Support Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Simple|✓|OAS2,OAS3
|Composite|✓|OAS2,OAS3
|Polymorphism|✗|OAS2,OAS3
|Union|✓|OAS3
|allOf|✓|OAS2,OAS3
|anyOf|✓|OAS3
|oneOf|✓|OAS3
|not|✗|OAS3

### Security Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasicAuth|✗|OAS2,OAS3
|ApiKey|✗|OAS2,OAS3
|OpenIDConnect|✗|OAS3
|BearerToken|✗|OAS3
|OAuth2_Implicit|✗|OAS2,OAS3
|OAuth2_Password|✗|OAS2,OAS3
|OAuth2_ClientCredentials|✗|OAS2,OAS3
|OAuth2_AuthorizationCode|✗|OAS2,OAS3
|SignatureAuth|✗|OAS3
|AWSV4Signature|✗|ToolingExtension

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✓|OAS2,OAS3
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3

## ROADMAP

Future enhancements planned:
- Input validation with custom validators
- Authentication middleware integration
- OpenAPI 3.1 support
- Performance optimizations
- WebSocket support consideration
