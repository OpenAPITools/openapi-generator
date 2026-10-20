
# StructuredType


## Properties

Name | Type
------------ | -------------
`someString` | string

## Example

```typescript
import type { StructuredType } from ''

// TODO: Update the object below with actual values
const example = {
  "someString": null,
} satisfies StructuredType

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as StructuredType
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


