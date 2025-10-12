
# ChildWithNullable


## Properties

Name | Type
------------ | -------------
`otherProperty` | string

## Example

```typescript
import type { ChildWithNullable } from ''

// TODO: Update the object below with actual values
const example = {
  "otherProperty": null,
} satisfies ChildWithNullable

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ChildWithNullable
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


