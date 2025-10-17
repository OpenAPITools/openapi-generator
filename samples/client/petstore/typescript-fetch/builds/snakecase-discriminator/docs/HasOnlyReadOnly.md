
# HasOnlyReadOnly


## Properties

Name | Type
------------ | -------------
`bar` | string
`foo` | string

## Example

```typescript
import type { HasOnlyReadOnly } from ''

// TODO: Update the object below with actual values
const example = {
  "bar": null,
  "foo": null,
} satisfies HasOnlyReadOnly

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as HasOnlyReadOnly
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


