
# Dog


## Properties

Name | Type
------------ | -------------
`breed` | string

## Example

```typescript
import type { Dog } from ''

// TODO: Update the object below with actual values
const example = {
  "breed": null,
} satisfies Dog

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Dog
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


