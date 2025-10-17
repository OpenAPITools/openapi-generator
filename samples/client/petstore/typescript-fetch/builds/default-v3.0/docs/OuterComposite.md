
# OuterComposite


## Properties

Name | Type
------------ | -------------
`myNumber` | number
`myString` | string
`myBoolean` | boolean

## Example

```typescript
import type { OuterComposite } from ''

// TODO: Update the object below with actual values
const example = {
  "myNumber": null,
  "myString": null,
  "myBoolean": null,
} satisfies OuterComposite

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as OuterComposite
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


