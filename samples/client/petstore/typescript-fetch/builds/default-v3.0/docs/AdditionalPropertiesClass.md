
# AdditionalPropertiesClass


## Properties

Name | Type
------------ | -------------
`mapProperty` | { [key: string]: string; }
`mapOfMapProperty` | { [key: string]: { [key: string]: string; }; }

## Example

```typescript
import type { AdditionalPropertiesClass } from ''

// TODO: Update the object below with actual values
const example = {
  "mapProperty": null,
  "mapOfMapProperty": null,
} satisfies AdditionalPropertiesClass

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as AdditionalPropertiesClass
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


