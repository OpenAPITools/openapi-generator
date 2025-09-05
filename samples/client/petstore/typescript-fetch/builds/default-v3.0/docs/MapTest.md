
# MapTest


## Properties

Name | Type
------------ | -------------
`mapMapOfString` | { [key: string]: { [key: string]: string; }; }
`mapOfEnumString` | { [key: string]: string; }
`directMap` | { [key: string]: boolean; }
`indirectMap` | { [key: string]: boolean; }

## Example

```typescript
import type { MapTest } from ''

// TODO: Update the object below with actual values
const example = {
  "mapMapOfString": null,
  "mapOfEnumString": null,
  "directMap": null,
  "indirectMap": null,
} satisfies MapTest

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as MapTest
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


