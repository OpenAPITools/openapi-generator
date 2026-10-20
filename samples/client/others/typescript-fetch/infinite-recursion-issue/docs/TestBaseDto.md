
# TestBaseDto


## Properties

Name | Type
------------ | -------------
`something` | string
`testObjectType` | [TestObjectType](TestObjectType.md)

## Example

```typescript
import type { TestBaseDto } from ''

// TODO: Update the object below with actual values
const example = {
  "something": null,
  "testObjectType": null,
} satisfies TestBaseDto

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as TestBaseDto
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


