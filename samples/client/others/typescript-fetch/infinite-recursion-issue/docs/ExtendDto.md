
# ExtendDto


## Properties

Name | Type
------------ | -------------
`someItems` | [Array&lt;TestBaseDto&gt;](TestBaseDto.md)

## Example

```typescript
import type { ExtendDto } from ''

// TODO: Update the object below with actual values
const example = {
  "someItems": null,
} satisfies ExtendDto

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ExtendDto
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


