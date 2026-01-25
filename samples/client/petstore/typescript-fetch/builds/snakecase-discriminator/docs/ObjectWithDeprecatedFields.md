
# ObjectWithDeprecatedFields


## Properties

Name | Type
------------ | -------------
`uuid` | string
`id` | number
`deprecatedRef` | [DeprecatedObject](DeprecatedObject.md)
`bars` | Array&lt;string&gt;

## Example

```typescript
import type { ObjectWithDeprecatedFields } from ''

// TODO: Update the object below with actual values
const example = {
  "uuid": null,
  "id": null,
  "deprecatedRef": null,
  "bars": null,
} satisfies ObjectWithDeprecatedFields

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ObjectWithDeprecatedFields
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


