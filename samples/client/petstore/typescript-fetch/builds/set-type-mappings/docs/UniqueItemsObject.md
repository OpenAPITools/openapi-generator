
# UniqueItemsObject


## Properties

Name | Type
------------ | -------------
`uniqueStrings` | Array&lt;string&gt;
`uniqueIntegers` | Array&lt;number&gt;
`uniqueNumbers` | Array&lt;number&gt;
`uniqueBooleans` | Array&lt;boolean&gt;
`uniqueObjects` | Array&lt;object&gt;
`uniqueArrays` | Array&lt;Array&lt;string&gt;&gt;
`uniqueRefs` | [Array&lt;Tag&gt;](Tag.md)
`uniqueEnums` | Array&lt;string&gt;
`uniqueNullable` | Array&lt;string | null&gt;

## Example

```typescript
import type { UniqueItemsObject } from ''

// TODO: Update the object below with actual values
const example = {
  "uniqueStrings": null,
  "uniqueIntegers": null,
  "uniqueNumbers": null,
  "uniqueBooleans": null,
  "uniqueObjects": null,
  "uniqueArrays": null,
  "uniqueRefs": null,
  "uniqueEnums": null,
  "uniqueNullable": null,
} satisfies UniqueItemsObject

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as UniqueItemsObject
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


