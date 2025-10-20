
# ArrayTest


## Properties

Name | Type
------------ | -------------
`arrayOfString` | Array&lt;string&gt;
`arrayArrayOfInteger` | Array&lt;Array&lt;number&gt;&gt;
`arrayArrayOfModel` | Array&lt;Array&lt;ReadOnlyFirst&gt;&gt;

## Example

```typescript
import type { ArrayTest } from ''

// TODO: Update the object below with actual values
const example = {
  "arrayOfString": null,
  "arrayArrayOfInteger": null,
  "arrayArrayOfModel": null,
} satisfies ArrayTest

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ArrayTest
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


