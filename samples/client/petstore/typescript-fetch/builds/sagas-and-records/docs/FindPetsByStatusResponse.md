
# FindPetsByStatusResponse


## Properties

Name | Type
------------ | -------------
`meta` | [ResponseMeta](ResponseMeta.md)
`data` | [Array&lt;Pet&gt;](Pet.md)

## Example

```typescript
import type { FindPetsByStatusResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example = {
  "meta": null,
  "data": null,
} satisfies FindPetsByStatusResponse

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as FindPetsByStatusResponse
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


