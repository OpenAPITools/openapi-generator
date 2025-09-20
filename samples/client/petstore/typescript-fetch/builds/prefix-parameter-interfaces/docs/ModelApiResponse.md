
# ModelApiResponse

Describes the result of uploading an image resource

## Properties

Name | Type
------------ | -------------
`code` | number
`type` | string
`message` | string

## Example

```typescript
import type { ModelApiResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example = {
  "code": null,
  "type": null,
  "message": null,
} satisfies ModelApiResponse

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ModelApiResponse
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


