
# ResponseMeta

Mandatory part of each response given by our API

## Properties

Name | Type
------------ | -------------
`code` | string
`detail` | string
`exception` | string
`type` | string
`errorCode` | [ErrorCode](ErrorCode.md)
`errors` | Array&lt;Error&gt;

## Example

```typescript
import type { ResponseMeta } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example = {
  "code": Ok,
  "detail": this is some detail about the error or the success,
  "exception": IOException + stack trace,
  "type": Invalid Token,
  "errorCode": null,
  "errors": null,
} satisfies ResponseMeta

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ResponseMeta
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


