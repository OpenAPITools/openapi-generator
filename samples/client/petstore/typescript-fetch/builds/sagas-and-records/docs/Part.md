
# Part

Contains all the info about a pet part

## Properties

Name | Type
------------ | -------------
`id` | number
`name` | string

## Example

```typescript
import type { Part } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example = {
  "id": 1,
  "name": head,
} satisfies Part

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Part
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


