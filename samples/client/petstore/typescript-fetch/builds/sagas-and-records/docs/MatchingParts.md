
# MatchingParts

Contains all the matching parts

## Properties

Name | Type
------------ | -------------
`connected` | [Array&lt;Part&gt;](Part.md)
`related` | [Array&lt;Part&gt;](Part.md)

## Example

```typescript
import type { MatchingParts } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example = {
  "connected": null,
  "related": null,
} satisfies MatchingParts

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as MatchingParts
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


