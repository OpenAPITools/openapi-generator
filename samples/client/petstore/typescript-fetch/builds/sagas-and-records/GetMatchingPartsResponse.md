
# GetMatchingPartsResponse


## Properties

Name | Type
------------ | -------------
**meta** | [**ResponseMeta**](ResponseMeta.md)
**data** | [**MatchingParts**](MatchingParts.md)

## Example

```typescript
import { GetMatchingPartsResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: GetMatchingPartsResponse = {
    "meta": null,
    "data": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as GetMatchingPartsResponse
console.log(exampleParsed)
```


