
# PetRegionsResponse


## Properties

Name | Type
------------ | -------------
**meta** | [**ResponseMeta**](ResponseMeta.md)
**data** | **Array&lt;Array&lt;number&gt;&gt;**

## Example

```typescript
import { PetRegionsResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: PetRegionsResponse = {
    "meta": null,
    "data": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as PetRegionsResponse
console.log(exampleParsed)
```


