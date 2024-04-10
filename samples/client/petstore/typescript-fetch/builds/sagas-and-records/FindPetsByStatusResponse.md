
# FindPetsByStatusResponse


## Properties

Name | Type
------------ | -------------
**meta** | [**ResponseMeta**](ResponseMeta.md)
**data** | [**Array&lt;Pet&gt;**](Pet.md)

## Example

```typescript
import { FindPetsByStatusResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: FindPetsByStatusResponse = {
    "meta": null,
    "data": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as FindPetsByStatusResponse
console.log(exampleParsed)
```


