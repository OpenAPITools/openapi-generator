
# GetPetPartTypeResponse


## Properties

Name | Type
------------ | -------------
**meta** | [**ResponseMeta**](ResponseMeta.md)
**data** | [**PetPartType**](PetPartType.md)

## Example

```typescript
import { GetPetPartTypeResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: GetPetPartTypeResponse = {
    "meta": null,
    "data": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as GetPetPartTypeResponse
console.log(exampleParsed)
```


