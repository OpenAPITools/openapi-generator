
# FindPetsByUserResponse


## Properties

Name | Type
------------ | -------------
**meta** | [**ResponseMeta**](ResponseMeta.md)
**data** | [**Array&lt;User&gt;**](User.md)

## Example

```typescript
import { FindPetsByUserResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: FindPetsByUserResponse = {
    "meta": null,
    "data": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as FindPetsByUserResponse
console.log(exampleParsed)
```


