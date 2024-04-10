
# GetBehaviorPermissionsResponse


## Properties

Name | Type
------------ | -------------
**meta** | [**ResponseMeta**](ResponseMeta.md)
**data** | **{ [key: string]: boolean; }**

## Example

```typescript
import { GetBehaviorPermissionsResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: GetBehaviorPermissionsResponse = {
    "meta": null,
    "data": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as GetBehaviorPermissionsResponse
console.log(exampleParsed)
```


