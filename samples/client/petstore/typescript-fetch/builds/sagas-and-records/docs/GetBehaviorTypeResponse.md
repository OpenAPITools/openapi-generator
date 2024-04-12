
# GetBehaviorTypeResponse


## Properties

Name | Type
------------ | -------------
**meta** | [**ResponseMeta**](ResponseMeta.md)
**data** | [**BehaviorType**](BehaviorType.md)

## Example

```typescript
import { GetBehaviorTypeResponse } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: GetBehaviorTypeResponse = {
    "meta": null,
    "data": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as GetBehaviorTypeResponse
console.log(exampleParsed)
```


