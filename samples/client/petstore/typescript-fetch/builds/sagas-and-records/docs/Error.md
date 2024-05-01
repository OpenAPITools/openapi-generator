
# ModelError

This represent an error normally linked to a specific item from a previous request

## Properties

Name | Type
------------ | -------------
**type** | **string**
**itemInfo** | [**ItemId**](ItemId.md)
**details** | **string**
**exception** | **string**

## Example

```typescript
import { ModelError } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: ModelError = {
    "type": GenericException,
    "itemInfo": null,
    "details": Could not update that field,
    "exception": DBException + stack trace,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ModelError
console.log(exampleParsed)
```


