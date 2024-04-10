
# ItemId

Simplified identifier of an item

## Properties

Name | Type
------------ | -------------
**id** | **string**
**type** | **string**

## Example

```typescript
import { ItemId } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: ItemId = {
    "id": 45,
    "type": 5667,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ItemId
console.log(exampleParsed)
```


