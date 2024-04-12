
# Category

A category for a pet

## Properties

Name | Type
------------ | -------------
**id** | **number**
**name** | **string**

## Example

```typescript
import { Category } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: Category = {
    "id": null,
    "name": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Category
console.log(exampleParsed)
```


