
# Part

Contains all the info about a pet part

## Properties

Name | Type
------------ | -------------
**id** | **number**
**name** | **string**

## Example

```typescript
import { Part } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: Part = {
    "id": 1,
    "name": head,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Part
console.log(exampleParsed)
```


