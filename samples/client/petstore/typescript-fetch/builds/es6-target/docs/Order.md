
# Order

An order for a pets from the pet store

## Properties

Name | Type
------------ | -------------
`id` | number
`petId` | number
`quantity` | number
`shipDate` | Date
`status` | string
`complete` | boolean

## Example

```typescript
import type { Order } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example = {
  "id": null,
  "petId": null,
  "quantity": null,
  "shipDate": null,
  "status": null,
  "complete": null,
} satisfies Order

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Order
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


