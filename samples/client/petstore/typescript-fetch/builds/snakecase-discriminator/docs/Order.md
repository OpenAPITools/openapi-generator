
# Order


## Properties

Name | Type
------------ | -------------
**id** | **number**
**petId** | **number**
**quantity** | **number**
**shipDate** | **Date**
**status** | **string**
**complete** | **boolean**

## Example

```typescript
import { Order } from ''

// TODO: Update the object below with actual values
const example: Order = {
    "id": null,
    "petId": null,
    "quantity": null,
    "shipDate": null,
    "status": null,
    "complete": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Order
console.log(exampleParsed)
```


