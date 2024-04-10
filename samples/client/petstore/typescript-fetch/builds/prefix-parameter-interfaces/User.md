
# User

A User who is purchasing from the pet store

## Properties

Name | Type
------------ | -------------
**id** | **number**
**username** | **string**
**firstName** | **string**
**lastName** | **string**
**email** | **string**
**password** | **string**
**phone** | **string**
**userStatus** | **number**

## Example

```typescript
import { User } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example: User = {
    "id": null,
    "username": null,
    "firstName": null,
    "lastName": null,
    "email": null,
    "password": null,
    "phone": null,
    "userStatus": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as User
console.log(exampleParsed)
```


