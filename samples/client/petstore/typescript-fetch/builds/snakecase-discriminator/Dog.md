
# Dog


## Properties

Name | Type
------------ | -------------
**breed** | **string**

## Example

```typescript
import { Dog } from ''

// TODO: Update the object below with actual values
const example: Dog = {
    "breed": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Dog
console.log(exampleParsed)
```


