
# Animal


## Properties

Name | Type
------------ | -------------
**className** | **string**
**color** | **string**

## Example

```typescript
import { Animal } from ''

// TODO: Update the object below with actual values
const example: Animal = {
    "className": null,
    "color": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Animal
console.log(exampleParsed)
```


