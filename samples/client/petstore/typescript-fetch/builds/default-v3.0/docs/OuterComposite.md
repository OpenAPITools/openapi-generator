
# OuterComposite


## Properties

Name | Type
------------ | -------------
**myNumber** | **number**
**myString** | **string**
**myBoolean** | **boolean**

## Example

```typescript
import { OuterComposite } from ''

// TODO: Update the object below with actual values
const example: OuterComposite = {
    "myNumber": null,
    "myString": null,
    "myBoolean": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as OuterComposite
console.log(exampleParsed)
```


