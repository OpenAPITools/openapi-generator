
# OuterObjectWithEnumProperty


## Properties

Name | Type
------------ | -------------
**value** | [**OuterEnumInteger**](OuterEnumInteger.md)

## Example

```typescript
import { OuterObjectWithEnumProperty } from ''

// TODO: Update the object below with actual values
const example: OuterObjectWithEnumProperty = {
    "value": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as OuterObjectWithEnumProperty
console.log(exampleParsed)
```


