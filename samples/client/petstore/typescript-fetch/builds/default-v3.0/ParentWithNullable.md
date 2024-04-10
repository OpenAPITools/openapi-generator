
# ParentWithNullable


## Properties

Name | Type
------------ | -------------
**type** | **string**
**nullableProperty** | **string**

## Example

```typescript
import { ParentWithNullable } from ''

// TODO: Update the object below with actual values
const example: ParentWithNullable = {
    "type": null,
    "nullableProperty": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ParentWithNullable
console.log(exampleParsed)
```


