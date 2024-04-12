
# HasOnlyReadOnly


## Properties

Name | Type
------------ | -------------
**bar** | **string**
**foo** | **string**

## Example

```typescript
import { HasOnlyReadOnly } from ''

// TODO: Update the object below with actual values
const example: HasOnlyReadOnly = {
    "bar": null,
    "foo": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as HasOnlyReadOnly
console.log(exampleParsed)
```


