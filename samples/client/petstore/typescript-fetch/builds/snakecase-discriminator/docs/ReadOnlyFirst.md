
# ReadOnlyFirst


## Properties

Name | Type
------------ | -------------
**bar** | **string**
**baz** | **string**

## Example

```typescript
import { ReadOnlyFirst } from ''

// TODO: Update the object below with actual values
const example: ReadOnlyFirst = {
    "bar": null,
    "baz": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ReadOnlyFirst
console.log(exampleParsed)
```


