
# EnumArrays


## Properties

Name | Type
------------ | -------------
**justSymbol** | **string**
**arrayEnum** | **Array&lt;string&gt;**

## Example

```typescript
import { EnumArrays } from ''

// TODO: Update the object below with actual values
const example: EnumArrays = {
    "justSymbol": null,
    "arrayEnum": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as EnumArrays
console.log(exampleParsed)
```


