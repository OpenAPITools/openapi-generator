
# EnumPatternObject


## Properties

Name | Type
------------ | -------------
**stringEnum** | [**StringEnum**](StringEnum.md)
**nullableStringEnum** | [**StringEnum**](StringEnum.md)
**numberEnum** | [**NumberEnum**](NumberEnum.md)
**nullableNumberEnum** | [**NumberEnum**](NumberEnum.md)

## Example

```typescript
import { EnumPatternObject } from ''

// TODO: Update the object below with actual values
const example: EnumPatternObject = {
    "stringEnum": null,
    "nullableStringEnum": null,
    "numberEnum": null,
    "nullableNumberEnum": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as EnumPatternObject
console.log(exampleParsed)
```


