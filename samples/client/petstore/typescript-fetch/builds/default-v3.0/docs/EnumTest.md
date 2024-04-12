
# EnumTest


## Properties

Name | Type
------------ | -------------
**enumString** | **string**
**enumStringRequired** | **string**
**enumInteger** | **number**
**enumNumber** | **number**
**outerEnum** | [**OuterEnum**](OuterEnum.md)
**outerEnumInteger** | [**OuterEnumInteger**](OuterEnumInteger.md)
**outerEnumDefaultValue** | [**OuterEnumDefaultValue**](OuterEnumDefaultValue.md)
**outerEnumIntegerDefaultValue** | [**OuterEnumIntegerDefaultValue**](OuterEnumIntegerDefaultValue.md)

## Example

```typescript
import { EnumTest } from ''

// TODO: Update the object below with actual values
const example: EnumTest = {
    "enumString": null,
    "enumStringRequired": null,
    "enumInteger": null,
    "enumNumber": null,
    "outerEnum": null,
    "outerEnumInteger": null,
    "outerEnumDefaultValue": null,
    "outerEnumIntegerDefaultValue": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as EnumTest
console.log(exampleParsed)
```


