
# FormatTest


## Properties

Name | Type
------------ | -------------
**integer** | **number**
**int32** | **number**
**int64** | **number**
**number** | **number**
**_float** | **number**
**_double** | **number**
**decimal** | [**Decimal**](Decimal.md)
**string** | **string**
**_byte** | **string**
**binary** | **Blob**
**date** | **Date**
**dateTime** | **Date**
**uuid** | **string**
**password** | **string**
**patternWithDigits** | **string**
**patternWithDigitsAndDelimiter** | **string**

## Example

```typescript
import { FormatTest } from ''

// TODO: Update the object below with actual values
const example: FormatTest = {
    "integer": null,
    "int32": null,
    "int64": null,
    "number": null,
    "_float": null,
    "_double": null,
    "decimal": null,
    "string": null,
    "_byte": null,
    "binary": null,
    "date": null,
    "dateTime": null,
    "uuid": 72f98069-206d-4f12-9f12-3d1e525a8e84,
    "password": null,
    "patternWithDigits": null,
    "patternWithDigitsAndDelimiter": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as FormatTest
console.log(exampleParsed)
```


