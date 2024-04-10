
# ObjectWithDeprecatedFields


## Properties

Name | Type
------------ | -------------
**uuid** | **string**
**id** | **number**
**deprecatedRef** | [**DeprecatedObject**](DeprecatedObject.md)
**bars** | **Array&lt;string&gt;**

## Example

```typescript
import { ObjectWithDeprecatedFields } from ''

// TODO: Update the object below with actual values
const example: ObjectWithDeprecatedFields = {
    "uuid": null,
    "id": null,
    "deprecatedRef": null,
    "bars": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as ObjectWithDeprecatedFields
console.log(exampleParsed)
```


