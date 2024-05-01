
# NullableClass


## Properties

Name | Type
------------ | -------------
**integerProp** | **number**
**numberProp** | **number**
**booleanProp** | **boolean**
**stringProp** | **string**
**dateProp** | **Date**
**datetimeProp** | **Date**
**arrayNullableProp** | **Array&lt;object&gt;**
**arrayAndItemsNullableProp** | **Array&lt;object&gt;**
**arrayItemsNullable** | **Array&lt;object&gt;**
**objectNullableProp** | **{ [key: string]: object; }**
**objectAndItemsNullableProp** | **{ [key: string]: object | null; }**
**objectItemsNullable** | **{ [key: string]: object | null; }**

## Example

```typescript
import { NullableClass } from ''

// TODO: Update the object below with actual values
const example: NullableClass = {
    "integerProp": null,
    "numberProp": null,
    "booleanProp": null,
    "stringProp": null,
    "dateProp": null,
    "datetimeProp": null,
    "arrayNullableProp": null,
    "arrayAndItemsNullableProp": null,
    "arrayItemsNullable": null,
    "objectNullableProp": null,
    "objectAndItemsNullableProp": null,
    "objectItemsNullable": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as NullableClass
console.log(exampleParsed)
```


