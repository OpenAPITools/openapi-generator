
# MixedPropertiesAndAdditionalPropertiesClass


## Properties

Name | Type
------------ | -------------
**uuid** | **string**
**dateTime** | **Date**
**map** | [**{ [key: string]: Animal; }**](Animal.md)

## Example

```typescript
import { MixedPropertiesAndAdditionalPropertiesClass } from ''

// TODO: Update the object below with actual values
const example: MixedPropertiesAndAdditionalPropertiesClass = {
    "uuid": null,
    "dateTime": null,
    "map": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as MixedPropertiesAndAdditionalPropertiesClass
console.log(exampleParsed)
```


