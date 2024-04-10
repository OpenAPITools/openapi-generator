
# AdditionalPropertiesClass


## Properties

Name | Type
------------ | -------------
**mapProperty** | **{ [key: string]: string; }**
**mapOfMapProperty** | **{ [key: string]: { [key: string]: string; }; }**

## Example

```typescript
import { AdditionalPropertiesClass } from ''

// TODO: Update the object below with actual values
const example: AdditionalPropertiesClass = {
    "mapProperty": null,
    "mapOfMapProperty": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as AdditionalPropertiesClass
console.log(exampleParsed)
```


