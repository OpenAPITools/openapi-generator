
# MapTest


## Properties

Name | Type
------------ | -------------
**mapMapOfString** | **{ [key: string]: { [key: string]: string; }; }**
**mapOfEnumString** | **{ [key: string]: string; }**
**directMap** | **{ [key: string]: boolean; }**
**indirectMap** | **{ [key: string]: boolean; }**

## Example

```typescript
import { MapTest } from ''

// TODO: Update the object below with actual values
const example: MapTest = {
    "mapMapOfString": null,
    "mapOfEnumString": null,
    "directMap": null,
    "indirectMap": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as MapTest
console.log(exampleParsed)
```


