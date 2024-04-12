
# Pet


## Properties

Name | Type
------------ | -------------
**id** | **number**
**category** | [**Category**](Category.md)
**name** | **string**
**photoUrls** | **Set&lt;string&gt;**
**tags** | [**Array&lt;Tag&gt;**](Tag.md)
**status** | **string**

## Example

```typescript
import { Pet } from ''

// TODO: Update the object below with actual values
const example: Pet = {
    "id": null,
    "category": null,
    "name": doggie,
    "photoUrls": null,
    "tags": null,
    "status": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Pet
console.log(exampleParsed)
```


