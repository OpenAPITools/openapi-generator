
# HealthCheckResult

Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.

## Properties

Name | Type
------------ | -------------
**nullableMessage** | **string**

## Example

```typescript
import { HealthCheckResult } from ''

// TODO: Update the object below with actual values
const example: HealthCheckResult = {
    "nullableMessage": null,
}

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as HealthCheckResult
console.log(exampleParsed)
```


