
# DiscriminatedPet


## Properties

Name | Type
------------ | -------------
`kind` | string
`meowCount` | number
`barkCount` | number
`tailLength` | number
`breed` | string

## Example

```typescript
import type { DiscriminatedPet } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example = {
  "kind": null,
  "meowCount": null,
  "barkCount": null,
  "tailLength": null,
  "breed": null,
} satisfies DiscriminatedPet

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as DiscriminatedPet
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


