
# Pet

A pet for sale in the pet store

## Properties

Name | Type
------------ | -------------
`id` | number
`friendId` | number
`otherFriendIds` | Array&lt;number&gt;
`friendAge` | number
`age` | number
`isHappy` | boolean
`isTall` | boolean
`category` | [Category](Category.md)
`optionalCategory` | [Category](Category.md)
`name` | string
`_entries` | [Array&lt;Category&gt;](Category.md)
`surname` | string
`photoUrls` | Array&lt;string&gt;
`warningStatus` | [WarningCode](WarningCode.md)
`depStatus` | [DeploymentRequestStatus](DeploymentRequestStatus.md)
`alternateStatus` | [DeploymentRequestStatus](DeploymentRequestStatus.md)
`otherDepStatuses` | [Array&lt;DeploymentRequestStatus&gt;](DeploymentRequestStatus.md)
`tags` | [Array&lt;Tag&gt;](Tag.md)
`optionalTags` | [Array&lt;Tag&gt;](Tag.md)
`status` | string
`regions` | Array&lt;Array&lt;number | null&gt;&gt;

## Example

```typescript
import type { Pet } from '@openapitools/typescript-fetch-petstore'

// TODO: Update the object below with actual values
const example = {
  "id": null,
  "friendId": null,
  "otherFriendIds": null,
  "friendAge": null,
  "age": null,
  "isHappy": null,
  "isTall": null,
  "category": null,
  "optionalCategory": null,
  "name": doggie,
  "_entries": null,
  "surname": woofy,
  "photoUrls": null,
  "warningStatus": null,
  "depStatus": null,
  "alternateStatus": null,
  "otherDepStatuses": null,
  "tags": null,
  "optionalTags": null,
  "status": null,
  "regions": null,
} satisfies Pet

console.log(example)

// Convert the instance to a JSON string
const exampleJSON: string = JSON.stringify(example)
console.log(exampleJSON)

// Parse the JSON string back to an object
const exampleParsed = JSON.parse(exampleJSON) as Pet
console.log(exampleParsed)
```

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


