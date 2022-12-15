# .Pet

## Properties

|Name | Type | Description | Notes|
|------------ | ------------- | ------------- | -------------|
|**id** | **number** |  | [default to &quot;-1&quot;]|
|**friendId** | **number** |  | [optional] [default to &quot;-1&quot;]|
|**otherFriendIds** | **Array&lt;number&gt;** |  | [default to List&lt;string&gt;()]|
|**friendAge** | **number** |  | [default to 0]|
|**age** | **number** |  | [default to 2]|
|**isHappy** | **boolean** |  | [default to true]|
|**isTall** | **boolean** |  | [default to false]|
|**category** | [**Category**](Category.md) |  | [default to CategoryRecord()]|
|**optionalCategory** | [**Category**](Category.md) |  | [optional] [default to CategoryRecord()]|
|**name** | **string** |  | [default to &quot;&quot;]|
|**_entries** | [**Array&lt;Category&gt;**](Category.md) |  | [optional] [default to List&lt;CategoryRecord&gt;()]|
|**surname** | **string** |  | [optional] [default to &quot;&quot;]|
|**photoUrls** | **Array&lt;string&gt;** |  | [default to List&lt;string&gt;()]|
|**warningStatus** | [**WarningCode**](WarningCode.md) |  | [default to WarningCode.ReduceVolumeRangeToAvoidLargeSteps]|
|**depStatus** | [**DeploymentRequestStatus**](DeploymentRequestStatus.md) |  | [optional] [default to DeploymentRequestStatus.New]|
|**alternateStatus** | [**DeploymentRequestStatus**](DeploymentRequestStatus.md) |  | [default to DeploymentRequestStatus.New]|
|**otherDepStatuses** | [**Array&lt;DeploymentRequestStatus&gt;**](DeploymentRequestStatus.md) |  | [default to List&lt;DeploymentRequestStatus&gt;()]|
|**tags** | [**Array&lt;Tag&gt;**](Tag.md) |  | [default to List&lt;TagRecord&gt;()]|
|**optionalTags** | [**Array&lt;Tag&gt;**](Tag.md) |  | [optional] [default to List&lt;TagRecord&gt;()]|
|**status** | **string** | pet status in the store | [default to PetStatusEnum.Pending]|
|**regions** | **Array&lt;Array&lt;number&gt;&gt;** | An array of all 15-minute time slots in 24 hours. | [optional] [default to List&lt;List&lt;string | null&gt;&gt;()]|


## Enum: PetStatusEnum


* `Available` (value: `'available'`)

* `Pending` (value: `'pending'`)

* `Sold` (value: `'sold'`)




