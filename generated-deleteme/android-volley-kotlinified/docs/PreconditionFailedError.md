

# PreconditionFailedError

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**type** | [**TypeEnum**](#TypeEnum) | The type of precondition (e.g OTP_REQUIRED). | 
**otpUuid** | **String** | The unique identifier for the OTP that authenticates this request (where required). | 
**otpRecipients** | [**List&lt;PreconditionFailedErrorOtpRecipients&gt;**](PreconditionFailedErrorOtpRecipients.md) | The recipients of the OTP. |  [optional]
**attempts** | [**PreconditionFailedErrorAttempts**](PreconditionFailedErrorAttempts.md) |  | 
**regenerateAllowed** | **Boolean** | Indicates whether or not an OTP can be regenerated and sent to the user. | 


## Enum: TypeEnum

Name | Value
---- | -----




