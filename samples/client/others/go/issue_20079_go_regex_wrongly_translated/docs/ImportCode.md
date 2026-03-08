# ImportCode

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**Code** | Pointer to **string** |  | [optional] 
**CreditCard** | Pointer to **string** | Visa credit card  matches: 4123 6453 2222 1746  non-matches: 3124 5675 4400 4567, 4123-6453-2222-1746 | [optional] 
**Date** | Pointer to **string** | Some dates  matches: 31/04/1999, 15/12/4567  non-matches: 31/4/1999, 31/4/99, 1999/04/19, 42/67/25456 | [optional] 
**WindowsAbsolutePath** | Pointer to **string** | Windows absolute path  matches: \\\\server\\share\\file  non-matches: \\directory\\directory2, /directory2 | [optional] 
**Email1** | Pointer to **string** | Email Address 1  matches: abc.123@def456.com, _123@abc.ca  non-matches: abc@dummy, ab*cd@efg.hijkl | [optional] 
**Email2** | Pointer to **string** | Email Address 2  matches: *@qrstuv@wxyz.12345.com, __1234^%@@abc.def.ghijkl  non-matches: abc.123.*&amp;ca, ^%abcdefg123 | [optional] 
**HtmlHexadecimalColorCode1** | Pointer to **string** | HTML Hexadecimal Color Code 1  matches: AB1234, CCCCCC, 12AF3B  non-matches: 123G45, 12-44-CC | [optional] 
**HtmlHexadecimalColorCode2** | Pointer to **string** | HTML Hexadecimal Color Code 2  matches: AB 11 00, CC 12 D3  non-matches: SS AB CD, AA BB CC DD, 1223AB | [optional] 
**IpAddress** | Pointer to **string** | IP Address  matches: 10.25.101.216  non-matches: 0.0.0, 256.89.457.02 | [optional] 
**JavaComments** | Pointer to **string** | Java Comments  matches: Matches Java comments that are between /_* and *_/, or one line comments prefaced by //  non-matches: a&#x3D;1 | [optional] 
**Money** | Pointer to **string** |   matches: $1.00, -$97.65 non-matches: $1, 1.00$, $-75.17 | [optional] 
**PositiveNegativeDecimalValue** | Pointer to **string** | Positive, negative numbers, and decimal values  matches: +41, -412, 2, 7968412, 41, +41.1, -3.141592653 non-matches: ++41, 41.1.19, -+97.14 | [optional] 
**Password1** | Pointer to **string** | Passwords 1  matches: abcd, 1234, A1b2C3d4, 1a2B3  non-matches: abc, *ab12, abcdefghijkl | [optional] 
**Password2** | Pointer to **string** | Passwords 2  matches: AB_cd, A1_b2c3, a123_  non-matches: *&amp;^g, abc, 1bcd | [optional] 
**PhoneNumber** | Pointer to **string** | Phone Numbers  matches: 519-883-6898, 519 888 6898  non-matches: 888 6898, 5198886898, 519 883-6898 | [optional] 
**Sentence1** | Pointer to **string** | Sentences 1  matches: Hello, how are you?  non-matches: i am fine | [optional] 
**Sentence2** | Pointer to **string** | Sentences 2  matches: Hello, how are you?n non-matches: i am fine | [optional] 
**SocialSecurityNumber** | Pointer to **string** | Social Security Number  matches: 123-45-6789  non-matches: 123 45 6789, 123456789, 1234-56-7891 | [optional] 
**Url** | Pointer to **string** | URL  matches: http://www.sample.com, www.sample.com  non-matches: http://sample.com, http://www.sample.comm | [optional] 

## Methods

### NewImportCode

`func NewImportCode() *ImportCode`

NewImportCode instantiates a new ImportCode object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewImportCodeWithDefaults

`func NewImportCodeWithDefaults() *ImportCode`

NewImportCodeWithDefaults instantiates a new ImportCode object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetCode

`func (o *ImportCode) GetCode() string`

GetCode returns the Code field if non-nil, zero value otherwise.

### GetCodeOk

`func (o *ImportCode) GetCodeOk() (*string, bool)`

GetCodeOk returns a tuple with the Code field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCode

`func (o *ImportCode) SetCode(v string)`

SetCode sets Code field to given value.

### HasCode

`func (o *ImportCode) HasCode() bool`

HasCode returns a boolean if a field has been set.

### GetCreditCard

`func (o *ImportCode) GetCreditCard() string`

GetCreditCard returns the CreditCard field if non-nil, zero value otherwise.

### GetCreditCardOk

`func (o *ImportCode) GetCreditCardOk() (*string, bool)`

GetCreditCardOk returns a tuple with the CreditCard field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetCreditCard

`func (o *ImportCode) SetCreditCard(v string)`

SetCreditCard sets CreditCard field to given value.

### HasCreditCard

`func (o *ImportCode) HasCreditCard() bool`

HasCreditCard returns a boolean if a field has been set.

### GetDate

`func (o *ImportCode) GetDate() string`

GetDate returns the Date field if non-nil, zero value otherwise.

### GetDateOk

`func (o *ImportCode) GetDateOk() (*string, bool)`

GetDateOk returns a tuple with the Date field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetDate

`func (o *ImportCode) SetDate(v string)`

SetDate sets Date field to given value.

### HasDate

`func (o *ImportCode) HasDate() bool`

HasDate returns a boolean if a field has been set.

### GetWindowsAbsolutePath

`func (o *ImportCode) GetWindowsAbsolutePath() string`

GetWindowsAbsolutePath returns the WindowsAbsolutePath field if non-nil, zero value otherwise.

### GetWindowsAbsolutePathOk

`func (o *ImportCode) GetWindowsAbsolutePathOk() (*string, bool)`

GetWindowsAbsolutePathOk returns a tuple with the WindowsAbsolutePath field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetWindowsAbsolutePath

`func (o *ImportCode) SetWindowsAbsolutePath(v string)`

SetWindowsAbsolutePath sets WindowsAbsolutePath field to given value.

### HasWindowsAbsolutePath

`func (o *ImportCode) HasWindowsAbsolutePath() bool`

HasWindowsAbsolutePath returns a boolean if a field has been set.

### GetEmail1

`func (o *ImportCode) GetEmail1() string`

GetEmail1 returns the Email1 field if non-nil, zero value otherwise.

### GetEmail1Ok

`func (o *ImportCode) GetEmail1Ok() (*string, bool)`

GetEmail1Ok returns a tuple with the Email1 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEmail1

`func (o *ImportCode) SetEmail1(v string)`

SetEmail1 sets Email1 field to given value.

### HasEmail1

`func (o *ImportCode) HasEmail1() bool`

HasEmail1 returns a boolean if a field has been set.

### GetEmail2

`func (o *ImportCode) GetEmail2() string`

GetEmail2 returns the Email2 field if non-nil, zero value otherwise.

### GetEmail2Ok

`func (o *ImportCode) GetEmail2Ok() (*string, bool)`

GetEmail2Ok returns a tuple with the Email2 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetEmail2

`func (o *ImportCode) SetEmail2(v string)`

SetEmail2 sets Email2 field to given value.

### HasEmail2

`func (o *ImportCode) HasEmail2() bool`

HasEmail2 returns a boolean if a field has been set.

### GetHtmlHexadecimalColorCode1

`func (o *ImportCode) GetHtmlHexadecimalColorCode1() string`

GetHtmlHexadecimalColorCode1 returns the HtmlHexadecimalColorCode1 field if non-nil, zero value otherwise.

### GetHtmlHexadecimalColorCode1Ok

`func (o *ImportCode) GetHtmlHexadecimalColorCode1Ok() (*string, bool)`

GetHtmlHexadecimalColorCode1Ok returns a tuple with the HtmlHexadecimalColorCode1 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetHtmlHexadecimalColorCode1

`func (o *ImportCode) SetHtmlHexadecimalColorCode1(v string)`

SetHtmlHexadecimalColorCode1 sets HtmlHexadecimalColorCode1 field to given value.

### HasHtmlHexadecimalColorCode1

`func (o *ImportCode) HasHtmlHexadecimalColorCode1() bool`

HasHtmlHexadecimalColorCode1 returns a boolean if a field has been set.

### GetHtmlHexadecimalColorCode2

`func (o *ImportCode) GetHtmlHexadecimalColorCode2() string`

GetHtmlHexadecimalColorCode2 returns the HtmlHexadecimalColorCode2 field if non-nil, zero value otherwise.

### GetHtmlHexadecimalColorCode2Ok

`func (o *ImportCode) GetHtmlHexadecimalColorCode2Ok() (*string, bool)`

GetHtmlHexadecimalColorCode2Ok returns a tuple with the HtmlHexadecimalColorCode2 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetHtmlHexadecimalColorCode2

`func (o *ImportCode) SetHtmlHexadecimalColorCode2(v string)`

SetHtmlHexadecimalColorCode2 sets HtmlHexadecimalColorCode2 field to given value.

### HasHtmlHexadecimalColorCode2

`func (o *ImportCode) HasHtmlHexadecimalColorCode2() bool`

HasHtmlHexadecimalColorCode2 returns a boolean if a field has been set.

### GetIpAddress

`func (o *ImportCode) GetIpAddress() string`

GetIpAddress returns the IpAddress field if non-nil, zero value otherwise.

### GetIpAddressOk

`func (o *ImportCode) GetIpAddressOk() (*string, bool)`

GetIpAddressOk returns a tuple with the IpAddress field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetIpAddress

`func (o *ImportCode) SetIpAddress(v string)`

SetIpAddress sets IpAddress field to given value.

### HasIpAddress

`func (o *ImportCode) HasIpAddress() bool`

HasIpAddress returns a boolean if a field has been set.

### GetJavaComments

`func (o *ImportCode) GetJavaComments() string`

GetJavaComments returns the JavaComments field if non-nil, zero value otherwise.

### GetJavaCommentsOk

`func (o *ImportCode) GetJavaCommentsOk() (*string, bool)`

GetJavaCommentsOk returns a tuple with the JavaComments field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetJavaComments

`func (o *ImportCode) SetJavaComments(v string)`

SetJavaComments sets JavaComments field to given value.

### HasJavaComments

`func (o *ImportCode) HasJavaComments() bool`

HasJavaComments returns a boolean if a field has been set.

### GetMoney

`func (o *ImportCode) GetMoney() string`

GetMoney returns the Money field if non-nil, zero value otherwise.

### GetMoneyOk

`func (o *ImportCode) GetMoneyOk() (*string, bool)`

GetMoneyOk returns a tuple with the Money field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetMoney

`func (o *ImportCode) SetMoney(v string)`

SetMoney sets Money field to given value.

### HasMoney

`func (o *ImportCode) HasMoney() bool`

HasMoney returns a boolean if a field has been set.

### GetPositiveNegativeDecimalValue

`func (o *ImportCode) GetPositiveNegativeDecimalValue() string`

GetPositiveNegativeDecimalValue returns the PositiveNegativeDecimalValue field if non-nil, zero value otherwise.

### GetPositiveNegativeDecimalValueOk

`func (o *ImportCode) GetPositiveNegativeDecimalValueOk() (*string, bool)`

GetPositiveNegativeDecimalValueOk returns a tuple with the PositiveNegativeDecimalValue field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPositiveNegativeDecimalValue

`func (o *ImportCode) SetPositiveNegativeDecimalValue(v string)`

SetPositiveNegativeDecimalValue sets PositiveNegativeDecimalValue field to given value.

### HasPositiveNegativeDecimalValue

`func (o *ImportCode) HasPositiveNegativeDecimalValue() bool`

HasPositiveNegativeDecimalValue returns a boolean if a field has been set.

### GetPassword1

`func (o *ImportCode) GetPassword1() string`

GetPassword1 returns the Password1 field if non-nil, zero value otherwise.

### GetPassword1Ok

`func (o *ImportCode) GetPassword1Ok() (*string, bool)`

GetPassword1Ok returns a tuple with the Password1 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPassword1

`func (o *ImportCode) SetPassword1(v string)`

SetPassword1 sets Password1 field to given value.

### HasPassword1

`func (o *ImportCode) HasPassword1() bool`

HasPassword1 returns a boolean if a field has been set.

### GetPassword2

`func (o *ImportCode) GetPassword2() string`

GetPassword2 returns the Password2 field if non-nil, zero value otherwise.

### GetPassword2Ok

`func (o *ImportCode) GetPassword2Ok() (*string, bool)`

GetPassword2Ok returns a tuple with the Password2 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPassword2

`func (o *ImportCode) SetPassword2(v string)`

SetPassword2 sets Password2 field to given value.

### HasPassword2

`func (o *ImportCode) HasPassword2() bool`

HasPassword2 returns a boolean if a field has been set.

### GetPhoneNumber

`func (o *ImportCode) GetPhoneNumber() string`

GetPhoneNumber returns the PhoneNumber field if non-nil, zero value otherwise.

### GetPhoneNumberOk

`func (o *ImportCode) GetPhoneNumberOk() (*string, bool)`

GetPhoneNumberOk returns a tuple with the PhoneNumber field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetPhoneNumber

`func (o *ImportCode) SetPhoneNumber(v string)`

SetPhoneNumber sets PhoneNumber field to given value.

### HasPhoneNumber

`func (o *ImportCode) HasPhoneNumber() bool`

HasPhoneNumber returns a boolean if a field has been set.

### GetSentence1

`func (o *ImportCode) GetSentence1() string`

GetSentence1 returns the Sentence1 field if non-nil, zero value otherwise.

### GetSentence1Ok

`func (o *ImportCode) GetSentence1Ok() (*string, bool)`

GetSentence1Ok returns a tuple with the Sentence1 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSentence1

`func (o *ImportCode) SetSentence1(v string)`

SetSentence1 sets Sentence1 field to given value.

### HasSentence1

`func (o *ImportCode) HasSentence1() bool`

HasSentence1 returns a boolean if a field has been set.

### GetSentence2

`func (o *ImportCode) GetSentence2() string`

GetSentence2 returns the Sentence2 field if non-nil, zero value otherwise.

### GetSentence2Ok

`func (o *ImportCode) GetSentence2Ok() (*string, bool)`

GetSentence2Ok returns a tuple with the Sentence2 field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSentence2

`func (o *ImportCode) SetSentence2(v string)`

SetSentence2 sets Sentence2 field to given value.

### HasSentence2

`func (o *ImportCode) HasSentence2() bool`

HasSentence2 returns a boolean if a field has been set.

### GetSocialSecurityNumber

`func (o *ImportCode) GetSocialSecurityNumber() string`

GetSocialSecurityNumber returns the SocialSecurityNumber field if non-nil, zero value otherwise.

### GetSocialSecurityNumberOk

`func (o *ImportCode) GetSocialSecurityNumberOk() (*string, bool)`

GetSocialSecurityNumberOk returns a tuple with the SocialSecurityNumber field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetSocialSecurityNumber

`func (o *ImportCode) SetSocialSecurityNumber(v string)`

SetSocialSecurityNumber sets SocialSecurityNumber field to given value.

### HasSocialSecurityNumber

`func (o *ImportCode) HasSocialSecurityNumber() bool`

HasSocialSecurityNumber returns a boolean if a field has been set.

### GetUrl

`func (o *ImportCode) GetUrl() string`

GetUrl returns the Url field if non-nil, zero value otherwise.

### GetUrlOk

`func (o *ImportCode) GetUrlOk() (*string, bool)`

GetUrlOk returns a tuple with the Url field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetUrl

`func (o *ImportCode) SetUrl(v string)`

SetUrl sets Url field to given value.

### HasUrl

`func (o *ImportCode) HasUrl() bool`

HasUrl returns a boolean if a field has been set.


[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


