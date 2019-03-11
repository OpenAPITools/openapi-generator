
# TypeHolderExample

## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**stringItem** | [**StringItemEnum**](#StringItemEnum) |  | 
**numberItem** | [**NumberItemEnum**](#NumberItemEnum) |  | 
**integerItem** | [**IntegerItemEnum**](#IntegerItemEnum) |  | 
**boolItem** | **Boolean** |  | 
**dateItem** | [**DateItemEnum**](#DateItemEnum) |  | 
**datetimeItem** | [**DatetimeItemEnum**](#DatetimeItemEnum) |  | 
**arrayItem** | **List&lt;Integer&gt;** |  | 


<a name="StringItemEnum"></a>
## Enum: StringItemEnum
Name | Value
---- | -----
WHAT | String.valueOf(&quot;what&quot;)


<a name="NumberItemEnum"></a>
## Enum: NumberItemEnum
Name | Value
---- | -----
NUMBER_1_DOT_2339999675750732 | Float.valueOf(&quot;1.2339999675750732&quot;)


<a name="IntegerItemEnum"></a>
## Enum: IntegerItemEnum
Name | Value
---- | -----
NUMBER_MINUS_2 | Integer.valueOf(-2)


<a name="DateItemEnum"></a>
## Enum: DateItemEnum
Name | Value
---- | -----
THU_JUL_20_17_00_00_PDT_2017 | LocalDate.parse(&quot;2017-07-21&quot;)


<a name="DatetimeItemEnum"></a>
## Enum: DatetimeItemEnum
Name | Value
---- | -----
FRI_JUL_21_10_32_28_PDT_2017 | OffsetDateTime.parse(&quot;2017-07-21T17:32:28.000Z&quot;)



