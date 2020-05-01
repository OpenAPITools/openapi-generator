# EquilateralTriangle

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ShapeType** | Pointer to **string** |  | 
**TriangleType** | Pointer to **string** |  | 

## Methods

### NewEquilateralTriangle

`func NewEquilateralTriangle(shapeType string, triangleType string, ) *EquilateralTriangle`

NewEquilateralTriangle instantiates a new EquilateralTriangle object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewEquilateralTriangleWithDefaults

`func NewEquilateralTriangleWithDefaults() *EquilateralTriangle`

NewEquilateralTriangleWithDefaults instantiates a new EquilateralTriangle object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetShapeType

`func (o *EquilateralTriangle) GetShapeType() string`

GetShapeType returns the ShapeType field if non-nil, zero value otherwise.

### GetShapeTypeOk

`func (o *EquilateralTriangle) GetShapeTypeOk() (*string, bool)`

GetShapeTypeOk returns a tuple with the ShapeType field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetShapeType

`func (o *EquilateralTriangle) SetShapeType(v string)`

SetShapeType sets ShapeType field to given value.


### GetTriangleType

`func (o *EquilateralTriangle) GetTriangleType() string`

GetTriangleType returns the TriangleType field if non-nil, zero value otherwise.

### GetTriangleTypeOk

`func (o *EquilateralTriangle) GetTriangleTypeOk() (*string, bool)`

GetTriangleTypeOk returns a tuple with the TriangleType field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTriangleType

`func (o *EquilateralTriangle) SetTriangleType(v string)`

SetTriangleType sets TriangleType field to given value.



### AsTriangle

`func (s *EquilateralTriangle) AsTriangle() Triangle`

Convenience method to wrap this instance of EquilateralTriangle in Triangle

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


