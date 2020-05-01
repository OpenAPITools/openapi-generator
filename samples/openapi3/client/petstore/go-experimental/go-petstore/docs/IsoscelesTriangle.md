# IsoscelesTriangle

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ShapeType** | Pointer to **string** |  | 
**TriangleType** | Pointer to **string** |  | 

## Methods

### NewIsoscelesTriangle

`func NewIsoscelesTriangle(shapeType string, triangleType string, ) *IsoscelesTriangle`

NewIsoscelesTriangle instantiates a new IsoscelesTriangle object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewIsoscelesTriangleWithDefaults

`func NewIsoscelesTriangleWithDefaults() *IsoscelesTriangle`

NewIsoscelesTriangleWithDefaults instantiates a new IsoscelesTriangle object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetShapeType

`func (o *IsoscelesTriangle) GetShapeType() string`

GetShapeType returns the ShapeType field if non-nil, zero value otherwise.

### GetShapeTypeOk

`func (o *IsoscelesTriangle) GetShapeTypeOk() (*string, bool)`

GetShapeTypeOk returns a tuple with the ShapeType field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetShapeType

`func (o *IsoscelesTriangle) SetShapeType(v string)`

SetShapeType sets ShapeType field to given value.


### GetTriangleType

`func (o *IsoscelesTriangle) GetTriangleType() string`

GetTriangleType returns the TriangleType field if non-nil, zero value otherwise.

### GetTriangleTypeOk

`func (o *IsoscelesTriangle) GetTriangleTypeOk() (*string, bool)`

GetTriangleTypeOk returns a tuple with the TriangleType field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetTriangleType

`func (o *IsoscelesTriangle) SetTriangleType(v string)`

SetTriangleType sets TriangleType field to given value.



### AsTriangle

`func (s *IsoscelesTriangle) AsTriangle() Triangle`

Convenience method to wrap this instance of IsoscelesTriangle in Triangle

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


