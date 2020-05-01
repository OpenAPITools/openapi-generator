# ComplexQuadrilateral

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**ShapeType** | Pointer to **string** |  | 
**QuadrilateralType** | Pointer to **string** |  | 

## Methods

### NewComplexQuadrilateral

`func NewComplexQuadrilateral(shapeType string, quadrilateralType string, ) *ComplexQuadrilateral`

NewComplexQuadrilateral instantiates a new ComplexQuadrilateral object
This constructor will assign default values to properties that have it defined,
and makes sure properties required by API are set, but the set of arguments
will change when the set of required properties is changed

### NewComplexQuadrilateralWithDefaults

`func NewComplexQuadrilateralWithDefaults() *ComplexQuadrilateral`

NewComplexQuadrilateralWithDefaults instantiates a new ComplexQuadrilateral object
This constructor will only assign default values to properties that have it defined,
but it doesn't guarantee that properties required by API are set

### GetShapeType

`func (o *ComplexQuadrilateral) GetShapeType() string`

GetShapeType returns the ShapeType field if non-nil, zero value otherwise.

### GetShapeTypeOk

`func (o *ComplexQuadrilateral) GetShapeTypeOk() (*string, bool)`

GetShapeTypeOk returns a tuple with the ShapeType field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetShapeType

`func (o *ComplexQuadrilateral) SetShapeType(v string)`

SetShapeType sets ShapeType field to given value.


### GetQuadrilateralType

`func (o *ComplexQuadrilateral) GetQuadrilateralType() string`

GetQuadrilateralType returns the QuadrilateralType field if non-nil, zero value otherwise.

### GetQuadrilateralTypeOk

`func (o *ComplexQuadrilateral) GetQuadrilateralTypeOk() (*string, bool)`

GetQuadrilateralTypeOk returns a tuple with the QuadrilateralType field if it's non-nil, zero value otherwise
and a boolean to check if the value has been set.

### SetQuadrilateralType

`func (o *ComplexQuadrilateral) SetQuadrilateralType(v string)`

SetQuadrilateralType sets QuadrilateralType field to given value.



### AsQuadrilateral

`func (s *ComplexQuadrilateral) AsQuadrilateral() Quadrilateral`

Convenience method to wrap this instance of ComplexQuadrilateral in Quadrilateral

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


