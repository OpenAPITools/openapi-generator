

# ModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes

## oneOf schemas
* [OtherPropertyType](OtherPropertyType.md)
* [PropertyType](PropertyType.md)

NOTE: this class is nullable.

## Example
```java
// Import classes:
import org.openapitools.client.model.ModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes;
import org.openapitools.client.model.OtherPropertyType;
import org.openapitools.client.model.PropertyType;

public class Example {
    public static void main(String[] args) {
        ModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes exampleModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes = new ModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes();

        // create a new OtherPropertyType
        OtherPropertyType exampleOtherPropertyType = new OtherPropertyType();
        // set ModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes to OtherPropertyType
        exampleModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes.setActualInstance(exampleOtherPropertyType);
        // to get back the OtherPropertyType set earlier
        OtherPropertyType testOtherPropertyType = (OtherPropertyType) exampleModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes.getActualInstance();

        // create a new PropertyType
        PropertyType examplePropertyType = new PropertyType();
        // set ModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes to PropertyType
        exampleModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes.setActualInstance(examplePropertyType);
        // to get back the PropertyType set earlier
        PropertyType testPropertyType = (PropertyType) exampleModelWithNullableObjectPropertyPropertyWithNullAndTwoTypes.getActualInstance();
    }
}
```


