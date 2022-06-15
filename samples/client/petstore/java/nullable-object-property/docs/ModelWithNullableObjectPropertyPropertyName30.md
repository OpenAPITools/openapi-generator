

# ModelWithNullableObjectPropertyPropertyName30

## oneOf schemas
* [PropertyType](PropertyType.md)

NOTE: this class is nullable.

## Example
```java
// Import classes:
import org.openapitools.client.model.ModelWithNullableObjectPropertyPropertyName30;
import org.openapitools.client.model.PropertyType;

public class Example {
    public static void main(String[] args) {
        ModelWithNullableObjectPropertyPropertyName30 exampleModelWithNullableObjectPropertyPropertyName30 = new ModelWithNullableObjectPropertyPropertyName30();

        // create a new PropertyType
        PropertyType examplePropertyType = new PropertyType();
        // set ModelWithNullableObjectPropertyPropertyName30 to PropertyType
        exampleModelWithNullableObjectPropertyPropertyName30.setActualInstance(examplePropertyType);
        // to get back the PropertyType set earlier
        PropertyType testPropertyType = (PropertyType) exampleModelWithNullableObjectPropertyPropertyName30.getActualInstance();
    }
}
```


