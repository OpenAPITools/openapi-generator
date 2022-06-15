

# ModelWithNullableObjectPropertyPropertyName31

## oneOf schemas
* [PropertyType](PropertyType.md)

NOTE: this class is nullable.

## Example
```java
// Import classes:
import org.openapitools.client.model.ModelWithNullableObjectPropertyPropertyName31;
import org.openapitools.client.model.PropertyType;

public class Example {
    public static void main(String[] args) {
        ModelWithNullableObjectPropertyPropertyName31 exampleModelWithNullableObjectPropertyPropertyName31 = new ModelWithNullableObjectPropertyPropertyName31();

        // create a new PropertyType
        PropertyType examplePropertyType = new PropertyType();
        // set ModelWithNullableObjectPropertyPropertyName31 to PropertyType
        exampleModelWithNullableObjectPropertyPropertyName31.setActualInstance(examplePropertyType);
        // to get back the PropertyType set earlier
        PropertyType testPropertyType = (PropertyType) exampleModelWithNullableObjectPropertyPropertyName31.getActualInstance();
    }
}
```


