

# ModelWithNullableObjectPropertyNonNullableProperty

## oneOf schemas
* [BigDecimal](BigDecimal.md)
* [String](String.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.ModelWithNullableObjectPropertyNonNullableProperty;
import org.openapitools.client.model.BigDecimal;
import org.openapitools.client.model.String;

public class Example {
    public static void main(String[] args) {
        ModelWithNullableObjectPropertyNonNullableProperty exampleModelWithNullableObjectPropertyNonNullableProperty = new ModelWithNullableObjectPropertyNonNullableProperty();

        // create a new BigDecimal
        BigDecimal exampleBigDecimal = new BigDecimal();
        // set ModelWithNullableObjectPropertyNonNullableProperty to BigDecimal
        exampleModelWithNullableObjectPropertyNonNullableProperty.setActualInstance(exampleBigDecimal);
        // to get back the BigDecimal set earlier
        BigDecimal testBigDecimal = (BigDecimal) exampleModelWithNullableObjectPropertyNonNullableProperty.getActualInstance();

        // create a new String
        String exampleString = new String();
        // set ModelWithNullableObjectPropertyNonNullableProperty to String
        exampleModelWithNullableObjectPropertyNonNullableProperty.setActualInstance(exampleString);
        // to get back the String set earlier
        String testString = (String) exampleModelWithNullableObjectPropertyNonNullableProperty.getActualInstance();
    }
}
```


