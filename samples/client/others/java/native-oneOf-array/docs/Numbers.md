

# Numbers

## oneOf schemas
* [List<BigDecimal>](List<BigDecimal>.md)
* [List<Integer>](List<Integer>.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Numbers;
import org.openapitools.client.model.List<BigDecimal>;
import org.openapitools.client.model.List<Integer>;

public class Example {
    public static void main(String[] args) {
        Numbers exampleNumbers = new Numbers();

        // create a new List<BigDecimal>
        List<BigDecimal> exampleList<BigDecimal> = new List<BigDecimal>();
        // set Numbers to List<BigDecimal>
        exampleNumbers.setActualInstance(exampleList<BigDecimal>);
        // to get back the List<BigDecimal> set earlier
        List<BigDecimal> testList<BigDecimal> = (List<BigDecimal>) exampleNumbers.getActualInstance();

        // create a new List<Integer>
        List<Integer> exampleList<Integer> = new List<Integer>();
        // set Numbers to List<Integer>
        exampleNumbers.setActualInstance(exampleList<Integer>);
        // to get back the List<Integer> set earlier
        List<Integer> testList<Integer> = (List<Integer>) exampleNumbers.getActualInstance();
    }
}
```


