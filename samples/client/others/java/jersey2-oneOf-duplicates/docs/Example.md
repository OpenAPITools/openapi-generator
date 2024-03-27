

# Example

## oneOf schemas
* [List<BigDecimal>](List<BigDecimal>.md)
* [List<Integer>](List<Integer>.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Example;
import org.openapitools.client.model.List<BigDecimal>;
import org.openapitools.client.model.List<Integer>;

public class Example {
    public static void main(String[] args) {
        Example exampleExample = new Example();

        // create a new List<BigDecimal>
        List<BigDecimal> exampleList<BigDecimal> = new List<BigDecimal>();
        // set Example to List<BigDecimal>
        exampleExample.setActualInstance(exampleList<BigDecimal>);
        // to get back the List<BigDecimal> set earlier
        List<BigDecimal> testList<BigDecimal> = (List<BigDecimal>) exampleExample.getActualInstance();

        // create a new List<Integer>
        List<Integer> exampleList<Integer> = new List<Integer>();
        // set Example to List<Integer>
        exampleExample.setActualInstance(exampleList<Integer>);
        // to get back the List<Integer> set earlier
        List<Integer> testList<Integer> = (List<Integer>) exampleExample.getActualInstance();
    }
}
```


