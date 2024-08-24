

# AnyofArrayOneOfParamParameter

## oneOf schemas
* [Integer](Integer.md)
* [String](String.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.AnyofArrayOneOfParamParameter;
import org.openapitools.client.model.Integer;
import org.openapitools.client.model.String;

public class Example {
    public static void main(String[] args) {
        AnyofArrayOneOfParamParameter exampleAnyofArrayOneOfParamParameter = new AnyofArrayOneOfParamParameter();

        // create a new Integer
        Integer exampleInteger = new Integer();
        // set AnyofArrayOneOfParamParameter to Integer
        exampleAnyofArrayOneOfParamParameter.setActualInstance(exampleInteger);
        // to get back the Integer set earlier
        Integer testInteger = (Integer) exampleAnyofArrayOneOfParamParameter.getActualInstance();

        // create a new String
        String exampleString = new String();
        // set AnyofArrayOneOfParamParameter to String
        exampleAnyofArrayOneOfParamParameter.setActualInstance(exampleString);
        // to get back the String set earlier
        String testString = (String) exampleAnyofArrayOneOfParamParameter.getActualInstance();
    }
}
```


