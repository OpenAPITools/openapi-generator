

# AnyofArray200Response

## anyOf schemas
* [List<AnyOf1>](List<AnyOf1>.md)
* [List<AnyOf2>](List<AnyOf2>.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.AnyofArray200Response;
import org.openapitools.client.model.List<AnyOf1>;
import org.openapitools.client.model.List<AnyOf2>;

public class Example {
    public static void main(String[] args) {
        AnyofArray200Response exampleAnyofArray200Response = new AnyofArray200Response();

        // create a new List<AnyOf1>
        List<AnyOf1> exampleList<AnyOf1> = new List<AnyOf1>();
        // set AnyofArray200Response to List<AnyOf1>
        exampleAnyofArray200Response.setActualInstance(exampleList<AnyOf1>);
        // to get back the List<AnyOf1> set earlier
        List<AnyOf1> testList<AnyOf1> = (List<AnyOf1>) exampleAnyofArray200Response.getActualInstance();

        // create a new List<AnyOf2>
        List<AnyOf2> exampleList<AnyOf2> = new List<AnyOf2>();
        // set AnyofArray200Response to List<AnyOf2>
        exampleAnyofArray200Response.setActualInstance(exampleList<AnyOf2>);
        // to get back the List<AnyOf2> set earlier
        List<AnyOf2> testList<AnyOf2> = (List<AnyOf2>) exampleAnyofArray200Response.getActualInstance();
    }
}
```


