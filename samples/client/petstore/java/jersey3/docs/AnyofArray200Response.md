

# AnyofArray200Response

## anyOf schemas
* [List<@Valid AnyOf1>](List<@Valid AnyOf1>.md)
* [List<@Valid AnyOf2>](List<@Valid AnyOf2>.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.AnyofArray200Response;
import org.openapitools.client.model.List<@Valid AnyOf1>;
import org.openapitools.client.model.List<@Valid AnyOf2>;

public class Example {
    public static void main(String[] args) {
        AnyofArray200Response exampleAnyofArray200Response = new AnyofArray200Response();

        // create a new List<@Valid AnyOf1>
        List<@Valid AnyOf1> exampleList<@Valid AnyOf1> = new List<@Valid AnyOf1>();
        // set AnyofArray200Response to List<@Valid AnyOf1>
        exampleAnyofArray200Response.setActualInstance(exampleList<@Valid AnyOf1>);
        // to get back the List<@Valid AnyOf1> set earlier
        List<@Valid AnyOf1> testList<@Valid AnyOf1> = (List<@Valid AnyOf1>) exampleAnyofArray200Response.getActualInstance();

        // create a new List<@Valid AnyOf2>
        List<@Valid AnyOf2> exampleList<@Valid AnyOf2> = new List<@Valid AnyOf2>();
        // set AnyofArray200Response to List<@Valid AnyOf2>
        exampleAnyofArray200Response.setActualInstance(exampleList<@Valid AnyOf2>);
        // to get back the List<@Valid AnyOf2> set earlier
        List<@Valid AnyOf2> testList<@Valid AnyOf2> = (List<@Valid AnyOf2>) exampleAnyofArray200Response.getActualInstance();
    }
}
```


