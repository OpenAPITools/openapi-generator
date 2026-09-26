

# CountsOrName

## oneOf schemas
* [Map<String, Integer>](Map<String, Integer>.md)
* [String](String.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.CountsOrName;
import org.openapitools.client.model.Map<String, Integer>;
import org.openapitools.client.model.String;

public class Example {
    public static void main(String[] args) {
        CountsOrName exampleCountsOrName = new CountsOrName();

        // create a new Map<String, Integer>
        Map<String, Integer> exampleMap<String, Integer> = new Map<String, Integer>();
        // set CountsOrName to Map<String, Integer>
        exampleCountsOrName.setActualInstance(exampleMap<String, Integer>);
        // to get back the Map<String, Integer> set earlier
        Map<String, Integer> testMap<String, Integer> = (Map<String, Integer>) exampleCountsOrName.getActualInstance();

        // create a new String
        String exampleString = new String();
        // set CountsOrName to String
        exampleCountsOrName.setActualInstance(exampleString);
        // to get back the String set earlier
        String testString = (String) exampleCountsOrName.getActualInstance();
    }
}
```


