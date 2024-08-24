

# AnyofArrayRequestMapProp

## oneOf schemas
* [Map<String, Object>](Map<String, Object>.md)
* [String](String.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.AnyofArrayRequestMapProp;
import org.openapitools.client.model.Map<String, Object>;
import org.openapitools.client.model.String;

public class Example {
    public static void main(String[] args) {
        AnyofArrayRequestMapProp exampleAnyofArrayRequestMapProp = new AnyofArrayRequestMapProp();

        // create a new Map<String, Object>
        Map<String, Object> exampleMap<String, Object> = new Map<String, Object>();
        // set AnyofArrayRequestMapProp to Map<String, Object>
        exampleAnyofArrayRequestMapProp.setActualInstance(exampleMap<String, Object>);
        // to get back the Map<String, Object> set earlier
        Map<String, Object> testMap<String, Object> = (Map<String, Object>) exampleAnyofArrayRequestMapProp.getActualInstance();

        // create a new String
        String exampleString = new String();
        // set AnyofArrayRequestMapProp to String
        exampleAnyofArrayRequestMapProp.setActualInstance(exampleString);
        // to get back the String set earlier
        String testString = (String) exampleAnyofArrayRequestMapProp.getActualInstance();
    }
}
```


