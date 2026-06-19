

# PostRequest

## oneOf schemas
* [Map<String, Object>](Map<String, Object>.md)
* [SchemaA](SchemaA.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.PostRequest;
import org.openapitools.client.model.Map<String, Object>;
import org.openapitools.client.model.SchemaA;

public class Example {
    public static void main(String[] args) {
        PostRequest examplePostRequest = new PostRequest();

        // create a new Map<String, Object>
        Map<String, Object> exampleMap<String, Object> = new Map<String, Object>();
        // set PostRequest to Map<String, Object>
        examplePostRequest.setActualInstance(exampleMap<String, Object>);
        // to get back the Map<String, Object> set earlier
        Map<String, Object> testMap<String, Object> = (Map<String, Object>) examplePostRequest.getActualInstance();

        // create a new SchemaA
        SchemaA exampleSchemaA = new SchemaA();
        // set PostRequest to SchemaA
        examplePostRequest.setActualInstance(exampleSchemaA);
        // to get back the SchemaA set earlier
        SchemaA testSchemaA = (SchemaA) examplePostRequest.getActualInstance();
    }
}
```


