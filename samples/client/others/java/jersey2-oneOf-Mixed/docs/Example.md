

# Example

## oneOf schemas
* [List<Integer>](List<Integer>.md)
* [UUID](UUID.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Example;
import org.openapitools.client.model.List<Integer>;
import org.openapitools.client.model.UUID;

public class Example {
    public static void main(String[] args) {
        Example exampleExample = new Example();

        // create a new List<Integer>
        List<Integer> exampleList<Integer> = new List<Integer>();
        // set Example to List<Integer>
        exampleExample.setActualInstance(exampleList<Integer>);
        // to get back the List<Integer> set earlier
        List<Integer> testList<Integer> = (List<Integer>) exampleExample.getActualInstance();

        // create a new UUID
        UUID exampleUUID = new UUID();
        // set Example to UUID
        exampleExample.setActualInstance(exampleUUID);
        // to get back the UUID set earlier
        UUID testUUID = (UUID) exampleExample.getActualInstance();
    }
}
```


