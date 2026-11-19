

# Projects

## oneOf schemas
* [List<String>](List<String>.md)
* [UUID](UUID.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Projects;
import org.openapitools.client.model.List<String>;
import org.openapitools.client.model.UUID;

public class Example {
    public static void main(String[] args) {
        Projects exampleProjects = new Projects();

        // create a new List<String>
        List<String> exampleList<String> = new List<String>();
        // set Projects to List<String>
        exampleProjects.setActualInstance(exampleList<String>);
        // to get back the List<String> set earlier
        List<String> testList<String> = (List<String>) exampleProjects.getActualInstance();

        // create a new UUID
        UUID exampleUUID = new UUID();
        // set Projects to UUID
        exampleProjects.setActualInstance(exampleUUID);
        // to get back the UUID set earlier
        UUID testUUID = (UUID) exampleProjects.getActualInstance();
    }
}
```


