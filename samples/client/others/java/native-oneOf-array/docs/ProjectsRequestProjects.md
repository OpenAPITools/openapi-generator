

# ProjectsRequestProjects

A list of projects, or \"*\" for all projects.

## oneOf schemas
* [List<String>](List<String>.md)
* [String](String.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.ProjectsRequestProjects;
import org.openapitools.client.model.List<String>;
import org.openapitools.client.model.String;

public class Example {
    public static void main(String[] args) {
        ProjectsRequestProjects exampleProjectsRequestProjects = new ProjectsRequestProjects();

        // create a new List<String>
        List<String> exampleList<String> = new List<String>();
        // set ProjectsRequestProjects to List<String>
        exampleProjectsRequestProjects.setActualInstance(exampleList<String>);
        // to get back the List<String> set earlier
        List<String> testList<String> = (List<String>) exampleProjectsRequestProjects.getActualInstance();

        // create a new String
        String exampleString = new String();
        // set ProjectsRequestProjects to String
        exampleProjectsRequestProjects.setActualInstance(exampleString);
        // to get back the String set earlier
        String testString = (String) exampleProjectsRequestProjects.getActualInstance();
    }
}
```


