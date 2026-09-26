

# TagsOrLabel

## anyOf schemas
* [Integer](Integer.md)
* [List<String>](List<String>.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.TagsOrLabel;
import org.openapitools.client.model.Integer;
import org.openapitools.client.model.List<String>;

public class Example {
    public static void main(String[] args) {
        TagsOrLabel exampleTagsOrLabel = new TagsOrLabel();

        // create a new Integer
        Integer exampleInteger = new Integer();
        // set TagsOrLabel to Integer
        exampleTagsOrLabel.setActualInstance(exampleInteger);
        // to get back the Integer set earlier
        Integer testInteger = (Integer) exampleTagsOrLabel.getActualInstance();

        // create a new List<String>
        List<String> exampleList<String> = new List<String>();
        // set TagsOrLabel to List<String>
        exampleTagsOrLabel.setActualInstance(exampleList<String>);
        // to get back the List<String> set earlier
        List<String> testList<String> = (List<String>) exampleTagsOrLabel.getActualInstance();
    }
}
```


