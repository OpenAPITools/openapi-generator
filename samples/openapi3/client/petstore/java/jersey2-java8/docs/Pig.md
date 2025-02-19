

# Pig

## oneOf schemas
* [BasquePig](BasquePig.md)
* [DanishPig](DanishPig.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Pig;
import org.openapitools.client.model.BasquePig;
import org.openapitools.client.model.DanishPig;

public class Example {
    public static void main(String[] args) {
        Pig examplePig = new Pig();

        // create a new BasquePig
        BasquePig exampleBasquePig = new BasquePig();
        // set Pig to BasquePig
        examplePig.setActualInstance(exampleBasquePig);
        // to get back the BasquePig set earlier
        BasquePig testBasquePig = (BasquePig) examplePig.getActualInstance();

        // create a new DanishPig
        DanishPig exampleDanishPig = new DanishPig();
        // set Pig to DanishPig
        examplePig.setActualInstance(exampleDanishPig);
        // to get back the DanishPig set earlier
        DanishPig testDanishPig = (DanishPig) examplePig.getActualInstance();
    }
}
```


