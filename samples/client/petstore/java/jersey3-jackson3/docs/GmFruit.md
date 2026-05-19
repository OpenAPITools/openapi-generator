

# GmFruit

## anyOf schemas
* [Apple](Apple.md)
* [Banana](Banana.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.GmFruit;
import org.openapitools.client.model.Apple;
import org.openapitools.client.model.Banana;

public class Example {
    public static void main(String[] args) {
        GmFruit exampleGmFruit = new GmFruit();

        // create a new Apple
        Apple exampleApple = new Apple();
        // set GmFruit to Apple
        exampleGmFruit.setActualInstance(exampleApple);
        // to get back the Apple set earlier
        Apple testApple = (Apple) exampleGmFruit.getActualInstance();

        // create a new Banana
        Banana exampleBanana = new Banana();
        // set GmFruit to Banana
        exampleGmFruit.setActualInstance(exampleBanana);
        // to get back the Banana set earlier
        Banana testBanana = (Banana) exampleGmFruit.getActualInstance();
    }
}
```


