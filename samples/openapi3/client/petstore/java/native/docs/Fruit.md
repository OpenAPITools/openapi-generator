

# Fruit

## oneOf schemas
* [Apple](Apple.md)
* [Banana](Banana.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Fruit;
import org.openapitools.client.model.Apple;
import org.openapitools.client.model.Banana;

public class Example {
    public static void main(String[] args) {
        Fruit exampleFruit = new Fruit();

        // create a new Apple
        Apple exampleApple = new Apple();
        // set Fruit to Apple
        exampleFruit.setActualInstance(exampleApple);
        // to get back the Apple set earlier
        Apple testApple = (Apple) exampleFruit.getActualInstance();

        // create a new Banana
        Banana exampleBanana = new Banana();
        // set Fruit to Banana
        exampleFruit.setActualInstance(exampleBanana);
        // to get back the Banana set earlier
        Banana testBanana = (Banana) exampleFruit.getActualInstance();
    }
}
```


