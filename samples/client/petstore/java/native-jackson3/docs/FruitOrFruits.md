

# FruitOrFruits

## oneOf schemas
* [Apple](Apple.md)
* [Banana](Banana.md)
* [List<Apple>](List<Apple>.md)
* [List<Banana>](List<Banana>.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.FruitOrFruits;
import org.openapitools.client.model.Apple;
import org.openapitools.client.model.Banana;
import org.openapitools.client.model.List<Apple>;
import org.openapitools.client.model.List<Banana>;

public class Example {
    public static void main(String[] args) {
        FruitOrFruits exampleFruitOrFruits = new FruitOrFruits();

        // create a new Apple
        Apple exampleApple = new Apple();
        // set FruitOrFruits to Apple
        exampleFruitOrFruits.setActualInstance(exampleApple);
        // to get back the Apple set earlier
        Apple testApple = (Apple) exampleFruitOrFruits.getActualInstance();

        // create a new Banana
        Banana exampleBanana = new Banana();
        // set FruitOrFruits to Banana
        exampleFruitOrFruits.setActualInstance(exampleBanana);
        // to get back the Banana set earlier
        Banana testBanana = (Banana) exampleFruitOrFruits.getActualInstance();

        // create a new List<Apple>
        List<Apple> exampleList<Apple> = new List<Apple>();
        // set FruitOrFruits to List<Apple>
        exampleFruitOrFruits.setActualInstance(exampleList<Apple>);
        // to get back the List<Apple> set earlier
        List<Apple> testList<Apple> = (List<Apple>) exampleFruitOrFruits.getActualInstance();

        // create a new List<Banana>
        List<Banana> exampleList<Banana> = new List<Banana>();
        // set FruitOrFruits to List<Banana>
        exampleFruitOrFruits.setActualInstance(exampleList<Banana>);
        // to get back the List<Banana> set earlier
        List<Banana> testList<Banana> = (List<Banana>) exampleFruitOrFruits.getActualInstance();
    }
}
```


