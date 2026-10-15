

# GmFruitOrFruits

## anyOf schemas
* [Apple](Apple.md)
* [Banana](Banana.md)
* [List<Apple>](List<Apple>.md)
* [List<Banana>](List<Banana>.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.GmFruitOrFruits;
import org.openapitools.client.model.Apple;
import org.openapitools.client.model.Banana;
import org.openapitools.client.model.List<Apple>;
import org.openapitools.client.model.List<Banana>;

public class Example {
    public static void main(String[] args) {
        GmFruitOrFruits exampleGmFruitOrFruits = new GmFruitOrFruits();

        // create a new Apple
        Apple exampleApple = new Apple();
        // set GmFruitOrFruits to Apple
        exampleGmFruitOrFruits.setActualInstance(exampleApple);
        // to get back the Apple set earlier
        Apple testApple = (Apple) exampleGmFruitOrFruits.getActualInstance();

        // create a new Banana
        Banana exampleBanana = new Banana();
        // set GmFruitOrFruits to Banana
        exampleGmFruitOrFruits.setActualInstance(exampleBanana);
        // to get back the Banana set earlier
        Banana testBanana = (Banana) exampleGmFruitOrFruits.getActualInstance();

        // create a new List<Apple>
        List<Apple> exampleList<Apple> = new List<Apple>();
        // set GmFruitOrFruits to List<Apple>
        exampleGmFruitOrFruits.setActualInstance(exampleList<Apple>);
        // to get back the List<Apple> set earlier
        List<Apple> testList<Apple> = (List<Apple>) exampleGmFruitOrFruits.getActualInstance();

        // create a new List<Banana>
        List<Banana> exampleList<Banana> = new List<Banana>();
        // set GmFruitOrFruits to List<Banana>
        exampleGmFruitOrFruits.setActualInstance(exampleList<Banana>);
        // to get back the List<Banana> set earlier
        List<Banana> testList<Banana> = (List<Banana>) exampleGmFruitOrFruits.getActualInstance();
    }
}
```


