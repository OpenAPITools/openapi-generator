

# Mammal

## oneOf schemas
* [Pig](Pig.md)
* [Whale](Whale.md)
* [Zebra](Zebra.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Mammal;
import org.openapitools.client.model.Pig;
import org.openapitools.client.model.Whale;
import org.openapitools.client.model.Zebra;

public class Example {
    public static void main(String[] args) {
        Mammal exampleMammal = new Mammal();

        // create a new Pig
        Pig examplePig = new Pig();
        // set Mammal to Pig
        exampleMammal.setActualInstance(examplePig);
        // to get back the Pig set earlier
        Pig testPig = (Pig) exampleMammal.getActualInstance();

        // create a new Whale
        Whale exampleWhale = new Whale();
        // set Mammal to Whale
        exampleMammal.setActualInstance(exampleWhale);
        // to get back the Whale set earlier
        Whale testWhale = (Whale) exampleMammal.getActualInstance();

        // create a new Zebra
        Zebra exampleZebra = new Zebra();
        // set Mammal to Zebra
        exampleMammal.setActualInstance(exampleZebra);
        // to get back the Zebra set earlier
        Zebra testZebra = (Zebra) exampleMammal.getActualInstance();
    }
}
```


