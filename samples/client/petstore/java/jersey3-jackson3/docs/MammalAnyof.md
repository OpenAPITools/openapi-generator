

# MammalAnyof

## anyOf schemas
* [Pig](Pig.md)
* [Whale](Whale.md)
* [Zebra](Zebra.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.MammalAnyof;
import org.openapitools.client.model.Pig;
import org.openapitools.client.model.Whale;
import org.openapitools.client.model.Zebra;

public class Example {
    public static void main(String[] args) {
        MammalAnyof exampleMammalAnyof = new MammalAnyof();

        // create a new Pig
        Pig examplePig = new Pig();
        // set MammalAnyof to Pig
        exampleMammalAnyof.setActualInstance(examplePig);
        // to get back the Pig set earlier
        Pig testPig = (Pig) exampleMammalAnyof.getActualInstance();

        // create a new Whale
        Whale exampleWhale = new Whale();
        // set MammalAnyof to Whale
        exampleMammalAnyof.setActualInstance(exampleWhale);
        // to get back the Whale set earlier
        Whale testWhale = (Whale) exampleMammalAnyof.getActualInstance();

        // create a new Zebra
        Zebra exampleZebra = new Zebra();
        // set MammalAnyof to Zebra
        exampleMammalAnyof.setActualInstance(exampleZebra);
        // to get back the Zebra set earlier
        Zebra testZebra = (Zebra) exampleMammalAnyof.getActualInstance();
    }
}
```


