

# FruitReq

## oneOf schemas
* [AppleReq](AppleReq.md)
* [BananaReq](BananaReq.md)

NOTE: this class is nullable.

## Example
```java
// Import classes:
import org.openapitools.client.model.FruitReq;
import org.openapitools.client.model.AppleReq;
import org.openapitools.client.model.BananaReq;

public class Example {
    public static void main(String[] args) {
        FruitReq exampleFruitReq = new FruitReq();

        // create a new AppleReq
        AppleReq exampleAppleReq = new AppleReq();
        // set FruitReq to AppleReq
        exampleFruitReq.setActualInstance(exampleAppleReq);
        // to get back the AppleReq set earlier
        AppleReq testAppleReq = (AppleReq) exampleFruitReq.getActualInstance();

        // create a new BananaReq
        BananaReq exampleBananaReq = new BananaReq();
        // set FruitReq to BananaReq
        exampleFruitReq.setActualInstance(exampleBananaReq);
        // to get back the BananaReq set earlier
        BananaReq testBananaReq = (BananaReq) exampleFruitReq.getActualInstance();
    }
}
```


