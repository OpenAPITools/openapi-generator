

# Quadrilateral

## oneOf schemas
* [ComplexQuadrilateral](ComplexQuadrilateral.md)
* [SimpleQuadrilateral](SimpleQuadrilateral.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Quadrilateral;
import org.openapitools.client.model.ComplexQuadrilateral;
import org.openapitools.client.model.SimpleQuadrilateral;

public class Example {
    public static void main(String[] args) {
        Quadrilateral exampleQuadrilateral = new Quadrilateral();

        // create a new ComplexQuadrilateral
        ComplexQuadrilateral exampleComplexQuadrilateral = new ComplexQuadrilateral();
        // set Quadrilateral to ComplexQuadrilateral
        exampleQuadrilateral.setActualInstance(exampleComplexQuadrilateral);
        // to get back the ComplexQuadrilateral set earlier
        ComplexQuadrilateral testComplexQuadrilateral = (ComplexQuadrilateral) exampleQuadrilateral.getActualInstance();

        // create a new SimpleQuadrilateral
        SimpleQuadrilateral exampleSimpleQuadrilateral = new SimpleQuadrilateral();
        // set Quadrilateral to SimpleQuadrilateral
        exampleQuadrilateral.setActualInstance(exampleSimpleQuadrilateral);
        // to get back the SimpleQuadrilateral set earlier
        SimpleQuadrilateral testSimpleQuadrilateral = (SimpleQuadrilateral) exampleQuadrilateral.getActualInstance();
    }
}
```


