

# Shape

## oneOf schemas
* [Quadrilateral](Quadrilateral.md)
* [Triangle](Triangle.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Shape;
import org.openapitools.client.model.Quadrilateral;
import org.openapitools.client.model.Triangle;

public class Example {
    public static void main(String[] args) {
        Shape exampleShape = new Shape();

        // create a new Quadrilateral
        Quadrilateral exampleQuadrilateral = new Quadrilateral();
        // set Shape to Quadrilateral
        exampleShape.setActualInstance(exampleQuadrilateral);
        // to get back the Quadrilateral set earlier
        Quadrilateral testQuadrilateral = (Quadrilateral) exampleShape.getActualInstance();

        // create a new Triangle
        Triangle exampleTriangle = new Triangle();
        // set Shape to Triangle
        exampleShape.setActualInstance(exampleTriangle);
        // to get back the Triangle set earlier
        Triangle testTriangle = (Triangle) exampleShape.getActualInstance();
    }
}
```


