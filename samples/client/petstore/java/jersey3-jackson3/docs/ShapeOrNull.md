

# ShapeOrNull

The value may be a shape or the 'null' value. This is introduced in OAS schema >= 3.1.

## oneOf schemas
* [Quadrilateral](Quadrilateral.md)
* [Triangle](Triangle.md)

NOTE: this class is nullable.

## Example
```java
// Import classes:
import org.openapitools.client.model.ShapeOrNull;
import org.openapitools.client.model.Quadrilateral;
import org.openapitools.client.model.Triangle;

public class Example {
    public static void main(String[] args) {
        ShapeOrNull exampleShapeOrNull = new ShapeOrNull();

        // create a new Quadrilateral
        Quadrilateral exampleQuadrilateral = new Quadrilateral();
        // set ShapeOrNull to Quadrilateral
        exampleShapeOrNull.setActualInstance(exampleQuadrilateral);
        // to get back the Quadrilateral set earlier
        Quadrilateral testQuadrilateral = (Quadrilateral) exampleShapeOrNull.getActualInstance();

        // create a new Triangle
        Triangle exampleTriangle = new Triangle();
        // set ShapeOrNull to Triangle
        exampleShapeOrNull.setActualInstance(exampleTriangle);
        // to get back the Triangle set earlier
        Triangle testTriangle = (Triangle) exampleShapeOrNull.getActualInstance();
    }
}
```


