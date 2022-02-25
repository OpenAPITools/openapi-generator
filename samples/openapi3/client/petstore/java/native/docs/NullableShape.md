

# NullableShape

The value may be a shape or the 'null' value. The 'nullable' attribute was introduced in OAS schema >= 3.0 and has been deprecated in OAS schema >= 3.1.

## oneOf schemas
* [Quadrilateral](Quadrilateral.md)
* [Triangle](Triangle.md)

NOTE: this class is nullable.

## Example
```java
// Import classes:
import org.openapitools.client.model.NullableShape;
import org.openapitools.client.model.Quadrilateral;
import org.openapitools.client.model.Triangle;

public class Example {
    public static void main(String[] args) {
        NullableShape exampleNullableShape = new NullableShape();

        // create a new Quadrilateral
        Quadrilateral exampleQuadrilateral = new Quadrilateral();
        // set NullableShape to Quadrilateral
        exampleNullableShape.setActualInstance(exampleQuadrilateral);
        // to get back the Quadrilateral set earlier
        Quadrilateral testQuadrilateral = (Quadrilateral) exampleNullableShape.getActualInstance();

        // create a new Triangle
        Triangle exampleTriangle = new Triangle();
        // set NullableShape to Triangle
        exampleNullableShape.setActualInstance(exampleTriangle);
        // to get back the Triangle set earlier
        Triangle testTriangle = (Triangle) exampleNullableShape.getActualInstance();
    }
}
```


