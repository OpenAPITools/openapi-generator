

# Triangle

## oneOf schemas
* [EquilateralTriangle](EquilateralTriangle.md)
* [IsoscelesTriangle](IsoscelesTriangle.md)
* [ScaleneTriangle](ScaleneTriangle.md)

## Example
```java
// Import classes:
import org.openapitools.client.model.Triangle;
import org.openapitools.client.model.EquilateralTriangle;
import org.openapitools.client.model.IsoscelesTriangle;
import org.openapitools.client.model.ScaleneTriangle;

public class Example {
    public static void main(String[] args) {
        Triangle exampleTriangle = new Triangle();

        // create a new EquilateralTriangle
        EquilateralTriangle exampleEquilateralTriangle = new EquilateralTriangle();
        // set Triangle to EquilateralTriangle
        exampleTriangle.setActualInstance(exampleEquilateralTriangle);
        // to get back the EquilateralTriangle set earlier
        EquilateralTriangle testEquilateralTriangle = (EquilateralTriangle) exampleTriangle.getActualInstance();

        // create a new IsoscelesTriangle
        IsoscelesTriangle exampleIsoscelesTriangle = new IsoscelesTriangle();
        // set Triangle to IsoscelesTriangle
        exampleTriangle.setActualInstance(exampleIsoscelesTriangle);
        // to get back the IsoscelesTriangle set earlier
        IsoscelesTriangle testIsoscelesTriangle = (IsoscelesTriangle) exampleTriangle.getActualInstance();

        // create a new ScaleneTriangle
        ScaleneTriangle exampleScaleneTriangle = new ScaleneTriangle();
        // set Triangle to ScaleneTriangle
        exampleTriangle.setActualInstance(exampleScaleneTriangle);
        // to get back the ScaleneTriangle set earlier
        ScaleneTriangle testScaleneTriangle = (ScaleneTriangle) exampleTriangle.getActualInstance();
    }
}
```


