package apimodels;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * ArrayOfNumberOnly
 */

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class ArrayOfNumberOnly   {
  @JsonProperty("ArrayNumber")
  private List<BigDecimal> arrayNumber = null;

  public ArrayOfNumberOnly arrayNumber(List<BigDecimal> arrayNumber) {
    this.arrayNumber = arrayNumber;
    return this;
  }

  public ArrayOfNumberOnly addArrayNumberItem(BigDecimal arrayNumberItem) {
    if (arrayNumber == null) {
      arrayNumber = new ArrayList<>();
    }
    arrayNumber.add(arrayNumberItem);
    return this;
  }

   /**
   * Get arrayNumber
   * @return arrayNumber
  **/
  @Valid
  public List<BigDecimal> getArrayNumber() {
    return arrayNumber;
  }

  public void setArrayNumber(List<BigDecimal> arrayNumber) {
    this.arrayNumber = arrayNumber;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ArrayOfNumberOnly arrayOfNumberOnly = (ArrayOfNumberOnly) o;
    return Objects.equals(arrayNumber, arrayOfNumberOnly.arrayNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(arrayNumber);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayOfNumberOnly {\n");
    
    sb.append("    arrayNumber: ").append(toIndentedString(arrayNumber)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }

  public void validate() {
    ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
    Validator validator = factory.getValidator();
    Set<ConstraintViolation<ArrayOfNumberOnly>> constraintViolations = validator.validate(this);
    if (constraintViolations.size() > 0) {
      StringBuilder errors = new StringBuilder();
      for (ConstraintViolation<ArrayOfNumberOnly> contraintes : constraintViolations) {
        errors.append(String.format("%s.%s %s\n",
            contraintes.getRootBeanClass().getSimpleName(),
            contraintes.getPropertyPath(),
            contraintes.getMessage()));
      }
      throw new RuntimeException("Bean validation : " + errors);
    }
  }
}

