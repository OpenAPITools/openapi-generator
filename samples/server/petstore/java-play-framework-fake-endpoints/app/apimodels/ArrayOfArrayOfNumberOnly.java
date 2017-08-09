package apimodels;

import java.util.Objects;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import javax.validation.constraints.*;
/**
 * ArrayOfArrayOfNumberOnly
 */

public class ArrayOfArrayOfNumberOnly   {
  @JsonProperty("ArrayArrayNumber")
  private List<List<BigDecimal>> arrayArrayNumber = null;

  public ArrayOfArrayOfNumberOnly arrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
    this.arrayArrayNumber = arrayArrayNumber;
    return this;
  }

  public ArrayOfArrayOfNumberOnly addArrayArrayNumberItem(List<BigDecimal> arrayArrayNumberItem) {
    if (this.arrayArrayNumber == null) {
      this.arrayArrayNumber = new ArrayList<List<BigDecimal>>();
    }
    this.arrayArrayNumber.add(arrayArrayNumberItem);
    return this;
  }

   /**
   * Get arrayArrayNumber
   * @return arrayArrayNumber
  **/
  @Valid
  public List<List<BigDecimal>> getArrayArrayNumber() {
    return arrayArrayNumber;
  }

  public void setArrayArrayNumber(List<List<BigDecimal>> arrayArrayNumber) {
    this.arrayArrayNumber = arrayArrayNumber;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ArrayOfArrayOfNumberOnly arrayOfArrayOfNumberOnly = (ArrayOfArrayOfNumberOnly) o;
    return Objects.equals(this.arrayArrayNumber, arrayOfArrayOfNumberOnly.arrayArrayNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(arrayArrayNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ArrayOfArrayOfNumberOnly {\n");
    
    sb.append("    arrayArrayNumber: ").append(toIndentedString(arrayArrayNumber)).append("\n");
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
    Set<ConstraintViolation<ArrayOfArrayOfNumberOnly>> constraintViolations = validator.validate(this);
    if (constraintViolations.size() > 0) {
      StringBuilder errors = new StringBuilder();
      for (ConstraintViolation<ArrayOfArrayOfNumberOnly> contraintes : constraintViolations) {
        errors.append(String.format("%s.%s %s\n",
            contraintes.getRootBeanClass().getSimpleName(),
            contraintes.getPropertyPath(),
            contraintes.getMessage()));
      }
      throw new RuntimeException("Bean validation : " + errors);
    }
  }
}

