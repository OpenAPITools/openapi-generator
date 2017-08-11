package apimodels;

import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * HasOnlyReadOnly
 */

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class HasOnlyReadOnly   {
  @JsonProperty("bar")
  private String bar = null;

  @JsonProperty("foo")
  private String foo = null;

  public HasOnlyReadOnly bar(String bar) {
    this.bar = bar;
    return this;
  }

   /**
   * Get bar
   * @return bar
  **/
    public String getBar() {
    return bar;
  }

  public void setBar(String bar) {
    this.bar = bar;
  }

  public HasOnlyReadOnly foo(String foo) {
    this.foo = foo;
    return this;
  }

   /**
   * Get foo
   * @return foo
  **/
    public String getFoo() {
    return foo;
  }

  public void setFoo(String foo) {
    this.foo = foo;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    HasOnlyReadOnly hasOnlyReadOnly = (HasOnlyReadOnly) o;
    return Objects.equals(bar, hasOnlyReadOnly.bar) &&
        Objects.equals(foo, hasOnlyReadOnly.foo);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bar, foo);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class HasOnlyReadOnly {\n");
    
    sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
    sb.append("    foo: ").append(toIndentedString(foo)).append("\n");
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
    Set<ConstraintViolation<HasOnlyReadOnly>> constraintViolations = validator.validate(this);
    if (constraintViolations.size() > 0) {
      StringBuilder errors = new StringBuilder();
      for (ConstraintViolation<HasOnlyReadOnly> contraintes : constraintViolations) {
        errors.append(String.format("%s.%s %s\n",
            contraintes.getRootBeanClass().getSimpleName(),
            contraintes.getPropertyPath(),
            contraintes.getMessage()));
      }
      throw new RuntimeException("Bean validation : " + errors);
    }
  }
}

