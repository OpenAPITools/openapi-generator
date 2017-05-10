package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * HasOnlyReadOnly
 */

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
  @ApiModelProperty(readOnly = true, value = "")


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
  @ApiModelProperty(readOnly = true, value = "")


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
    return Objects.equals(this.bar, hasOnlyReadOnly.bar) &&
        Objects.equals(this.foo, hasOnlyReadOnly.foo);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bar, foo);
  }

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
}

