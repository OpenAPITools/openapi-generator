package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * HasOnlyReadOnly
 */

@JsonTypeName("hasOnlyReadOnly")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class HasOnlyReadOnly {

  private Optional<String> bar = Optional.empty();

  private Optional<String> foo = Optional.empty();

  public HasOnlyReadOnly bar(String bar) {
    this.bar = Optional.ofNullable(bar);
    return this;
  }

  /**
   * Get bar
   * @return bar
   */
  
  @ApiModelProperty(readOnly = true, value = "")
  @JsonProperty("bar")
  public Optional<String> getBar() {
    return bar;
  }

  public void setBar(Optional<String> bar) {
    this.bar = bar;
  }

  public HasOnlyReadOnly foo(String foo) {
    this.foo = Optional.ofNullable(foo);
    return this;
  }

  /**
   * Get foo
   * @return foo
   */
  
  @ApiModelProperty(readOnly = true, value = "")
  @JsonProperty("foo")
  public Optional<String> getFoo() {
    return foo;
  }

  public void setFoo(Optional<String> foo) {
    this.foo = foo;
  }

  @Override
  public boolean equals(Object o) {
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder {

    private HasOnlyReadOnly instance;

    public Builder() {
      this(new HasOnlyReadOnly());
    }

    protected Builder(HasOnlyReadOnly instance) {
      this.instance = instance;
    }

    protected Builder copyOf(HasOnlyReadOnly value) { 
      this.instance.setBar(value.bar);
      this.instance.setFoo(value.foo);
      return this;
    }

    public HasOnlyReadOnly.Builder bar(String bar) {
      this.instance.bar(bar);
      return this;
    }
    
    public HasOnlyReadOnly.Builder foo(String foo) {
      this.instance.foo(foo);
      return this;
    }
    
    /**
    * returns a built HasOnlyReadOnly instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public HasOnlyReadOnly build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static HasOnlyReadOnly.Builder builder() {
    return new HasOnlyReadOnly.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public HasOnlyReadOnly.Builder toBuilder() {
    HasOnlyReadOnly.Builder builder = new HasOnlyReadOnly.Builder();
    return builder.copyOf(this);
  }

}

