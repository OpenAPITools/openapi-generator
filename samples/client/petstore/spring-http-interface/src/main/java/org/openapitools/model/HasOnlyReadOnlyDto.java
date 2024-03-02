package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * HasOnlyReadOnlyDto
 */

@JsonTypeName("hasOnlyReadOnly")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class HasOnlyReadOnlyDto {

  private String bar;

  private String foo;

  public HasOnlyReadOnlyDto bar(String bar) {
    this.bar = bar;
    return this;
  }

  /**
   * Get bar
   * @return bar
  */
  
  @JsonProperty("bar")
  public String getBar() {
    return bar;
  }

  public void setBar(String bar) {
    this.bar = bar;
  }

  public HasOnlyReadOnlyDto foo(String foo) {
    this.foo = foo;
    return this;
  }

  /**
   * Get foo
   * @return foo
  */
  
  @JsonProperty("foo")
  public String getFoo() {
    return foo;
  }

  public void setFoo(String foo) {
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
    HasOnlyReadOnlyDto hasOnlyReadOnly = (HasOnlyReadOnlyDto) o;
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
    sb.append("class HasOnlyReadOnlyDto {\n");
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

    private HasOnlyReadOnlyDto instance;

    public Builder() {
      this(new HasOnlyReadOnlyDto());
    }

    protected Builder(HasOnlyReadOnlyDto instance) {
      this.instance = instance;
    }

    public HasOnlyReadOnlyDto.Builder bar(String bar) {
      this.instance.bar(bar);
      return this;
    }
    public HasOnlyReadOnlyDto.Builder foo(String foo) {
      this.instance.foo(foo);
      return this;
    }
    /**
    * returns a built HasOnlyReadOnlyDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public HasOnlyReadOnlyDto build() {
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
  * Create a builder with no initialized field.
  */
  public static HasOnlyReadOnlyDto.Builder builder() {
    return new HasOnlyReadOnlyDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public HasOnlyReadOnlyDto.Builder toBuilder() {
    HasOnlyReadOnlyDto.Builder builder = new HasOnlyReadOnlyDto.Builder();
    builder.instance.setBar(bar);
    builder.instance.setFoo(foo);
    return builder;
  }

}

