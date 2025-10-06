package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Arrays;
import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.model.ParentWithNullable;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ChildWithNullable
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class ChildWithNullable extends ParentWithNullable {

  private Optional<String> otherProperty = Optional.empty();

  public ChildWithNullable otherProperty(String otherProperty) {
    this.otherProperty = Optional.ofNullable(otherProperty);
    return this;
  }

  /**
   * Get otherProperty
   * @return otherProperty
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("otherProperty")
  public Optional<String> getOtherProperty() {
    return otherProperty;
  }

  public void setOtherProperty(Optional<String> otherProperty) {
    this.otherProperty = otherProperty;
  }


  public ChildWithNullable type(TypeEnum type) {
    super.type(type);
    return this;
  }

  public ChildWithNullable nullableProperty(String nullableProperty) {
    super.nullableProperty(nullableProperty);
    return this;
  }
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ChildWithNullable childWithNullable = (ChildWithNullable) o;
    return Objects.equals(this.otherProperty, childWithNullable.otherProperty) &&
        super.equals(o);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(otherProperty, super.hashCode());
  }

  private static <T> int hashCodeNullable(JsonNullable<T> a) {
    if (a == null) {
      return 1;
    }
    return a.isPresent() ? Arrays.deepHashCode(new Object[]{a.get()}) : 31;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ChildWithNullable {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    otherProperty: ").append(toIndentedString(otherProperty)).append("\n");
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
  
  public static class Builder extends ParentWithNullable.Builder {

    private ChildWithNullable instance;

    public Builder() {
      this(new ChildWithNullable());
    }

    protected Builder(ChildWithNullable instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(ChildWithNullable value) { 
      super.copyOf(value);
      this.instance.setOtherProperty(value.otherProperty);
      return this;
    }

    public ChildWithNullable.Builder otherProperty(String otherProperty) {
      this.instance.otherProperty(otherProperty);
      return this;
    }
    
    @Override
    public ChildWithNullable.Builder type(TypeEnum type) {
      this.instance.type(type);
      return this;
    }
    
    @Override
    public ChildWithNullable.Builder nullableProperty(String nullableProperty) {
      this.instance.nullableProperty(nullableProperty);
      return this;
    }
    
    public ChildWithNullable.Builder nullableProperty(JsonNullable<String> nullableProperty) {
      this.instance.setNullableProperty(nullableProperty);
      return this;
    }
    
    /**
    * returns a built ChildWithNullable instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ChildWithNullable build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        super.build();
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
  public static ChildWithNullable.Builder builder() {
    return new ChildWithNullable.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ChildWithNullable.Builder toBuilder() {
    ChildWithNullable.Builder builder = new ChildWithNullable.Builder();
    return builder.copyOf(this);
  }

}

