package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import java.util.Map;
import org.openapitools.model.Base;
import org.springframework.lang.Nullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Child
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class Child extends Base {

  private @Nullable String x;

  public Child() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Child(String kind) {
    super(kind);
  }

  public Child x(@Nullable String x) {
    this.x = x;
    return this;
  }

  /**
   * Get x
   * @return x
   */
  
  @Schema(name = "x", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("x")
  public @Nullable String getX() {
    return x;
  }

  @JsonProperty("x")
  public void setX(@Nullable String x) {
    this.x = x;
  }


  public Child kind(String kind) {
    super.kind(kind);
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
    Child child = (Child) o;
    return Objects.equals(this.x, child.x) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(x, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Child {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    x: ").append(toIndentedString(x)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(@Nullable Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder extends Base.Builder {

    private Child instance;

    public Builder() {
      this(new Child());
    }

    protected Builder(Child instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(Child value) { 
      super.copyOf(value);
      this.instance.setX(value.x);
      return this;
    }

    public Child.Builder x(String x) {
      this.instance.x(x);
      return this;
    }
    
    @Override
    public Child.Builder kind(String kind) {
      this.instance.kind(kind);
      return this;
    }
    
    /**
    * returns a built Child instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Child build() {
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
  public static Child.Builder builder() {
    return new Child.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Child.Builder toBuilder() {
    Child.Builder builder = new Child.Builder();
    return builder.copyOf(this);
  }

}

