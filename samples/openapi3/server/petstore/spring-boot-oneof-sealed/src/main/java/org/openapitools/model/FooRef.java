package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.model.EntityRef;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * FooRef
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public final class FooRef extends EntityRef implements FooRefOrValue {

  private @Nullable String foorefPropA;

  public FooRef() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public FooRef(String atType) {
    super(atType);
  }

  public FooRef foorefPropA(@Nullable String foorefPropA) {
    this.foorefPropA = foorefPropA;
    return this;
  }

  /**
   * Get foorefPropA
   * @return foorefPropA
   */
  
  @Schema(name = "foorefPropA", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("foorefPropA")
  public @Nullable String getFoorefPropA() {
    return foorefPropA;
  }

  public void setFoorefPropA(@Nullable String foorefPropA) {
    this.foorefPropA = foorefPropA;
  }


  public FooRef name(String name) {
    super.name(name);
    return this;
  }

  public FooRef atReferredType(String atReferredType) {
    super.atReferredType(atReferredType);
    return this;
  }

  public FooRef href(String href) {
    super.href(href);
    return this;
  }

  public FooRef id(String id) {
    super.id(id);
    return this;
  }

  public FooRef atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public FooRef atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public FooRef atType(String atType) {
    super.atType(atType);
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
    FooRef fooRef = (FooRef) o;
    return Objects.equals(this.foorefPropA, fooRef.foorefPropA) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(foorefPropA, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FooRef {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    foorefPropA: ").append(toIndentedString(foorefPropA)).append("\n");
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
  
  public static class Builder extends EntityRef.Builder {

    private FooRef instance;

    public Builder() {
      this(new FooRef());
    }

    protected Builder(FooRef instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(FooRef value) { 
      super.copyOf(value);
      this.instance.setFoorefPropA(value.foorefPropA);
      return this;
    }

    public FooRef.Builder foorefPropA(String foorefPropA) {
      this.instance.foorefPropA(foorefPropA);
      return this;
    }
    
    @Override
    public FooRef.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    @Override
    public FooRef.Builder atReferredType(String atReferredType) {
      this.instance.atReferredType(atReferredType);
      return this;
    }
    
    @Override
    public FooRef.Builder href(String href) {
      this.instance.href(href);
      return this;
    }
    
    @Override
    public FooRef.Builder id(String id) {
      this.instance.id(id);
      return this;
    }
    
    @Override
    public FooRef.Builder atSchemaLocation(String atSchemaLocation) {
      this.instance.atSchemaLocation(atSchemaLocation);
      return this;
    }
    
    @Override
    public FooRef.Builder atBaseType(String atBaseType) {
      this.instance.atBaseType(atBaseType);
      return this;
    }
    
    @Override
    public FooRef.Builder atType(String atType) {
      this.instance.atType(atType);
      return this;
    }
    
    /**
    * returns a built FooRef instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public FooRef build() {
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
  public static FooRef.Builder builder() {
    return new FooRef.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public FooRef.Builder toBuilder() {
    FooRef.Builder builder = new FooRef.Builder();
    return builder.copyOf(this);
  }

}

