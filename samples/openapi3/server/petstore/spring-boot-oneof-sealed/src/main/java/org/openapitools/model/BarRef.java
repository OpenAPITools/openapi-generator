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
 * BarRef
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public final class BarRef extends EntityRef implements BarRefOrValue {

  public BarRef() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public BarRef(String atType) {
    super(atType);
  }


  public BarRef name(String name) {
    super.name(name);
    return this;
  }

  public BarRef atReferredType(String atReferredType) {
    super.atReferredType(atReferredType);
    return this;
  }

  public BarRef href(String href) {
    super.href(href);
    return this;
  }

  public BarRef id(String id) {
    super.id(id);
    return this;
  }

  public BarRef atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public BarRef atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public BarRef atType(String atType) {
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
    return super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class BarRef {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
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

    private BarRef instance;

    public Builder() {
      this(new BarRef());
    }

    protected Builder(BarRef instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(BarRef value) { 
      super.copyOf(value);
      return this;
    }

    @Override
    public BarRef.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    @Override
    public BarRef.Builder atReferredType(String atReferredType) {
      this.instance.atReferredType(atReferredType);
      return this;
    }
    
    @Override
    public BarRef.Builder href(String href) {
      this.instance.href(href);
      return this;
    }
    
    @Override
    public BarRef.Builder id(String id) {
      this.instance.id(id);
      return this;
    }
    
    @Override
    public BarRef.Builder atSchemaLocation(String atSchemaLocation) {
      this.instance.atSchemaLocation(atSchemaLocation);
      return this;
    }
    
    @Override
    public BarRef.Builder atBaseType(String atBaseType) {
      this.instance.atBaseType(atBaseType);
      return this;
    }
    
    @Override
    public BarRef.Builder atType(String atType) {
      this.instance.atType(atType);
      return this;
    }
    
    /**
    * returns a built BarRef instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public BarRef build() {
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
  public static BarRef.Builder builder() {
    return new BarRef.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public BarRef.Builder toBuilder() {
    BarRef.Builder builder = new BarRef.Builder();
    return builder.copyOf(this);
  }

}

