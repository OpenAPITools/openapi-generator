package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.model.Entity;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Pasta
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public class Pasta extends Entity {

  private @Nullable String vendor;

  public Pasta() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Pasta(String atType) {
    super(atType);
  }

  public Pasta vendor(@Nullable String vendor) {
    this.vendor = vendor;
    return this;
  }

  /**
   * Get vendor
   * @return vendor
   */
  
  @Schema(name = "vendor", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("vendor")
  public @Nullable String getVendor() {
    return vendor;
  }

  public void setVendor(@Nullable String vendor) {
    this.vendor = vendor;
  }


  public Pasta href(String href) {
    super.href(href);
    return this;
  }

  public Pasta id(String id) {
    super.id(id);
    return this;
  }

  public Pasta atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public Pasta atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public Pasta atType(String atType) {
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
    Pasta pasta = (Pasta) o;
    return Objects.equals(this.vendor, pasta.vendor) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(vendor, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pasta {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    vendor: ").append(toIndentedString(vendor)).append("\n");
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
  
  public static class Builder extends Entity.Builder {

    private Pasta instance;

    public Builder() {
      this(new Pasta());
    }

    protected Builder(Pasta instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(Pasta value) { 
      super.copyOf(value);
      this.instance.setVendor(value.vendor);
      return this;
    }

    public Pasta.Builder vendor(String vendor) {
      this.instance.vendor(vendor);
      return this;
    }
    
    @Override
    public Pasta.Builder href(String href) {
      this.instance.href(href);
      return this;
    }
    
    @Override
    public Pasta.Builder id(String id) {
      this.instance.id(id);
      return this;
    }
    
    @Override
    public Pasta.Builder atSchemaLocation(String atSchemaLocation) {
      this.instance.atSchemaLocation(atSchemaLocation);
      return this;
    }
    
    @Override
    public Pasta.Builder atBaseType(String atBaseType) {
      this.instance.atBaseType(atBaseType);
      return this;
    }
    
    @Override
    public Pasta.Builder atType(String atType) {
      this.instance.atType(atType);
      return this;
    }
    
    /**
    * returns a built Pasta instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Pasta build() {
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
  public static Pasta.Builder builder() {
    return new Pasta.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Pasta.Builder toBuilder() {
    Pasta.Builder builder = new Pasta.Builder();
    return builder.copyOf(this);
  }

}

