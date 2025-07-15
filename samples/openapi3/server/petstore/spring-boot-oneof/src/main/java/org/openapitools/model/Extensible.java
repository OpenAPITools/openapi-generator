package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Extensible
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public class Extensible {

  private @Nullable String atSchemaLocation;

  private @Nullable String atBaseType;

  private String atType;

  public Extensible() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Extensible(String atType) {
    this.atType = atType;
  }

  public Extensible atSchemaLocation(@Nullable String atSchemaLocation) {
    this.atSchemaLocation = atSchemaLocation;
    return this;
  }

  /**
   * A URI to a JSON-Schema file that defines additional attributes and relationships
   * @return atSchemaLocation
   */
  
  @Schema(name = "@schemaLocation", description = "A URI to a JSON-Schema file that defines additional attributes and relationships", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("@schemaLocation")
  public @Nullable String getAtSchemaLocation() {
    return atSchemaLocation;
  }

  public void setAtSchemaLocation(@Nullable String atSchemaLocation) {
    this.atSchemaLocation = atSchemaLocation;
  }

  public Extensible atBaseType(@Nullable String atBaseType) {
    this.atBaseType = atBaseType;
    return this;
  }

  /**
   * When sub-classing, this defines the super-class
   * @return atBaseType
   */
  
  @Schema(name = "@baseType", description = "When sub-classing, this defines the super-class", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("@baseType")
  public @Nullable String getAtBaseType() {
    return atBaseType;
  }

  public void setAtBaseType(@Nullable String atBaseType) {
    this.atBaseType = atBaseType;
  }

  public Extensible atType(String atType) {
    this.atType = atType;
    return this;
  }

  /**
   * When sub-classing, this defines the sub-class Extensible name
   * @return atType
   */
  @NotNull 
  @Schema(name = "@type", description = "When sub-classing, this defines the sub-class Extensible name", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("@type")
  public String getAtType() {
    return atType;
  }

  public void setAtType(String atType) {
    this.atType = atType;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Extensible extensible = (Extensible) o;
    return Objects.equals(this.atSchemaLocation, extensible.atSchemaLocation) &&
        Objects.equals(this.atBaseType, extensible.atBaseType) &&
        Objects.equals(this.atType, extensible.atType);
  }

  @Override
  public int hashCode() {
    return Objects.hash(atSchemaLocation, atBaseType, atType);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Extensible {\n");
    sb.append("    atSchemaLocation: ").append(toIndentedString(atSchemaLocation)).append("\n");
    sb.append("    atBaseType: ").append(toIndentedString(atBaseType)).append("\n");
    sb.append("    atType: ").append(toIndentedString(atType)).append("\n");
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

    private Extensible instance;

    public Builder() {
      this(new Extensible());
    }

    protected Builder(Extensible instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Extensible value) { 
      this.instance.setAtSchemaLocation(value.atSchemaLocation);
      this.instance.setAtBaseType(value.atBaseType);
      this.instance.setAtType(value.atType);
      return this;
    }

    public Extensible.Builder atSchemaLocation(String atSchemaLocation) {
      this.instance.atSchemaLocation(atSchemaLocation);
      return this;
    }
    
    public Extensible.Builder atBaseType(String atBaseType) {
      this.instance.atBaseType(atBaseType);
      return this;
    }
    
    public Extensible.Builder atType(String atType) {
      this.instance.atType(atType);
      return this;
    }
    
    /**
    * returns a built Extensible instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Extensible build() {
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
  public static Extensible.Builder builder() {
    return new Extensible.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Extensible.Builder toBuilder() {
    Extensible.Builder builder = new Extensible.Builder();
    return builder.copyOf(this);
  }

}

