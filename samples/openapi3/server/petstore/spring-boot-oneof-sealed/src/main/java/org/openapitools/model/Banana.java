package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Banana
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public final class Banana implements Fruit {

  private Integer length;

  private FruitType fruitType;

  public Banana() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Banana(Integer length) {
    this.length = length;
    this.fruitType = fruitType;
  }

  public Banana length(Integer length) {
    this.length = length;
    return this;
  }

  /**
   * Get length
   * @return length
   */
  @NotNull 
  @Schema(name = "length", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("length")
  public Integer getLength() {
    return length;
  }

  public void setLength(Integer length) {
    this.length = length;
  }

  public Banana fruitType(FruitType fruitType) {
    this.fruitType = fruitType;
    return this;
  }

  /**
   * Get fruitType
   * @return fruitType
   */
  @NotNull @Valid 
  @Schema(name = "fruitType", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("fruitType")
  public FruitType getFruitType() {
    return fruitType;
  }

  public void setFruitType(FruitType fruitType) {
    this.fruitType = fruitType;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Banana banana = (Banana) o;
    return Objects.equals(this.length, banana.length) &&
        Objects.equals(this.fruitType, banana.fruitType);
  }

  @Override
  public int hashCode() {
    return Objects.hash(length, fruitType);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Banana {\n");
    sb.append("    length: ").append(toIndentedString(length)).append("\n");
    sb.append("    fruitType: ").append(toIndentedString(fruitType)).append("\n");
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

    private Banana instance;

    public Builder() {
      this(new Banana());
    }

    protected Builder(Banana instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Banana value) { 
      this.instance.setLength(value.length);
      this.instance.setFruitType(value.fruitType);
      return this;
    }

    public Banana.Builder length(Integer length) {
      this.instance.length(length);
      return this;
    }
    
    public Banana.Builder fruitType(FruitType fruitType) {
      this.instance.fruitType(fruitType);
      return this;
    }
    
    /**
    * returns a built Banana instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Banana build() {
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
  public static Banana.Builder builder() {
    return new Banana.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Banana.Builder toBuilder() {
    Banana.Builder builder = new Banana.Builder();
    return builder.copyOf(this);
  }

}

