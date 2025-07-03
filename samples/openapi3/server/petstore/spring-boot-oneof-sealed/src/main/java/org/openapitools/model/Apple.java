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
 * Apple
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public final class Apple implements Fruit {

  private Integer seeds;

  private FruitType fruitType;

  public Apple() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Apple(Integer seeds) {
    this.seeds = seeds;
    this.fruitType = fruitType;
  }

  public Apple seeds(Integer seeds) {
    this.seeds = seeds;
    return this;
  }

  /**
   * Get seeds
   * @return seeds
   */
  @NotNull 
  @Schema(name = "seeds", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("seeds")
  public Integer getSeeds() {
    return seeds;
  }

  public void setSeeds(Integer seeds) {
    this.seeds = seeds;
  }

  public Apple fruitType(FruitType fruitType) {
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
    Apple apple = (Apple) o;
    return Objects.equals(this.seeds, apple.seeds) &&
        Objects.equals(this.fruitType, apple.fruitType);
  }

  @Override
  public int hashCode() {
    return Objects.hash(seeds, fruitType);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Apple {\n");
    sb.append("    seeds: ").append(toIndentedString(seeds)).append("\n");
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

    private Apple instance;

    public Builder() {
      this(new Apple());
    }

    protected Builder(Apple instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Apple value) { 
      this.instance.setSeeds(value.seeds);
      this.instance.setFruitType(value.fruitType);
      return this;
    }

    public Apple.Builder seeds(Integer seeds) {
      this.instance.seeds(seeds);
      return this;
    }
    
    public Apple.Builder fruitType(FruitType fruitType) {
      this.instance.fruitType(fruitType);
      return this;
    }
    
    /**
    * returns a built Apple instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Apple build() {
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
  public static Apple.Builder builder() {
    return new Apple.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Apple.Builder toBuilder() {
    Apple.Builder builder = new Apple.Builder();
    return builder.copyOf(this);
  }

}

