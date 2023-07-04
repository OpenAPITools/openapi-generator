package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
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

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class Apple implements Fruit {

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
}

