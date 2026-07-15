package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Apple
 */

@Generated(value = "org.openapitools.codegen.languages.JavaCamelServerCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class Apple implements Fruit {

  private String fruitType;

  private Integer seeds;

  public Apple() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Apple(String fruitType) {
    this.fruitType = fruitType;
  }

  public Apple fruitType(String fruitType) {
    this.fruitType = fruitType;
    return this;
  }

  /**
   * Get fruitType
   * @return fruitType
   */
  @NotNull 
  @Schema(name = "fruitType", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("fruitType")
  public String getFruitType() {
    return fruitType;
  }

  public void setFruitType(String fruitType) {
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
  
  @Schema(name = "seeds", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("seeds")
  public Integer getSeeds() {
    return seeds;
  }

  public void setSeeds(Integer seeds) {
    this.seeds = seeds;
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
    return Objects.equals(this.fruitType, apple.fruitType) &&
        Objects.equals(this.seeds, apple.seeds);
  }

  @Override
  public int hashCode() {
    return Objects.hash(fruitType, seeds);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Apple {\n");
    sb.append("    fruitType: ").append(toIndentedString(fruitType)).append("\n");
    sb.append("    seeds: ").append(toIndentedString(seeds)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
}

