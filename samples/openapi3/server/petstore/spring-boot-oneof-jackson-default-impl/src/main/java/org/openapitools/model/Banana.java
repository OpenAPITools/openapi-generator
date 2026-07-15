package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Banana
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class Banana implements Fruit {

  private String fruitType;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer length;

  public Banana() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Banana(String fruitType) {
    this.fruitType = fruitType;
  }

  public Banana fruitType(String fruitType) {
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

  @JsonProperty("fruitType")
  public void setFruitType(String fruitType) {
    this.fruitType = fruitType;
  }

  public Banana length(@Nullable Integer length) {
    this.length = length;
    return this;
  }

  /**
   * Get length
   * @return length
   */
  
  @Schema(name = "length", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("length")
  public @Nullable Integer getLength() {
    return length;
  }

  @JsonProperty("length")
  public void setLength(@Nullable Integer length) {
    this.length = length;
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
    return Objects.equals(this.fruitType, banana.fruitType) &&
        Objects.equals(this.length, banana.length);
  }

  @Override
  public int hashCode() {
    return Objects.hash(fruitType, length);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Banana {\n");
    sb.append("    fruitType: ").append(toIndentedString(fruitType)).append("\n");
    sb.append("    length: ").append(toIndentedString(length)).append("\n");
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
}

