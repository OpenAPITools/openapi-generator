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
 * Water
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class Water implements Beverage {

  private String beverageType;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Boolean sparkling;

  public Water() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Water(String beverageType) {
    this.beverageType = beverageType;
  }

  public Water beverageType(String beverageType) {
    this.beverageType = beverageType;
    return this;
  }

  /**
   * Get beverageType
   * @return beverageType
   */
  @NotNull 
  @Schema(name = "beverageType", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("beverageType")
  public String getBeverageType() {
    return beverageType;
  }

  @JsonProperty("beverageType")
  public void setBeverageType(String beverageType) {
    this.beverageType = beverageType;
  }

  public Water sparkling(@Nullable Boolean sparkling) {
    this.sparkling = sparkling;
    return this;
  }

  /**
   * Get sparkling
   * @return sparkling
   */
  
  @Schema(name = "sparkling", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("sparkling")
  public @Nullable Boolean getSparkling() {
    return sparkling;
  }

  @JsonProperty("sparkling")
  public void setSparkling(@Nullable Boolean sparkling) {
    this.sparkling = sparkling;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Water water = (Water) o;
    return Objects.equals(this.beverageType, water.beverageType) &&
        Objects.equals(this.sparkling, water.sparkling);
  }

  @Override
  public int hashCode() {
    return Objects.hash(beverageType, sparkling);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Water {\n");
    sb.append("    beverageType: ").append(toIndentedString(beverageType)).append("\n");
    sb.append("    sparkling: ").append(toIndentedString(sparkling)).append("\n");
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

