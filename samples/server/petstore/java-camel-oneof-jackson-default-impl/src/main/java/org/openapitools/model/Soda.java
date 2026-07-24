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
 * Soda
 */

@Generated(value = "org.openapitools.codegen.languages.JavaCamelServerCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class Soda implements Beverage {

  private String beverageType;

  private Integer sugarGrams;

  public Soda() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Soda(String beverageType) {
    this.beverageType = beverageType;
  }

  public Soda beverageType(String beverageType) {
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

  public void setBeverageType(String beverageType) {
    this.beverageType = beverageType;
  }

  public Soda sugarGrams(Integer sugarGrams) {
    this.sugarGrams = sugarGrams;
    return this;
  }

  /**
   * Get sugarGrams
   * @return sugarGrams
   */
  
  @Schema(name = "sugarGrams", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("sugarGrams")
  public Integer getSugarGrams() {
    return sugarGrams;
  }

  public void setSugarGrams(Integer sugarGrams) {
    this.sugarGrams = sugarGrams;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Soda soda = (Soda) o;
    return Objects.equals(this.beverageType, soda.beverageType) &&
        Objects.equals(this.sugarGrams, soda.sugarGrams);
  }

  @Override
  public int hashCode() {
    return Objects.hash(beverageType, sugarGrams);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Soda {\n");
    sb.append("    beverageType: ").append(toIndentedString(beverageType)).append("\n");
    sb.append("    sugarGrams: ").append(toIndentedString(sugarGrams)).append("\n");
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

