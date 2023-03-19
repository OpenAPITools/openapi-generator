package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * SpecialModelNameDto
 */

@JsonTypeName("_special_model.name_")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class SpecialModelNameDto {

  private Long $specialPropertyName;

  public SpecialModelNameDto $specialPropertyName(Long $specialPropertyName) {
    this.$specialPropertyName = $specialPropertyName;
    return this;
  }

  /**
   * Get $specialPropertyName
   * @return $specialPropertyName
  */
  
  @JsonProperty("$special[property.name]")
  public Long get$SpecialPropertyName() {
    return $specialPropertyName;
  }

  public void set$SpecialPropertyName(Long $specialPropertyName) {
    this.$specialPropertyName = $specialPropertyName;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SpecialModelNameDto specialModelName = (SpecialModelNameDto) o;
    return Objects.equals(this.$specialPropertyName, specialModelName.$specialPropertyName);
  }

  @Override
  public int hashCode() {
    return Objects.hash($specialPropertyName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelNameDto {\n");
    sb.append("    $specialPropertyName: ").append(toIndentedString($specialPropertyName)).append("\n");
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

