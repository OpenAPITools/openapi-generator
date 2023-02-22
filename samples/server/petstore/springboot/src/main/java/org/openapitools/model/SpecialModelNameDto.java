package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * SpecialModelNameDto
 */

@JsonTypeName("$special[model.name]")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class SpecialModelNameDto {

  @JsonProperty("$special[property.name]")
  private Long $SpecialPropertyName;

  public SpecialModelNameDto $SpecialPropertyName(Long $SpecialPropertyName) {
    this.$SpecialPropertyName = $SpecialPropertyName;
    return this;
  }

  /**
   * Get $SpecialPropertyName
   * @return $SpecialPropertyName
  */
  
  @ApiModelProperty(value = "")
  public Long get$SpecialPropertyName() {
    return $SpecialPropertyName;
  }

  public void set$SpecialPropertyName(Long $SpecialPropertyName) {
    this.$SpecialPropertyName = $SpecialPropertyName;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SpecialModelNameDto $SpecialModelName = (SpecialModelNameDto) o;
    return Objects.equals(this.$SpecialPropertyName, $SpecialModelName.$SpecialPropertyName);
  }

  @Override
  public int hashCode() {
    return Objects.hash($SpecialPropertyName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelNameDto {\n");
    sb.append("    $SpecialPropertyName: ").append(toIndentedString($SpecialPropertyName)).append("\n");
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

