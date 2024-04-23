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

@JsonTypeName("_special_model.name_")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class SpecialModelNameDto {

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
  @JsonProperty("$special[property.name]")
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
    SpecialModelNameDto specialModelName = (SpecialModelNameDto) o;
    return Objects.equals(this.$SpecialPropertyName, specialModelName.$SpecialPropertyName);
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
  
  public static class Builder {

    private SpecialModelNameDto instance;

    public Builder() {
      this(new SpecialModelNameDto());
    }

    protected Builder(SpecialModelNameDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(SpecialModelNameDto value) { 
      this.instance.set$SpecialPropertyName(value.$SpecialPropertyName);
      return this;
    }

    public SpecialModelNameDto.Builder $SpecialPropertyName(Long $SpecialPropertyName) {
      this.instance.$SpecialPropertyName($SpecialPropertyName);
      return this;
    }
    
    /**
    * returns a built SpecialModelNameDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public SpecialModelNameDto build() {
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
  public static SpecialModelNameDto.Builder builder() {
    return new SpecialModelNameDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public SpecialModelNameDto.Builder toBuilder() {
    SpecialModelNameDto.Builder builder = new SpecialModelNameDto.Builder();
    return builder.copyOf(this);
  }

}

