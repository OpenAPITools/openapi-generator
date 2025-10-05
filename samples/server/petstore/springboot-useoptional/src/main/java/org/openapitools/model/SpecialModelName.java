package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * SpecialModelName
 */

@JsonTypeName("_special_model.name_")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class SpecialModelName {

  private Optional<Long> $specialPropertyName = Optional.empty();

  public SpecialModelName $specialPropertyName(Long $specialPropertyName) {
    this.$specialPropertyName = Optional.ofNullable($specialPropertyName);
    return this;
  }

  /**
   * Get $specialPropertyName
   * @return $specialPropertyName
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("$special[property.name]")
  public Optional<Long> get$SpecialPropertyName() {
    return $specialPropertyName;
  }

  public void set$SpecialPropertyName(Optional<Long> $specialPropertyName) {
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
    SpecialModelName specialModelName = (SpecialModelName) o;
    return Objects.equals(this.$specialPropertyName, specialModelName.$specialPropertyName);
  }

  @Override
  public int hashCode() {
    return Objects.hash($specialPropertyName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
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
  
  public static class Builder {

    private SpecialModelName instance;

    public Builder() {
      this(new SpecialModelName());
    }

    protected Builder(SpecialModelName instance) {
      this.instance = instance;
    }

    protected Builder copyOf(SpecialModelName value) { 
      this.instance.set$SpecialPropertyName(value.$specialPropertyName);
      return this;
    }

    public SpecialModelName.Builder $specialPropertyName(Long $specialPropertyName) {
      this.instance.$specialPropertyName($specialPropertyName);
      return this;
    }
    
    /**
    * returns a built SpecialModelName instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public SpecialModelName build() {
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
  public static SpecialModelName.Builder builder() {
    return new SpecialModelName.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public SpecialModelName.Builder toBuilder() {
    SpecialModelName.Builder builder = new SpecialModelName.Builder();
    return builder.copyOf(this);
  }

}

