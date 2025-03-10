package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.AdditionalPropertiesClass;
import org.openapitools.model.AdditionalPropertiesClassRef;
import org.openapitools.model.MixedPropertiesAndAdditionalPropertiesClass;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * AdditionalProperties
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.13.0-SNAPSHOT")
public class AdditionalProperties {

  private @Nullable AdditionalPropertiesClassRef additionalPropertiesClassRef;

  private @Nullable AdditionalPropertiesClass additionalPropertiesClass;

  private @Nullable MixedPropertiesAndAdditionalPropertiesClass mixed;

  public AdditionalProperties additionalPropertiesClassRef(AdditionalPropertiesClassRef additionalPropertiesClassRef) {
    this.additionalPropertiesClassRef = additionalPropertiesClassRef;
    return this;
  }

  /**
   * Get additionalPropertiesClassRef
   * @return additionalPropertiesClassRef
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("additionalPropertiesClassRef")
  public AdditionalPropertiesClassRef getAdditionalPropertiesClassRef() {
    return additionalPropertiesClassRef;
  }

  public void setAdditionalPropertiesClassRef(AdditionalPropertiesClassRef additionalPropertiesClassRef) {
    this.additionalPropertiesClassRef = additionalPropertiesClassRef;
  }

  public AdditionalProperties additionalPropertiesClass(AdditionalPropertiesClass additionalPropertiesClass) {
    this.additionalPropertiesClass = additionalPropertiesClass;
    return this;
  }

  /**
   * Get additionalPropertiesClass
   * @return additionalPropertiesClass
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("additionalPropertiesClass")
  public AdditionalPropertiesClass getAdditionalPropertiesClass() {
    return additionalPropertiesClass;
  }

  public void setAdditionalPropertiesClass(AdditionalPropertiesClass additionalPropertiesClass) {
    this.additionalPropertiesClass = additionalPropertiesClass;
  }

  public AdditionalProperties mixed(MixedPropertiesAndAdditionalPropertiesClass mixed) {
    this.mixed = mixed;
    return this;
  }

  /**
   * Get mixed
   * @return mixed
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("mixed")
  public MixedPropertiesAndAdditionalPropertiesClass getMixed() {
    return mixed;
  }

  public void setMixed(MixedPropertiesAndAdditionalPropertiesClass mixed) {
    this.mixed = mixed;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    AdditionalProperties additionalProperties = (AdditionalProperties) o;
    return Objects.equals(this.additionalPropertiesClassRef, additionalProperties.additionalPropertiesClassRef) &&
        Objects.equals(this.additionalPropertiesClass, additionalProperties.additionalPropertiesClass) &&
        Objects.equals(this.mixed, additionalProperties.mixed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(additionalPropertiesClassRef, additionalPropertiesClass, mixed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalProperties {\n");
    sb.append("    additionalPropertiesClassRef: ").append(toIndentedString(additionalPropertiesClassRef)).append("\n");
    sb.append("    additionalPropertiesClass: ").append(toIndentedString(additionalPropertiesClass)).append("\n");
    sb.append("    mixed: ").append(toIndentedString(mixed)).append("\n");
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

