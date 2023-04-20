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
 * ResponseObjectWithDifferentFieldNamesDto
 */

@JsonTypeName("ResponseObjectWithDifferentFieldNames")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ResponseObjectWithDifferentFieldNamesDto {

  private String normalPropertyName;

  private String UPPER_CASE_PROPERTY_SNAKE;

  private String lowerCasePropertyDashes;

  private String propertyNameWithSpaces;

  public ResponseObjectWithDifferentFieldNamesDto normalPropertyName(String normalPropertyName) {
    this.normalPropertyName = normalPropertyName;
    return this;
  }

  /**
   * Get normalPropertyName
   * @return normalPropertyName
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("normalPropertyName")
  public String getNormalPropertyName() {
    return normalPropertyName;
  }

  public void setNormalPropertyName(String normalPropertyName) {
    this.normalPropertyName = normalPropertyName;
  }

  public ResponseObjectWithDifferentFieldNamesDto UPPER_CASE_PROPERTY_SNAKE(String UPPER_CASE_PROPERTY_SNAKE) {
    this.UPPER_CASE_PROPERTY_SNAKE = UPPER_CASE_PROPERTY_SNAKE;
    return this;
  }

  /**
   * Get UPPER_CASE_PROPERTY_SNAKE
   * @return UPPER_CASE_PROPERTY_SNAKE
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("UPPER_CASE_PROPERTY_SNAKE")
  public String getUPPERCASEPROPERTYSNAKE() {
    return UPPER_CASE_PROPERTY_SNAKE;
  }

  public void setUPPERCASEPROPERTYSNAKE(String UPPER_CASE_PROPERTY_SNAKE) {
    this.UPPER_CASE_PROPERTY_SNAKE = UPPER_CASE_PROPERTY_SNAKE;
  }

  public ResponseObjectWithDifferentFieldNamesDto lowerCasePropertyDashes(String lowerCasePropertyDashes) {
    this.lowerCasePropertyDashes = lowerCasePropertyDashes;
    return this;
  }

  /**
   * Get lowerCasePropertyDashes
   * @return lowerCasePropertyDashes
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("lower-case-property-dashes")
  public String getLowerCasePropertyDashes() {
    return lowerCasePropertyDashes;
  }

  public void setLowerCasePropertyDashes(String lowerCasePropertyDashes) {
    this.lowerCasePropertyDashes = lowerCasePropertyDashes;
  }

  public ResponseObjectWithDifferentFieldNamesDto propertyNameWithSpaces(String propertyNameWithSpaces) {
    this.propertyNameWithSpaces = propertyNameWithSpaces;
    return this;
  }

  /**
   * Get propertyNameWithSpaces
   * @return propertyNameWithSpaces
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("property name with spaces")
  public String getPropertyNameWithSpaces() {
    return propertyNameWithSpaces;
  }

  public void setPropertyNameWithSpaces(String propertyNameWithSpaces) {
    this.propertyNameWithSpaces = propertyNameWithSpaces;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ResponseObjectWithDifferentFieldNamesDto responseObjectWithDifferentFieldNames = (ResponseObjectWithDifferentFieldNamesDto) o;
    return Objects.equals(this.normalPropertyName, responseObjectWithDifferentFieldNames.normalPropertyName) &&
        Objects.equals(this.UPPER_CASE_PROPERTY_SNAKE, responseObjectWithDifferentFieldNames.UPPER_CASE_PROPERTY_SNAKE) &&
        Objects.equals(this.lowerCasePropertyDashes, responseObjectWithDifferentFieldNames.lowerCasePropertyDashes) &&
        Objects.equals(this.propertyNameWithSpaces, responseObjectWithDifferentFieldNames.propertyNameWithSpaces);
  }

  @Override
  public int hashCode() {
    return Objects.hash(normalPropertyName, UPPER_CASE_PROPERTY_SNAKE, lowerCasePropertyDashes, propertyNameWithSpaces);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ResponseObjectWithDifferentFieldNamesDto {\n");
    sb.append("    normalPropertyName: ").append(toIndentedString(normalPropertyName)).append("\n");
    sb.append("    UPPER_CASE_PROPERTY_SNAKE: ").append(toIndentedString(UPPER_CASE_PROPERTY_SNAKE)).append("\n");
    sb.append("    lowerCasePropertyDashes: ").append(toIndentedString(lowerCasePropertyDashes)).append("\n");
    sb.append("    propertyNameWithSpaces: ").append(toIndentedString(propertyNameWithSpaces)).append("\n");
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

