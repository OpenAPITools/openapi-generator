package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.fasterxml.jackson.annotation.JsonTypeName;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * ResponseObjectWithDifferentFieldNamesDto
 */

@JsonPropertyOrder({
    ResponseObjectWithDifferentFieldNamesDto.JSON_PROPERTY_NORMAL_PROPERTY_NAME,
    ResponseObjectWithDifferentFieldNamesDto.JSON_PROPERTY_U_P_P_E_R_C_A_S_E_P_R_O_P_E_R_T_Y_S_N_A_K_E,
    ResponseObjectWithDifferentFieldNamesDto.JSON_PROPERTY_LOWER_CASE_PROPERTY_DASHES,
    ResponseObjectWithDifferentFieldNamesDto.JSON_PROPERTY_PROPERTY_NAME_WITH_SPACES
})
@JsonTypeName("ResponseObjectWithDifferentFieldNames")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class ResponseObjectWithDifferentFieldNamesDto {

    public static final String JSON_PROPERTY_NORMAL_PROPERTY_NAME = "normalPropertyName";
  private @Nullable String normalPropertyName;

    public static final String JSON_PROPERTY_U_P_P_E_R_C_A_S_E_P_R_O_P_E_R_T_Y_S_N_A_K_E = "UPPER_CASE_PROPERTY_SNAKE";
  private @Nullable String UPPER_CASE_PROPERTY_SNAKE;

    public static final String JSON_PROPERTY_LOWER_CASE_PROPERTY_DASHES = "lower-case-property-dashes";
  private @Nullable String lowerCasePropertyDashes;

    public static final String JSON_PROPERTY_PROPERTY_NAME_WITH_SPACES = "property name with spaces";
  private @Nullable String propertyNameWithSpaces;

  public ResponseObjectWithDifferentFieldNamesDto normalPropertyName(@Nullable String normalPropertyName) {
    this.normalPropertyName = normalPropertyName;
    return this;
  }

  /**
   * Get normalPropertyName
   * @return normalPropertyName
   */
  
  @JsonProperty("normalPropertyName")
  public @Nullable String getNormalPropertyName() {
    return normalPropertyName;
  }

  @JsonProperty("normalPropertyName")
  public void setNormalPropertyName(@Nullable String normalPropertyName) {
    this.normalPropertyName = normalPropertyName;
  }

  public ResponseObjectWithDifferentFieldNamesDto UPPER_CASE_PROPERTY_SNAKE(@Nullable String UPPER_CASE_PROPERTY_SNAKE) {
    this.UPPER_CASE_PROPERTY_SNAKE = UPPER_CASE_PROPERTY_SNAKE;
    return this;
  }

  /**
   * Get UPPER_CASE_PROPERTY_SNAKE
   * @return UPPER_CASE_PROPERTY_SNAKE
   */
  
  @JsonProperty("UPPER_CASE_PROPERTY_SNAKE")
  public @Nullable String getUPPERCASEPROPERTYSNAKE() {
    return UPPER_CASE_PROPERTY_SNAKE;
  }

  @JsonProperty("UPPER_CASE_PROPERTY_SNAKE")
  public void setUPPERCASEPROPERTYSNAKE(@Nullable String UPPER_CASE_PROPERTY_SNAKE) {
    this.UPPER_CASE_PROPERTY_SNAKE = UPPER_CASE_PROPERTY_SNAKE;
  }

  public ResponseObjectWithDifferentFieldNamesDto lowerCasePropertyDashes(@Nullable String lowerCasePropertyDashes) {
    this.lowerCasePropertyDashes = lowerCasePropertyDashes;
    return this;
  }

  /**
   * Get lowerCasePropertyDashes
   * @return lowerCasePropertyDashes
   */
  
  @JsonProperty("lower-case-property-dashes")
  public @Nullable String getLowerCasePropertyDashes() {
    return lowerCasePropertyDashes;
  }

  @JsonProperty("lower-case-property-dashes")
  public void setLowerCasePropertyDashes(@Nullable String lowerCasePropertyDashes) {
    this.lowerCasePropertyDashes = lowerCasePropertyDashes;
  }

  public ResponseObjectWithDifferentFieldNamesDto propertyNameWithSpaces(@Nullable String propertyNameWithSpaces) {
    this.propertyNameWithSpaces = propertyNameWithSpaces;
    return this;
  }

  /**
   * Get propertyNameWithSpaces
   * @return propertyNameWithSpaces
   */
  
  @JsonProperty("property name with spaces")
  public @Nullable String getPropertyNameWithSpaces() {
    return propertyNameWithSpaces;
  }

  @JsonProperty("property name with spaces")
  public void setPropertyNameWithSpaces(@Nullable String propertyNameWithSpaces) {
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
  private String toIndentedString(@Nullable Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
}

