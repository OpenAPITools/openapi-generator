package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.AdditionalPropertiesBoolean;
import org.openapitools.model.AdditionalPropertiesInteger;
import org.openapitools.model.AdditionalPropertiesNumber;
import org.openapitools.model.AdditionalPropertiesString;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * AdditionalPropertiesClassRef
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.13.0-SNAPSHOT")
public class AdditionalPropertiesClassRef {

  private @Nullable AdditionalPropertiesString mapString;

  private @Nullable AdditionalPropertiesNumber mapNumber;

  private @Nullable AdditionalPropertiesInteger mapInteger;

  private @Nullable AdditionalPropertiesBoolean mapBoolean;

  private @Nullable Object anytype1;

  public AdditionalPropertiesClassRef mapString(AdditionalPropertiesString mapString) {
    this.mapString = mapString;
    return this;
  }

  /**
   * Get mapString
   * @return mapString
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map_string")
  public AdditionalPropertiesString getMapString() {
    return mapString;
  }

  public void setMapString(AdditionalPropertiesString mapString) {
    this.mapString = mapString;
  }

  public AdditionalPropertiesClassRef mapNumber(AdditionalPropertiesNumber mapNumber) {
    this.mapNumber = mapNumber;
    return this;
  }

  /**
   * Get mapNumber
   * @return mapNumber
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map_number")
  public AdditionalPropertiesNumber getMapNumber() {
    return mapNumber;
  }

  public void setMapNumber(AdditionalPropertiesNumber mapNumber) {
    this.mapNumber = mapNumber;
  }

  public AdditionalPropertiesClassRef mapInteger(AdditionalPropertiesInteger mapInteger) {
    this.mapInteger = mapInteger;
    return this;
  }

  /**
   * Get mapInteger
   * @return mapInteger
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map_integer")
  public AdditionalPropertiesInteger getMapInteger() {
    return mapInteger;
  }

  public void setMapInteger(AdditionalPropertiesInteger mapInteger) {
    this.mapInteger = mapInteger;
  }

  public AdditionalPropertiesClassRef mapBoolean(AdditionalPropertiesBoolean mapBoolean) {
    this.mapBoolean = mapBoolean;
    return this;
  }

  /**
   * Get mapBoolean
   * @return mapBoolean
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map_boolean")
  public AdditionalPropertiesBoolean getMapBoolean() {
    return mapBoolean;
  }

  public void setMapBoolean(AdditionalPropertiesBoolean mapBoolean) {
    this.mapBoolean = mapBoolean;
  }

  public AdditionalPropertiesClassRef anytype1(Object anytype1) {
    this.anytype1 = anytype1;
    return this;
  }

  /**
   * Get anytype1
   * @return anytype1
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_1")
  public Object getAnytype1() {
    return anytype1;
  }

  public void setAnytype1(Object anytype1) {
    this.anytype1 = anytype1;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    AdditionalPropertiesClassRef additionalPropertiesClassRef = (AdditionalPropertiesClassRef) o;
    return Objects.equals(this.mapString, additionalPropertiesClassRef.mapString) &&
        Objects.equals(this.mapNumber, additionalPropertiesClassRef.mapNumber) &&
        Objects.equals(this.mapInteger, additionalPropertiesClassRef.mapInteger) &&
        Objects.equals(this.mapBoolean, additionalPropertiesClassRef.mapBoolean) &&
        Objects.equals(this.anytype1, additionalPropertiesClassRef.anytype1);
  }

  @Override
  public int hashCode() {
    return Objects.hash(mapString, mapNumber, mapInteger, mapBoolean, anytype1);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertiesClassRef {\n");
    sb.append("    mapString: ").append(toIndentedString(mapString)).append("\n");
    sb.append("    mapNumber: ").append(toIndentedString(mapNumber)).append("\n");
    sb.append("    mapInteger: ").append(toIndentedString(mapInteger)).append("\n");
    sb.append("    mapBoolean: ").append(toIndentedString(mapBoolean)).append("\n");
    sb.append("    anytype1: ").append(toIndentedString(anytype1)).append("\n");
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

