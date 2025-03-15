package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.AdditionalPropertyArrayAnyType;
import org.openapitools.model.AdditionalPropertyArrayInteger;
import org.openapitools.model.AdditionalPropertyBoolean;
import org.openapitools.model.AdditionalPropertyInteger;
import org.openapitools.model.AdditionalPropertyMapAnyType;
import org.openapitools.model.AdditionalPropertyMapString;
import org.openapitools.model.AdditionalPropertyNumber;
import org.openapitools.model.AdditionalPropertyString;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * AdditionalPropertyClassRef
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.13.0-SNAPSHOT")
public class AdditionalPropertyClassRef {

  private @Nullable AdditionalPropertyString mapString;

  private @Nullable AdditionalPropertyNumber mapNumber;

  private @Nullable AdditionalPropertyInteger mapInteger;

  private @Nullable AdditionalPropertyBoolean mapBoolean;

  private @Nullable AdditionalPropertyArrayInteger mapArrayInteger;

  private @Nullable AdditionalPropertyArrayAnyType mapArrayAnytype;

  private @Nullable AdditionalPropertyMapString mapMapString;

  private @Nullable AdditionalPropertyMapAnyType mapMapAnytype;

  private @Nullable Object anytype1;

  public AdditionalPropertyClassRef mapString(AdditionalPropertyString mapString) {
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
  public AdditionalPropertyString getMapString() {
    return mapString;
  }

  public void setMapString(AdditionalPropertyString mapString) {
    this.mapString = mapString;
  }

  public AdditionalPropertyClassRef mapNumber(AdditionalPropertyNumber mapNumber) {
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
  public AdditionalPropertyNumber getMapNumber() {
    return mapNumber;
  }

  public void setMapNumber(AdditionalPropertyNumber mapNumber) {
    this.mapNumber = mapNumber;
  }

  public AdditionalPropertyClassRef mapInteger(AdditionalPropertyInteger mapInteger) {
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
  public AdditionalPropertyInteger getMapInteger() {
    return mapInteger;
  }

  public void setMapInteger(AdditionalPropertyInteger mapInteger) {
    this.mapInteger = mapInteger;
  }

  public AdditionalPropertyClassRef mapBoolean(AdditionalPropertyBoolean mapBoolean) {
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
  public AdditionalPropertyBoolean getMapBoolean() {
    return mapBoolean;
  }

  public void setMapBoolean(AdditionalPropertyBoolean mapBoolean) {
    this.mapBoolean = mapBoolean;
  }

  public AdditionalPropertyClassRef mapArrayInteger(AdditionalPropertyArrayInteger mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
    return this;
  }

  /**
   * Get mapArrayInteger
   * @return mapArrayInteger
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map_array_integer")
  public AdditionalPropertyArrayInteger getMapArrayInteger() {
    return mapArrayInteger;
  }

  public void setMapArrayInteger(AdditionalPropertyArrayInteger mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
  }

  public AdditionalPropertyClassRef mapArrayAnytype(AdditionalPropertyArrayAnyType mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
    return this;
  }

  /**
   * Get mapArrayAnytype
   * @return mapArrayAnytype
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map_array_anytype")
  public AdditionalPropertyArrayAnyType getMapArrayAnytype() {
    return mapArrayAnytype;
  }

  public void setMapArrayAnytype(AdditionalPropertyArrayAnyType mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
  }

  public AdditionalPropertyClassRef mapMapString(AdditionalPropertyMapString mapMapString) {
    this.mapMapString = mapMapString;
    return this;
  }

  /**
   * Get mapMapString
   * @return mapMapString
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map_map_string")
  public AdditionalPropertyMapString getMapMapString() {
    return mapMapString;
  }

  public void setMapMapString(AdditionalPropertyMapString mapMapString) {
    this.mapMapString = mapMapString;
  }

  public AdditionalPropertyClassRef mapMapAnytype(AdditionalPropertyMapAnyType mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
    return this;
  }

  /**
   * Get mapMapAnytype
   * @return mapMapAnytype
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("map_map_anytype")
  public AdditionalPropertyMapAnyType getMapMapAnytype() {
    return mapMapAnytype;
  }

  public void setMapMapAnytype(AdditionalPropertyMapAnyType mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
  }

  public AdditionalPropertyClassRef anytype1(Object anytype1) {
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
    AdditionalPropertyClassRef additionalPropertyClassRef = (AdditionalPropertyClassRef) o;
    return Objects.equals(this.mapString, additionalPropertyClassRef.mapString) &&
        Objects.equals(this.mapNumber, additionalPropertyClassRef.mapNumber) &&
        Objects.equals(this.mapInteger, additionalPropertyClassRef.mapInteger) &&
        Objects.equals(this.mapBoolean, additionalPropertyClassRef.mapBoolean) &&
        Objects.equals(this.mapArrayInteger, additionalPropertyClassRef.mapArrayInteger) &&
        Objects.equals(this.mapArrayAnytype, additionalPropertyClassRef.mapArrayAnytype) &&
        Objects.equals(this.mapMapString, additionalPropertyClassRef.mapMapString) &&
        Objects.equals(this.mapMapAnytype, additionalPropertyClassRef.mapMapAnytype) &&
        Objects.equals(this.anytype1, additionalPropertyClassRef.anytype1);
  }

  @Override
  public int hashCode() {
    return Objects.hash(mapString, mapNumber, mapInteger, mapBoolean, mapArrayInteger, mapArrayAnytype, mapMapString, mapMapAnytype, anytype1);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertyClassRef {\n");
    sb.append("    mapString: ").append(toIndentedString(mapString)).append("\n");
    sb.append("    mapNumber: ").append(toIndentedString(mapNumber)).append("\n");
    sb.append("    mapInteger: ").append(toIndentedString(mapInteger)).append("\n");
    sb.append("    mapBoolean: ").append(toIndentedString(mapBoolean)).append("\n");
    sb.append("    mapArrayInteger: ").append(toIndentedString(mapArrayInteger)).append("\n");
    sb.append("    mapArrayAnytype: ").append(toIndentedString(mapArrayAnytype)).append("\n");
    sb.append("    mapMapString: ").append(toIndentedString(mapMapString)).append("\n");
    sb.append("    mapMapAnytype: ").append(toIndentedString(mapMapAnytype)).append("\n");
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

