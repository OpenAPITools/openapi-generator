package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * CommonObject
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class CommonObject   {
  @JsonProperty("id")
  private Long id;

  @JsonProperty("stringField")
  private String stringField;

  @JsonProperty("customObject")
  private org.customimport.CustomObject customObject;

  public CommonObject id(Long id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
  */
  @ApiModelProperty(value = "")


  public Long getId() {
    return id;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public CommonObject stringField(String stringField) {
    this.stringField = stringField;
    return this;
  }

  /**
   * Get stringField
   * @return stringField
  */
  @ApiModelProperty(value = "")


  public String getStringField() {
    return stringField;
  }

  public void setStringField(String stringField) {
    this.stringField = stringField;
  }

  public CommonObject customObject(org.customimport.CustomObject customObject) {
    this.customObject = customObject;
    return this;
  }

  /**
   * Get customObject
   * @return customObject
  */
  @ApiModelProperty(value = "")

  @Valid

  public org.customimport.CustomObject getCustomObject() {
    return customObject;
  }

  public void setCustomObject(org.customimport.CustomObject customObject) {
    this.customObject = customObject;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CommonObject commonObject = (CommonObject) o;
    return Objects.equals(this.id, commonObject.id) &&
        Objects.equals(this.stringField, commonObject.stringField) &&
        Objects.equals(this.customObject, commonObject.customObject);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, stringField, customObject);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CommonObject {\n");
    
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    stringField: ").append(toIndentedString(stringField)).append("\n");
    sb.append("    customObject: ").append(toIndentedString(customObject)).append("\n");
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

