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
 * CustomObject
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class CustomObject   {
  @JsonProperty("field1")
  private String field1;

  @JsonProperty("field2")
  private Boolean field2;

  public CustomObject field1(String field1) {
    this.field1 = field1;
    return this;
  }

  /**
   * Get field1
   * @return field1
  */
  @ApiModelProperty(value = "")


  public String getField1() {
    return field1;
  }

  public void setField1(String field1) {
    this.field1 = field1;
  }

  public CustomObject field2(Boolean field2) {
    this.field2 = field2;
    return this;
  }

  /**
   * Get field2
   * @return field2
  */
  @ApiModelProperty(value = "")


  public Boolean getField2() {
    return field2;
  }

  public void setField2(Boolean field2) {
    this.field2 = field2;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CustomObject customObject = (CustomObject) o;
    return Objects.equals(this.field1, customObject.field1) &&
        Objects.equals(this.field2, customObject.field2);
  }

  @Override
  public int hashCode() {
    return Objects.hash(field1, field2);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CustomObject {\n");
    
    sb.append("    field1: ").append(toIndentedString(field1)).append("\n");
    sb.append("    field2: ").append(toIndentedString(field2)).append("\n");
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

