package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModel;
import org.openapitools.jackson.nullable.JsonNullable;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
  * Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
 **/
@ApiModel(description="Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.")
public class HealthCheckResult  {
  
  @ApiModelProperty(value = "")
  private JsonNullable<String> nullableMessage = JsonNullable.<String>undefined();
 /**
   * Get nullableMessage
   * @return nullableMessage
  **/
  @JsonIgnore
  public String getNullableMessage() {
    if (nullableMessage == null) {
      return null;
    }
    return nullableMessage.orElse(null);
  }

  @JsonProperty("NullableMessage")
  public JsonNullable<String> getNullableMessage_JsonNullable() {
    return nullableMessage;
  }

  public void setNullableMessage(String nullableMessage) {
      this.nullableMessage = JsonNullable.<String>of(nullableMessage);
  }

  @JsonProperty("NullableMessage")
  public void setNullableMessage_JsonNullable(JsonNullable<String> nullableMessage) {
    this.nullableMessage = nullableMessage;
  }

  public HealthCheckResult nullableMessage(String nullableMessage) {
    this.nullableMessage = JsonNullable.<String>of(nullableMessage);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class HealthCheckResult {\n");
    
    sb.append("    nullableMessage: ").append(toIndentedString(nullableMessage)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

