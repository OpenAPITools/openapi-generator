package org.openapitools.model;

import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class Client  {
  
  @ApiModelProperty(value = "")
  private String client;
 /**
  * Get client
  * @return client
  */
  @JsonProperty("client")
  public String getClient() {
    return client;
  }

  /**
   * Sets the <code>client</code> property.
   */
 public void setClient(String client) {
    this.client = client;
  }

  /**
   * Sets the <code>client</code> property.
   */
  public Client client(String client) {
    this.client = client;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Client {\n");
    
    sb.append("    client: ").append(toIndentedString(client)).append("\n");
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

