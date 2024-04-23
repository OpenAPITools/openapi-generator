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
 * ClientDto
 */

@JsonTypeName("Client")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class ClientDto {

  private String client;

  public ClientDto client(String client) {
    this.client = client;
    return this;
  }

  /**
   * Get client
   * @return client
  */
  
  @ApiModelProperty(value = "")
  @JsonProperty("client")
  public String getClient() {
    return client;
  }

  public void setClient(String client) {
    this.client = client;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ClientDto client = (ClientDto) o;
    return Objects.equals(this.client, client.client);
  }

  @Override
  public int hashCode() {
    return Objects.hash(client);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ClientDto {\n");
    sb.append("    client: ").append(toIndentedString(client)).append("\n");
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

    private ClientDto instance;

    public Builder() {
      this(new ClientDto());
    }

    protected Builder(ClientDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ClientDto value) { 
      this.instance.setClient(value.client);
      return this;
    }

    public ClientDto.Builder client(String client) {
      this.instance.client(client);
      return this;
    }
    
    /**
    * returns a built ClientDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ClientDto build() {
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
  public static ClientDto.Builder builder() {
    return new ClientDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ClientDto.Builder toBuilder() {
    ClientDto.Builder builder = new ClientDto.Builder();
    return builder.copyOf(this);
  }

}

