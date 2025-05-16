package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Client
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class Client {

  private Optional<String> client = Optional.empty();

  public Client client(String client) {
    this.client = Optional.ofNullable(client);
    return this;
  }

  /**
   * Get client
   * @return client
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("client")
  public Optional<String> getClient() {
    return client;
  }

  public void setClient(Optional<String> client) {
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
    Client client = (Client) o;
    return Objects.equals(this.client, client.client);
  }

  @Override
  public int hashCode() {
    return Objects.hash(client);
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder {

    private Client instance;

    public Builder() {
      this(new Client());
    }

    protected Builder(Client instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Client value) { 
      this.instance.setClient(value.client);
      return this;
    }

    public Client.Builder client(String client) {
      this.instance.client(client);
      return this;
    }
    
    /**
    * returns a built Client instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Client build() {
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
  public static Client.Builder builder() {
    return new Client.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Client.Builder toBuilder() {
    Client.Builder builder = new Client.Builder();
    return builder.copyOf(this);
  }

}

