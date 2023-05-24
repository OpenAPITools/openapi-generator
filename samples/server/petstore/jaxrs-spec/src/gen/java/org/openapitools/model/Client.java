package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("Client")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class Client  implements Serializable {
  private @Valid String client;

  protected Client(ClientBuilder<?, ?> b) {
    this.client = b.client;
  }

  public Client() {
  }

  /**
   **/
  public Client client(String client) {
    this.client = client;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("client")
  public String getClient() {
    return client;
  }

  @JsonProperty("client")
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


  public static ClientBuilder<?, ?> builder() {
    return new ClientBuilderImpl();
  }

  private static final class ClientBuilderImpl extends ClientBuilder<Client, ClientBuilderImpl> {

    @Override
    protected ClientBuilderImpl self() {
      return this;
    }

    @Override
    public Client build() {
      return new Client(this);
    }
  }

  public static abstract class ClientBuilder<C extends Client, B extends ClientBuilder<C, B>>  {
    private String client;
    protected abstract B self();

    public abstract C build();

    public B client(String client) {
      this.client = client;
      return self();
    }
  }
}

