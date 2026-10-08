package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.SingleRefType;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;
import jakarta.json.bind.annotation.JsonbTypeDeserializer;
import jakarta.json.bind.annotation.JsonbTypeSerializer;
import jakarta.json.bind.serializer.DeserializationContext;
import jakarta.json.bind.serializer.JsonbDeserializer;
import jakarta.json.bind.serializer.JsonbSerializer;
import jakarta.json.bind.serializer.SerializationContext;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonParser;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class AllOfWithSingleRef  implements Serializable {
  private String username;
  private SingleRefType singleRefType;

  protected AllOfWithSingleRef(AllOfWithSingleRefBuilder<?, ?> b) {
    this.username = b.username;
    this.singleRefType = b.singleRefType;
  }

  public AllOfWithSingleRef() {
  }

  /**
   **/
  public AllOfWithSingleRef username(String username) {
    this.username = username;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("username")
  public String getUsername() {
    return username;
  }

  @JsonbProperty("username")
  public void setUsername(String username) {
    this.username = username;
  }

  /**
   **/
  public AllOfWithSingleRef singleRefType(SingleRefType singleRefType) {
    this.singleRefType = singleRefType;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("SingleRefType")
  public SingleRefType getSingleRefType() {
    return singleRefType;
  }

  @JsonbProperty("SingleRefType")
  public void setSingleRefType(SingleRefType singleRefType) {
    this.singleRefType = singleRefType;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    AllOfWithSingleRef allOfWithSingleRef = (AllOfWithSingleRef) o;
    return Objects.equals(this.username, allOfWithSingleRef.username) &&
        Objects.equals(this.singleRefType, allOfWithSingleRef.singleRefType);
  }

  @Override
  public int hashCode() {
    return Objects.hash(username, singleRefType);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AllOfWithSingleRef {\n");
    
    sb.append("    username: ").append(toIndentedString(username)).append("\n");
    sb.append("    singleRefType: ").append(toIndentedString(singleRefType)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }


  public static AllOfWithSingleRefBuilder<?, ?> builder() {
    return new AllOfWithSingleRefBuilderImpl();
  }

  private static final class AllOfWithSingleRefBuilderImpl extends AllOfWithSingleRefBuilder<AllOfWithSingleRef, AllOfWithSingleRefBuilderImpl> {

    @Override
    protected AllOfWithSingleRefBuilderImpl self() {
      return this;
    }

    @Override
    public AllOfWithSingleRef build() {
      return new AllOfWithSingleRef(this);
    }
  }

  public static abstract class AllOfWithSingleRefBuilder<C extends AllOfWithSingleRef, B extends AllOfWithSingleRefBuilder<C, B>>  {
    private String username;
    private SingleRefType singleRefType;
    protected abstract B self();

    public abstract C build();

    public B username(String username) {
      this.username = username;
      return self();
    }
    public B singleRefType(SingleRefType singleRefType) {
      this.singleRefType = singleRefType;
      return self();
    }
  }
}
