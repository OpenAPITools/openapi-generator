package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.Foo;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;



@JsonTypeName("_foo_get_default_response")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "FooGetDefaultResponse", propOrder =
    { "string"
    })
    
    @XmlRootElement(name="FooGetDefaultResponse")

public class FooGetDefaultResponse  implements Serializable {
  private Foo string;

  protected FooGetDefaultResponse(FooGetDefaultResponseBuilder<?, ?> b) {
    this.string = b.string;
  }

  public FooGetDefaultResponse() {
  }

  /**
   **/
  public FooGetDefaultResponse string(Foo string) {
    this.string = string;
    return this;
  }

      @XmlElement(name="string")
  
  @ApiModelProperty(value = "")
  @JsonProperty("string")
  @Valid public Foo getString() {
    return string;
  }

  @JsonProperty("string")
  public void setString(Foo string) {
    this.string = string;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FooGetDefaultResponse fooGetDefaultResponse = (FooGetDefaultResponse) o;
    return Objects.equals(this.string, fooGetDefaultResponse.string);
  }

  @Override
  public int hashCode() {
    return Objects.hash(string);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FooGetDefaultResponse {\n");
    
    sb.append("    string: ").append(toIndentedString(string)).append("\n");
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


  public static FooGetDefaultResponseBuilder<?, ?> builder() {
    return new FooGetDefaultResponseBuilderImpl();
  }

  private static final class FooGetDefaultResponseBuilderImpl extends FooGetDefaultResponseBuilder<FooGetDefaultResponse, FooGetDefaultResponseBuilderImpl> {

    @Override
    protected FooGetDefaultResponseBuilderImpl self() {
      return this;
    }

    @Override
    public FooGetDefaultResponse build() {
      return new FooGetDefaultResponse(this);
    }
  }

  public static abstract class FooGetDefaultResponseBuilder<C extends FooGetDefaultResponse, B extends FooGetDefaultResponseBuilder<C, B>>  {
    private Foo string;
    protected abstract B self();

    public abstract C build();

    public B string(Foo string) {
      this.string = string;
      return self();
    }
  }
}

