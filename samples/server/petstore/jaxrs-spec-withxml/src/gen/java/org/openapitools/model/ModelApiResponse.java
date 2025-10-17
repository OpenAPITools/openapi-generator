package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
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

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;



@JsonTypeName("ApiResponse")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "ModelApiResponse", propOrder =
    { "code", "type", "message"
    })
    
    @XmlRootElement(name="ModelApiResponse")

public class ModelApiResponse  implements Serializable {
  private Integer code;
  private String type;
  private String message;

  protected ModelApiResponse(ModelApiResponseBuilder<?, ?> b) {
    this.code = b.code;
    this.type = b.type;
    this.message = b.message;
  }

  public ModelApiResponse() {
  }

  /**
   **/
  public ModelApiResponse code(Integer code) {
    this.code = code;
    return this;
  }

      @XmlElement(name="code")
  
  @ApiModelProperty(value = "")
  @JsonProperty("code")
  public Integer getCode() {
    return code;
  }

  @JsonProperty("code")
  public void setCode(Integer code) {
    this.code = code;
  }

  /**
   **/
  public ModelApiResponse type(String type) {
    this.type = type;
    return this;
  }

      @XmlElement(name="type")
  
  @ApiModelProperty(value = "")
  @JsonProperty("type")
  public String getType() {
    return type;
  }

  @JsonProperty("type")
  public void setType(String type) {
    this.type = type;
  }

  /**
   **/
  public ModelApiResponse message(String message) {
    this.message = message;
    return this;
  }

      @XmlElement(name="message")
  
  @ApiModelProperty(value = "")
  @JsonProperty("message")
  public String getMessage() {
    return message;
  }

  @JsonProperty("message")
  public void setMessage(String message) {
    this.message = message;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ModelApiResponse _apiResponse = (ModelApiResponse) o;
    return Objects.equals(this.code, _apiResponse.code) &&
        Objects.equals(this.type, _apiResponse.type) &&
        Objects.equals(this.message, _apiResponse.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(code, type, message);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelApiResponse {\n");
    
    sb.append("    code: ").append(toIndentedString(code)).append("\n");
    sb.append("    type: ").append(toIndentedString(type)).append("\n");
    sb.append("    message: ").append(toIndentedString(message)).append("\n");
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


  public static ModelApiResponseBuilder<?, ?> builder() {
    return new ModelApiResponseBuilderImpl();
  }

  private static final class ModelApiResponseBuilderImpl extends ModelApiResponseBuilder<ModelApiResponse, ModelApiResponseBuilderImpl> {

    @Override
    protected ModelApiResponseBuilderImpl self() {
      return this;
    }

    @Override
    public ModelApiResponse build() {
      return new ModelApiResponse(this);
    }
  }

  public static abstract class ModelApiResponseBuilder<C extends ModelApiResponse, B extends ModelApiResponseBuilder<C, B>>  {
    private Integer code;
    private String type;
    private String message;
    protected abstract B self();

    public abstract C build();

    public B code(Integer code) {
      this.code = code;
      return self();
    }
    public B type(String type) {
      this.type = type;
      return self();
    }
    public B message(String message) {
      this.message = message;
      return self();
    }
  }
}

