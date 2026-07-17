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
import org.openapitools.jackson.nullable.JsonNullable;



@JsonTypeName("ReadonlyAndRequiredProperties")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.24.0-SNAPSHOT")
public class ReadonlyAndRequiredProperties  implements Serializable {
  private String requiredYesReadonlyYes;
  private String requiredYesReadonlyNo;
  private String requiredNoReadonlyYes;
  private String requiredNoReadonlyNo;
  private String requiredEmail;
  private String optionalEmail;

  protected ReadonlyAndRequiredProperties(ReadonlyAndRequiredPropertiesBuilder<?, ?> b) {
    this.requiredYesReadonlyYes = b.requiredYesReadonlyYes;
    this.requiredYesReadonlyNo = b.requiredYesReadonlyNo;
    this.requiredNoReadonlyYes = b.requiredNoReadonlyYes;
    this.requiredNoReadonlyNo = b.requiredNoReadonlyNo;
    this.requiredEmail = b.requiredEmail;
    this.optionalEmail = b.optionalEmail;
  }

  public ReadonlyAndRequiredProperties() {
  }

  @JsonCreator
  public ReadonlyAndRequiredProperties(
    @JsonProperty(required = true, value = "requiredYesReadonlyYes") String requiredYesReadonlyYes,
    @JsonProperty(required = true, value = "requiredYesReadonlyNo") String requiredYesReadonlyNo,
    @JsonProperty(required = true, value = "requiredEmail") String requiredEmail
  ) {
    this.requiredYesReadonlyYes = requiredYesReadonlyYes;
    this.requiredYesReadonlyNo = requiredYesReadonlyNo;
    this.requiredEmail = requiredEmail;
  }

  /**
   **/
  public ReadonlyAndRequiredProperties requiredYesReadonlyYes(String requiredYesReadonlyYes) {
    this.requiredYesReadonlyYes = requiredYesReadonlyYes;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "requiredYesReadonlyYes")
  public String getRequiredYesReadonlyYes() {
    return requiredYesReadonlyYes;
  }

  @JsonProperty(required = true, value = "requiredYesReadonlyYes")
  public void setRequiredYesReadonlyYes(String requiredYesReadonlyYes) {
    this.requiredYesReadonlyYes = requiredYesReadonlyYes;
  }

  /**
   **/
  public ReadonlyAndRequiredProperties requiredYesReadonlyNo(String requiredYesReadonlyNo) {
    this.requiredYesReadonlyNo = requiredYesReadonlyNo;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "requiredYesReadonlyNo")
  @NotNull public String getRequiredYesReadonlyNo() {
    return requiredYesReadonlyNo;
  }

  @JsonProperty(required = true, value = "requiredYesReadonlyNo")
  public void setRequiredYesReadonlyNo(String requiredYesReadonlyNo) {
    this.requiredYesReadonlyNo = requiredYesReadonlyNo;
  }

  /**
   **/
  public ReadonlyAndRequiredProperties requiredNoReadonlyYes(String requiredNoReadonlyYes) {
    this.requiredNoReadonlyYes = requiredNoReadonlyYes;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("requiredNoReadonlyYes")
  public String getRequiredNoReadonlyYes() {
    return requiredNoReadonlyYes;
  }

  @JsonProperty("requiredNoReadonlyYes")
  public void setRequiredNoReadonlyYes(String requiredNoReadonlyYes) {
    this.requiredNoReadonlyYes = requiredNoReadonlyYes;
  }

  /**
   **/
  public ReadonlyAndRequiredProperties requiredNoReadonlyNo(String requiredNoReadonlyNo) {
    this.requiredNoReadonlyNo = requiredNoReadonlyNo;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("requiredNoReadonlyNo")
  public String getRequiredNoReadonlyNo() {
    return requiredNoReadonlyNo;
  }

  @JsonProperty("requiredNoReadonlyNo")
  public void setRequiredNoReadonlyNo(String requiredNoReadonlyNo) {
    this.requiredNoReadonlyNo = requiredNoReadonlyNo;
  }

  /**
   **/
  public ReadonlyAndRequiredProperties requiredEmail(String requiredEmail) {
    this.requiredEmail = requiredEmail;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "requiredEmail")
  @NotNull  @Email public String getRequiredEmail() {
    return requiredEmail;
  }

  @JsonProperty(required = true, value = "requiredEmail")
  public void setRequiredEmail(String requiredEmail) {
    this.requiredEmail = requiredEmail;
  }

  /**
   **/
  public ReadonlyAndRequiredProperties optionalEmail(String optionalEmail) {
    this.optionalEmail = optionalEmail;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("optionalEmail")
   @Email public String getOptionalEmail() {
    return optionalEmail;
  }

  @JsonProperty("optionalEmail")
  public void setOptionalEmail(String optionalEmail) {
    this.optionalEmail = optionalEmail;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ReadonlyAndRequiredProperties readonlyAndRequiredProperties = (ReadonlyAndRequiredProperties) o;
    return Objects.equals(this.requiredYesReadonlyYes, readonlyAndRequiredProperties.requiredYesReadonlyYes) &&
        Objects.equals(this.requiredYesReadonlyNo, readonlyAndRequiredProperties.requiredYesReadonlyNo) &&
        Objects.equals(this.requiredNoReadonlyYes, readonlyAndRequiredProperties.requiredNoReadonlyYes) &&
        Objects.equals(this.requiredNoReadonlyNo, readonlyAndRequiredProperties.requiredNoReadonlyNo) &&
        Objects.equals(this.requiredEmail, readonlyAndRequiredProperties.requiredEmail) &&
        Objects.equals(this.optionalEmail, readonlyAndRequiredProperties.optionalEmail);
  }

  @Override
  public int hashCode() {
    return Objects.hash(requiredYesReadonlyYes, requiredYesReadonlyNo, requiredNoReadonlyYes, requiredNoReadonlyNo, requiredEmail, optionalEmail);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ReadonlyAndRequiredProperties {\n");
    
    sb.append("    requiredYesReadonlyYes: ").append(toIndentedString(requiredYesReadonlyYes)).append("\n");
    sb.append("    requiredYesReadonlyNo: ").append(toIndentedString(requiredYesReadonlyNo)).append("\n");
    sb.append("    requiredNoReadonlyYes: ").append(toIndentedString(requiredNoReadonlyYes)).append("\n");
    sb.append("    requiredNoReadonlyNo: ").append(toIndentedString(requiredNoReadonlyNo)).append("\n");
    sb.append("    requiredEmail: ").append(toIndentedString(requiredEmail)).append("\n");
    sb.append("    optionalEmail: ").append(toIndentedString(optionalEmail)).append("\n");
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


  public static ReadonlyAndRequiredPropertiesBuilder<?, ?> builder() {
    return new ReadonlyAndRequiredPropertiesBuilderImpl();
  }

  private static final class ReadonlyAndRequiredPropertiesBuilderImpl extends ReadonlyAndRequiredPropertiesBuilder<ReadonlyAndRequiredProperties, ReadonlyAndRequiredPropertiesBuilderImpl> {

    @Override
    protected ReadonlyAndRequiredPropertiesBuilderImpl self() {
      return this;
    }

    @Override
    public ReadonlyAndRequiredProperties build() {
      return new ReadonlyAndRequiredProperties(this);
    }
  }

  public static abstract class ReadonlyAndRequiredPropertiesBuilder<C extends ReadonlyAndRequiredProperties, B extends ReadonlyAndRequiredPropertiesBuilder<C, B>>  {
    private String requiredYesReadonlyYes;
    private String requiredYesReadonlyNo;
    private String requiredNoReadonlyYes;
    private String requiredNoReadonlyNo;
    private String requiredEmail;
    private String optionalEmail;
    protected abstract B self();

    public abstract C build();

    public B requiredYesReadonlyYes(String requiredYesReadonlyYes) {
      this.requiredYesReadonlyYes = requiredYesReadonlyYes;
      return self();
    }
    public B requiredYesReadonlyNo(String requiredYesReadonlyNo) {
      this.requiredYesReadonlyNo = requiredYesReadonlyNo;
      return self();
    }
    public B requiredNoReadonlyYes(String requiredNoReadonlyYes) {
      this.requiredNoReadonlyYes = requiredNoReadonlyYes;
      return self();
    }
    public B requiredNoReadonlyNo(String requiredNoReadonlyNo) {
      this.requiredNoReadonlyNo = requiredNoReadonlyNo;
      return self();
    }
    public B requiredEmail(String requiredEmail) {
      this.requiredEmail = requiredEmail;
      return self();
    }
    public B optionalEmail(String optionalEmail) {
      this.optionalEmail = optionalEmail;
      return self();
    }
  }
}
