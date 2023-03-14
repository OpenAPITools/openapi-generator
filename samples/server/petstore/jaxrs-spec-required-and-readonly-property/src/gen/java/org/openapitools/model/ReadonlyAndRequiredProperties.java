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



@JsonTypeName("ReadonlyAndRequiredProperties")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class ReadonlyAndRequiredProperties  implements Serializable {
  private @Valid String requiredYesReadonlyYes;
  private @Valid String requiredYesReadonlyNo;
  private @Valid String requiredNoReadonlyYes;
  private @Valid String requiredNoReadonlyNo;

  protected ReadonlyAndRequiredProperties(ReadonlyAndRequiredPropertiesBuilder<?, ?> b) {
    this.requiredYesReadonlyYes = b.requiredYesReadonlyYes;
    this.requiredYesReadonlyNo = b.requiredYesReadonlyNo;
    this.requiredNoReadonlyYes = b.requiredNoReadonlyYes;
    this.requiredNoReadonlyNo = b.requiredNoReadonlyNo;
  }

  public ReadonlyAndRequiredProperties() {
  }

  /**
   **/
  public ReadonlyAndRequiredProperties requiredYesReadonlyYes(String requiredYesReadonlyYes) {
    this.requiredYesReadonlyYes = requiredYesReadonlyYes;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("requiredYesReadonlyYes")
  public String getRequiredYesReadonlyYes() {
    return requiredYesReadonlyYes;
  }

  @JsonProperty("requiredYesReadonlyYes")
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
  @JsonProperty("requiredYesReadonlyNo")
  @NotNull
  public String getRequiredYesReadonlyNo() {
    return requiredYesReadonlyNo;
  }

  @JsonProperty("requiredYesReadonlyNo")
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
        Objects.equals(this.requiredNoReadonlyNo, readonlyAndRequiredProperties.requiredNoReadonlyNo);
  }

  @Override
  public int hashCode() {
    return Objects.hash(requiredYesReadonlyYes, requiredYesReadonlyNo, requiredNoReadonlyYes, requiredNoReadonlyNo);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ReadonlyAndRequiredProperties {\n");
    
    sb.append("    requiredYesReadonlyYes: ").append(toIndentedString(requiredYesReadonlyYes)).append("\n");
    sb.append("    requiredYesReadonlyNo: ").append(toIndentedString(requiredYesReadonlyNo)).append("\n");
    sb.append("    requiredNoReadonlyYes: ").append(toIndentedString(requiredNoReadonlyYes)).append("\n");
    sb.append("    requiredNoReadonlyNo: ").append(toIndentedString(requiredNoReadonlyNo)).append("\n");
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
  }
}

