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



@JsonTypeName("_special_model.name_")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "SpecialModelName", propOrder =
    { "$specialPropertyName"
    })
    
    @XmlRootElement(name="SpecialModelName")

public class SpecialModelName  implements Serializable {
  private Long $specialPropertyName;

  protected SpecialModelName(SpecialModelNameBuilder<?, ?> b) {
    this.$specialPropertyName = b.$specialPropertyName;
  }

  public SpecialModelName() {
  }

  /**
   **/
  public SpecialModelName $specialPropertyName(Long $specialPropertyName) {
    this.$specialPropertyName = $specialPropertyName;
    return this;
  }

      @XmlElement(name="$special[property.name]")
  
  @ApiModelProperty(value = "")
  @JsonProperty("$special[property.name]")
  public Long get$SpecialPropertyName() {
    return $specialPropertyName;
  }

  @JsonProperty("$special[property.name]")
  public void set$SpecialPropertyName(Long $specialPropertyName) {
    this.$specialPropertyName = $specialPropertyName;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SpecialModelName specialModelName = (SpecialModelName) o;
    return Objects.equals(this.$specialPropertyName, specialModelName.$specialPropertyName);
  }

  @Override
  public int hashCode() {
    return Objects.hash($specialPropertyName);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
    
    sb.append("    $specialPropertyName: ").append(toIndentedString($specialPropertyName)).append("\n");
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


  public static SpecialModelNameBuilder<?, ?> builder() {
    return new SpecialModelNameBuilderImpl();
  }

  private static final class SpecialModelNameBuilderImpl extends SpecialModelNameBuilder<SpecialModelName, SpecialModelNameBuilderImpl> {

    @Override
    protected SpecialModelNameBuilderImpl self() {
      return this;
    }

    @Override
    public SpecialModelName build() {
      return new SpecialModelName(this);
    }
  }

  public static abstract class SpecialModelNameBuilder<C extends SpecialModelName, B extends SpecialModelNameBuilder<C, B>>  {
    private Long $specialPropertyName;
    protected abstract B self();

    public abstract C build();

    public B $specialPropertyName(Long $specialPropertyName) {
      this.$specialPropertyName = $specialPropertyName;
      return self();
    }
  }
}

