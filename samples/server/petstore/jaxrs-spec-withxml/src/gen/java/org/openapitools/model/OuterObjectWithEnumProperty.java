package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.OuterEnumInteger;
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



@JsonTypeName("OuterObjectWithEnumProperty")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "OuterObjectWithEnumProperty", propOrder =
    { "value"
    })
    
    @XmlRootElement(name="OuterObjectWithEnumProperty")

public class OuterObjectWithEnumProperty  implements Serializable {
  private OuterEnumInteger value;

  protected OuterObjectWithEnumProperty(OuterObjectWithEnumPropertyBuilder<?, ?> b) {
    this.value = b.value;
  }

  public OuterObjectWithEnumProperty() {
  }

  @JsonCreator
  public OuterObjectWithEnumProperty(
    @JsonProperty(required = true, value = "value") OuterEnumInteger value
  ) {
    this.value = value;
  }

  /**
   **/
  public OuterObjectWithEnumProperty value(OuterEnumInteger value) {
    this.value = value;
    return this;
  }

      @XmlElement(name="value", required = true)
  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty(required = true, value = "value")
  @NotNull public OuterEnumInteger getValue() {
    return value;
  }

  @JsonProperty(required = true, value = "value")
  public void setValue(OuterEnumInteger value) {
    this.value = value;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OuterObjectWithEnumProperty outerObjectWithEnumProperty = (OuterObjectWithEnumProperty) o;
    return Objects.equals(this.value, outerObjectWithEnumProperty.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(value);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OuterObjectWithEnumProperty {\n");
    
    sb.append("    value: ").append(toIndentedString(value)).append("\n");
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


  public static OuterObjectWithEnumPropertyBuilder<?, ?> builder() {
    return new OuterObjectWithEnumPropertyBuilderImpl();
  }

  private static final class OuterObjectWithEnumPropertyBuilderImpl extends OuterObjectWithEnumPropertyBuilder<OuterObjectWithEnumProperty, OuterObjectWithEnumPropertyBuilderImpl> {

    @Override
    protected OuterObjectWithEnumPropertyBuilderImpl self() {
      return this;
    }

    @Override
    public OuterObjectWithEnumProperty build() {
      return new OuterObjectWithEnumProperty(this);
    }
  }

  public static abstract class OuterObjectWithEnumPropertyBuilder<C extends OuterObjectWithEnumProperty, B extends OuterObjectWithEnumPropertyBuilder<C, B>>  {
    private OuterEnumInteger value;
    protected abstract B self();

    public abstract C build();

    public B value(OuterEnumInteger value) {
      this.value = value;
      return self();
    }
  }
}

