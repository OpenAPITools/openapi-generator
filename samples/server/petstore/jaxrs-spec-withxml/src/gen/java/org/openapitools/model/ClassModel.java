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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

/**
 * Model for testing model with \&quot;_class\&quot; property
 **/
@ApiModel(description = "Model for testing model with \"_class\" property")
@JsonTypeName("ClassModel")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.18.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "ClassModel", propOrder =
    { "propertyClass"
    })
    
    @XmlRootElement(name="ClassModel")

public class ClassModel  implements Serializable {
  private String propertyClass;

  protected ClassModel(ClassModelBuilder<?, ?> b) {
    this.propertyClass = b.propertyClass;
  }

  public ClassModel() {
  }

  /**
   **/
  public ClassModel propertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
    return this;
  }

      @XmlElement(name="_class")
  
  @ApiModelProperty(value = "")
  @JsonProperty("_class")
  public String getPropertyClass() {
    return propertyClass;
  }

  @JsonProperty("_class")
  public void setPropertyClass(String propertyClass) {
    this.propertyClass = propertyClass;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ClassModel classModel = (ClassModel) o;
    return Objects.equals(this.propertyClass, classModel.propertyClass);
  }

  @Override
  public int hashCode() {
    return Objects.hash(propertyClass);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ClassModel {\n");
    
    sb.append("    propertyClass: ").append(toIndentedString(propertyClass)).append("\n");
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


  public static ClassModelBuilder<?, ?> builder() {
    return new ClassModelBuilderImpl();
  }

  private static final class ClassModelBuilderImpl extends ClassModelBuilder<ClassModel, ClassModelBuilderImpl> {

    @Override
    protected ClassModelBuilderImpl self() {
      return this;
    }

    @Override
    public ClassModel build() {
      return new ClassModel(this);
    }
  }

  public static abstract class ClassModelBuilder<C extends ClassModel, B extends ClassModelBuilder<C, B>>  {
    private String propertyClass;
    protected abstract B self();

    public abstract C build();

    public B propertyClass(String propertyClass) {
      this.propertyClass = propertyClass;
      return self();
    }
  }
}

