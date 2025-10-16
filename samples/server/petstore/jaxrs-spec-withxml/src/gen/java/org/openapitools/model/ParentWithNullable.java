package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
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

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "type", visible = true)
@JsonSubTypes({
  @JsonSubTypes.Type(value = ChildWithNullable.class, name = "ChildWithNullable"),
})


@JsonTypeName("ParentWithNullable")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "ParentWithNullable", propOrder =
    { "type", "nullableProperty"
    })
    
    @XmlRootElement(name="ParentWithNullable")

public class ParentWithNullable  implements Serializable {
  @XmlType(name="TypeEnum")
@XmlEnum(String.class)
public enum TypeEnum {

    @XmlEnumValue("ChildWithNullable")CHILD_WITH_NULLABLE(String.valueOf("ChildWithNullable"));


    private String value;

    TypeEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    /**
     * Convert a String into String, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
    public static TypeEnum fromString(String s) {
        for (TypeEnum b : TypeEnum.values()) {
            // using Objects.toString() to be safe if value type non-object type
            // because types like 'int' etc. will be auto-boxed
            if (java.util.Objects.toString(b.value).equals(s)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected string value '" + s + "'");
    }

    @JsonCreator
    public static TypeEnum fromValue(String value) {
        for (TypeEnum b : TypeEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  private TypeEnum type;
  private String nullableProperty;

  protected ParentWithNullable(ParentWithNullableBuilder<?, ?> b) {
    this.type = b.type;
    this.nullableProperty = b.nullableProperty;
  }

  public ParentWithNullable() {
  }

  /**
   **/
  public ParentWithNullable type(TypeEnum type) {
    this.type = type;
    return this;
  }

      @XmlElement(name="type")
  
  @ApiModelProperty(value = "")
  @JsonProperty("type")
  public TypeEnum getType() {
    return type;
  }

  @JsonProperty("type")
  public void setType(TypeEnum type) {
    this.type = type;
  }

  /**
   **/
  public ParentWithNullable nullableProperty(String nullableProperty) {
    this.nullableProperty = nullableProperty;
    return this;
  }

      @XmlElement(name="nullableProperty")
  
  @ApiModelProperty(value = "")
  @JsonProperty("nullableProperty")
  public String getNullableProperty() {
    return nullableProperty;
  }

  @JsonProperty("nullableProperty")
  public void setNullableProperty(String nullableProperty) {
    this.nullableProperty = nullableProperty;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ParentWithNullable parentWithNullable = (ParentWithNullable) o;
    return Objects.equals(this.type, parentWithNullable.type) &&
        Objects.equals(this.nullableProperty, parentWithNullable.nullableProperty);
  }

  @Override
  public int hashCode() {
    return Objects.hash(type, nullableProperty);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ParentWithNullable {\n");
    
    sb.append("    type: ").append(toIndentedString(type)).append("\n");
    sb.append("    nullableProperty: ").append(toIndentedString(nullableProperty)).append("\n");
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


  public static ParentWithNullableBuilder<?, ?> builder() {
    return new ParentWithNullableBuilderImpl();
  }

  private static final class ParentWithNullableBuilderImpl extends ParentWithNullableBuilder<ParentWithNullable, ParentWithNullableBuilderImpl> {

    @Override
    protected ParentWithNullableBuilderImpl self() {
      return this;
    }

    @Override
    public ParentWithNullable build() {
      return new ParentWithNullable(this);
    }
  }

  public static abstract class ParentWithNullableBuilder<C extends ParentWithNullable, B extends ParentWithNullableBuilder<C, B>>  {
    private TypeEnum type;
    private String nullableProperty;
    protected abstract B self();

    public abstract C build();

    public B type(TypeEnum type) {
      this.type = type;
      return self();
    }
    public B nullableProperty(String nullableProperty) {
      this.nullableProperty = nullableProperty;
      return self();
    }
  }
}

