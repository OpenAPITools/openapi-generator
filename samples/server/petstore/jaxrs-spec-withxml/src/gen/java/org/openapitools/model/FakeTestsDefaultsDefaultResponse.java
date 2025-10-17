package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.IntegerEnum;
import org.openapitools.model.StringEnum;
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



@JsonTypeName("fake_tests_defaults_default_response")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "FakeTestsDefaultsDefaultResponse", propOrder =
    { "stringEnum", "integerEnum", "stringEnumInline", "integerEnumInline"
    })
    
    @XmlRootElement(name="FakeTestsDefaultsDefaultResponse")

public class FakeTestsDefaultsDefaultResponse  implements Serializable {
  private StringEnum stringEnum = StringEnum.FOO;
  private IntegerEnum integerEnum = IntegerEnum.NUMBER_1;
  @XmlType(name="StringEnumInlineEnum")
@XmlEnum(String.class)
public enum StringEnumInlineEnum {

    @XmlEnumValue("foo")FOO(String.valueOf("foo")), @XmlEnumValue("bar")BAR(String.valueOf("bar")), @XmlEnumValue("baz")BAZ(String.valueOf("baz"));


    private String value;

    StringEnumInlineEnum (String v) {
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
    public static StringEnumInlineEnum fromString(String s) {
        for (StringEnumInlineEnum b : StringEnumInlineEnum.values()) {
            // using Objects.toString() to be safe if value type non-object type
            // because types like 'int' etc. will be auto-boxed
            if (java.util.Objects.toString(b.value).equals(s)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected string value '" + s + "'");
    }

    @JsonCreator
    public static StringEnumInlineEnum fromValue(String value) {
        for (StringEnumInlineEnum b : StringEnumInlineEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  private StringEnumInlineEnum stringEnumInline = StringEnumInlineEnum.FOO;
  @XmlType(name="IntegerEnumInlineEnum")
@XmlEnum(Integer.class)
public enum IntegerEnumInlineEnum {

    @XmlEnumValue("1")NUMBER_1(Integer.valueOf(1)), @XmlEnumValue("2")NUMBER_2(Integer.valueOf(2)), @XmlEnumValue("3")NUMBER_3(Integer.valueOf(3));


    private Integer value;

    IntegerEnumInlineEnum (Integer v) {
        value = v;
    }

    public Integer value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    /**
     * Convert a String into Integer, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
    public static IntegerEnumInlineEnum fromString(String s) {
        for (IntegerEnumInlineEnum b : IntegerEnumInlineEnum.values()) {
            // using Objects.toString() to be safe if value type non-object type
            // because types like 'int' etc. will be auto-boxed
            if (java.util.Objects.toString(b.value).equals(s)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected string value '" + s + "'");
    }

    @JsonCreator
    public static IntegerEnumInlineEnum fromValue(Integer value) {
        for (IntegerEnumInlineEnum b : IntegerEnumInlineEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  private IntegerEnumInlineEnum integerEnumInline = IntegerEnumInlineEnum.NUMBER_1;

  protected FakeTestsDefaultsDefaultResponse(FakeTestsDefaultsDefaultResponseBuilder<?, ?> b) {
    this.stringEnum = b.stringEnum;
    this.integerEnum = b.integerEnum;
    this.stringEnumInline = b.stringEnumInline;
    this.integerEnumInline = b.integerEnumInline;
  }

  public FakeTestsDefaultsDefaultResponse() {
  }

  /**
   **/
  public FakeTestsDefaultsDefaultResponse stringEnum(StringEnum stringEnum) {
    this.stringEnum = stringEnum;
    return this;
  }

      @XmlElement(name="stringEnum")
  
  @ApiModelProperty(value = "")
  @JsonProperty("stringEnum")
  public StringEnum getStringEnum() {
    return stringEnum;
  }

  @JsonProperty("stringEnum")
  public void setStringEnum(StringEnum stringEnum) {
    this.stringEnum = stringEnum;
  }

  /**
   **/
  public FakeTestsDefaultsDefaultResponse integerEnum(IntegerEnum integerEnum) {
    this.integerEnum = integerEnum;
    return this;
  }

      @XmlElement(name="integerEnum")
  
  @ApiModelProperty(value = "")
  @JsonProperty("integerEnum")
  public IntegerEnum getIntegerEnum() {
    return integerEnum;
  }

  @JsonProperty("integerEnum")
  public void setIntegerEnum(IntegerEnum integerEnum) {
    this.integerEnum = integerEnum;
  }

  /**
   **/
  public FakeTestsDefaultsDefaultResponse stringEnumInline(StringEnumInlineEnum stringEnumInline) {
    this.stringEnumInline = stringEnumInline;
    return this;
  }

      @XmlElement(name="stringEnumInline")
  
  @ApiModelProperty(value = "")
  @JsonProperty("stringEnumInline")
  public StringEnumInlineEnum getStringEnumInline() {
    return stringEnumInline;
  }

  @JsonProperty("stringEnumInline")
  public void setStringEnumInline(StringEnumInlineEnum stringEnumInline) {
    this.stringEnumInline = stringEnumInline;
  }

  /**
   **/
  public FakeTestsDefaultsDefaultResponse integerEnumInline(IntegerEnumInlineEnum integerEnumInline) {
    this.integerEnumInline = integerEnumInline;
    return this;
  }

      @XmlElement(name="integerEnumInline")
  
  @ApiModelProperty(value = "")
  @JsonProperty("integerEnumInline")
  public IntegerEnumInlineEnum getIntegerEnumInline() {
    return integerEnumInline;
  }

  @JsonProperty("integerEnumInline")
  public void setIntegerEnumInline(IntegerEnumInlineEnum integerEnumInline) {
    this.integerEnumInline = integerEnumInline;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FakeTestsDefaultsDefaultResponse fakeTestsDefaultsDefaultResponse = (FakeTestsDefaultsDefaultResponse) o;
    return Objects.equals(this.stringEnum, fakeTestsDefaultsDefaultResponse.stringEnum) &&
        Objects.equals(this.integerEnum, fakeTestsDefaultsDefaultResponse.integerEnum) &&
        Objects.equals(this.stringEnumInline, fakeTestsDefaultsDefaultResponse.stringEnumInline) &&
        Objects.equals(this.integerEnumInline, fakeTestsDefaultsDefaultResponse.integerEnumInline);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringEnum, integerEnum, stringEnumInline, integerEnumInline);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FakeTestsDefaultsDefaultResponse {\n");
    
    sb.append("    stringEnum: ").append(toIndentedString(stringEnum)).append("\n");
    sb.append("    integerEnum: ").append(toIndentedString(integerEnum)).append("\n");
    sb.append("    stringEnumInline: ").append(toIndentedString(stringEnumInline)).append("\n");
    sb.append("    integerEnumInline: ").append(toIndentedString(integerEnumInline)).append("\n");
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


  public static FakeTestsDefaultsDefaultResponseBuilder<?, ?> builder() {
    return new FakeTestsDefaultsDefaultResponseBuilderImpl();
  }

  private static final class FakeTestsDefaultsDefaultResponseBuilderImpl extends FakeTestsDefaultsDefaultResponseBuilder<FakeTestsDefaultsDefaultResponse, FakeTestsDefaultsDefaultResponseBuilderImpl> {

    @Override
    protected FakeTestsDefaultsDefaultResponseBuilderImpl self() {
      return this;
    }

    @Override
    public FakeTestsDefaultsDefaultResponse build() {
      return new FakeTestsDefaultsDefaultResponse(this);
    }
  }

  public static abstract class FakeTestsDefaultsDefaultResponseBuilder<C extends FakeTestsDefaultsDefaultResponse, B extends FakeTestsDefaultsDefaultResponseBuilder<C, B>>  {
    private StringEnum stringEnum = StringEnum.FOO;
    private IntegerEnum integerEnum = IntegerEnum.NUMBER_1;
    private StringEnumInlineEnum stringEnumInline = StringEnumInlineEnum.FOO;
    private IntegerEnumInlineEnum integerEnumInline = IntegerEnumInlineEnum.NUMBER_1;
    protected abstract B self();

    public abstract C build();

    public B stringEnum(StringEnum stringEnum) {
      this.stringEnum = stringEnum;
      return self();
    }
    public B integerEnum(IntegerEnum integerEnum) {
      this.integerEnum = integerEnum;
      return self();
    }
    public B stringEnumInline(StringEnumInlineEnum stringEnumInline) {
      this.stringEnumInline = stringEnumInline;
      return self();
    }
    public B integerEnumInline(IntegerEnumInlineEnum integerEnumInline) {
      this.integerEnumInline = integerEnumInline;
      return self();
    }
  }
}

