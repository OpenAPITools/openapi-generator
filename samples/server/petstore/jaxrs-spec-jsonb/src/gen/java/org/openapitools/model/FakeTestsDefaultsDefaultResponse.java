package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.IntegerEnum;
import org.openapitools.model.StringEnum;
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
public class FakeTestsDefaultsDefaultResponse  implements Serializable {
  private StringEnum stringEnum = StringEnum.FOO;
  private IntegerEnum integerEnum = IntegerEnum.NUMBER_1;
  @JsonbTypeSerializer(StringEnumInlineEnum.Serializer.class)
  @JsonbTypeDeserializer(StringEnumInlineEnum.Deserializer.class)
  public enum StringEnumInlineEnum {

    FOO(String.valueOf("foo")), BAR(String.valueOf("bar")), BAZ(String.valueOf("baz"));


    private String value;

    StringEnumInlineEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
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

    public static StringEnumInlineEnum fromValue(String value) {
        for (StringEnumInlineEnum b : StringEnumInlineEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }

    public static final class Serializer implements JsonbSerializer<StringEnumInlineEnum> {
        @Override
        public void serialize(StringEnumInlineEnum obj, JsonGenerator generator, SerializationContext ctx) {
            ctx.serialize(obj.value, generator);
        }
    }

    public static final class Deserializer implements JsonbDeserializer<StringEnumInlineEnum> {
        @Override
        public StringEnumInlineEnum deserialize(JsonParser parser, DeserializationContext ctx, java.lang.reflect.Type rtType) {
            if (parser.getValue().getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
                return null;
            }
            return fromValue(ctx.deserialize(String.class, parser));
        }
    }
}

  private StringEnumInlineEnum stringEnumInline = StringEnumInlineEnum.FOO;
  @JsonbTypeSerializer(IntegerEnumInlineEnum.Serializer.class)
  @JsonbTypeDeserializer(IntegerEnumInlineEnum.Deserializer.class)
  public enum IntegerEnumInlineEnum {

    NUMBER_1(Integer.valueOf(1)), NUMBER_2(Integer.valueOf(2)), NUMBER_3(Integer.valueOf(3));


    private Integer value;

    IntegerEnumInlineEnum (Integer v) {
        value = v;
    }

    public Integer value() {
        return value;
    }

    @Override
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

    public static IntegerEnumInlineEnum fromValue(Integer value) {
        for (IntegerEnumInlineEnum b : IntegerEnumInlineEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }

    public static final class Serializer implements JsonbSerializer<IntegerEnumInlineEnum> {
        @Override
        public void serialize(IntegerEnumInlineEnum obj, JsonGenerator generator, SerializationContext ctx) {
            ctx.serialize(obj.value, generator);
        }
    }

    public static final class Deserializer implements JsonbDeserializer<IntegerEnumInlineEnum> {
        @Override
        public IntegerEnumInlineEnum deserialize(JsonParser parser, DeserializationContext ctx, java.lang.reflect.Type rtType) {
            if (parser.getValue().getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
                return null;
            }
            return fromValue(ctx.deserialize(Integer.class, parser));
        }
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

  
  @ApiModelProperty(value = "")
  @JsonbProperty("stringEnum")
  public StringEnum getStringEnum() {
    return stringEnum;
  }

  @JsonbProperty("stringEnum")
  public void setStringEnum(StringEnum stringEnum) {
    this.stringEnum = stringEnum;
  }

  /**
   **/
  public FakeTestsDefaultsDefaultResponse integerEnum(IntegerEnum integerEnum) {
    this.integerEnum = integerEnum;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("integerEnum")
  public IntegerEnum getIntegerEnum() {
    return integerEnum;
  }

  @JsonbProperty("integerEnum")
  public void setIntegerEnum(IntegerEnum integerEnum) {
    this.integerEnum = integerEnum;
  }

  /**
   **/
  public FakeTestsDefaultsDefaultResponse stringEnumInline(StringEnumInlineEnum stringEnumInline) {
    this.stringEnumInline = stringEnumInline;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("stringEnumInline")
  public StringEnumInlineEnum getStringEnumInline() {
    return stringEnumInline;
  }

  @JsonbProperty("stringEnumInline")
  public void setStringEnumInline(StringEnumInlineEnum stringEnumInline) {
    this.stringEnumInline = stringEnumInline;
  }

  /**
   **/
  public FakeTestsDefaultsDefaultResponse integerEnumInline(IntegerEnumInlineEnum integerEnumInline) {
    this.integerEnumInline = integerEnumInline;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("integerEnumInline")
  public IntegerEnumInlineEnum getIntegerEnumInline() {
    return integerEnumInline;
  }

  @JsonbProperty("integerEnumInline")
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
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
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
