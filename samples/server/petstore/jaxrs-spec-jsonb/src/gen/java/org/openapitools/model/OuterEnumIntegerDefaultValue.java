package org.openapitools.model;

import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import jakarta.json.bind.annotation.JsonbTypeDeserializer;
import jakarta.json.bind.annotation.JsonbTypeSerializer;
import jakarta.json.bind.serializer.DeserializationContext;
import jakarta.json.bind.serializer.JsonbDeserializer;
import jakarta.json.bind.serializer.JsonbSerializer;
import jakarta.json.bind.serializer.SerializationContext;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonParser;

/**
 * Gets or Sets OuterEnumIntegerDefaultValue
 */
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
@JsonbTypeSerializer(OuterEnumIntegerDefaultValue.Serializer.class)
@JsonbTypeDeserializer(OuterEnumIntegerDefaultValue.Deserializer.class)
public enum OuterEnumIntegerDefaultValue {
  
  NUMBER_0(0),
  
  NUMBER_1(1),
  
  NUMBER_2(2);

  private Integer value;

  OuterEnumIntegerDefaultValue(Integer value) {
    this.value = value;
  }

    /**
     * Convert a String into Integer, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
    public static OuterEnumIntegerDefaultValue fromString(String s) {
      for (OuterEnumIntegerDefaultValue b : OuterEnumIntegerDefaultValue.values()) {
        // using Objects.toString() to be safe if value type non-object type
        // because types like 'int' etc. will be auto-boxed
        if (java.util.Objects.toString(b.value).equals(s)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected string value '" + s + "'");
    }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

  public static OuterEnumIntegerDefaultValue fromValue(Integer value) {
    for (OuterEnumIntegerDefaultValue b : OuterEnumIntegerDefaultValue.values()) {
      if (b.value.equals(value)) {
        return b;
      }
    }
    throw new IllegalArgumentException("Unexpected value '" + value + "'");
  }

  public static final class Serializer implements JsonbSerializer<OuterEnumIntegerDefaultValue> {
    @Override
    public void serialize(OuterEnumIntegerDefaultValue obj, JsonGenerator generator, SerializationContext ctx) {
      ctx.serialize(obj.value, generator);
    }
  }

  public static final class Deserializer implements JsonbDeserializer<OuterEnumIntegerDefaultValue> {
    @Override
    public OuterEnumIntegerDefaultValue deserialize(JsonParser parser, DeserializationContext ctx, java.lang.reflect.Type rtType) {
      if (parser.getValue().getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
        return null;
      }
      return fromValue(ctx.deserialize(Integer.class, parser));
    }
  }
}


