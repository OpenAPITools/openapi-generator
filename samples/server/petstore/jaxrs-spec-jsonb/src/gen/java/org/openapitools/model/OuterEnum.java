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
 * Gets or Sets OuterEnum
 */
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
@JsonbTypeSerializer(OuterEnum.Serializer.class)
@JsonbTypeDeserializer(OuterEnum.Deserializer.class)
public enum OuterEnum {
  
  PLACED("placed"),
  
  APPROVED("approved"),
  
  DELIVERED("delivered");

  private String value;

  OuterEnum(String value) {
    this.value = value;
  }

    /**
     * Convert a String into String, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
    public static OuterEnum fromString(String s) {
      for (OuterEnum b : OuterEnum.values()) {
        // using Objects.toString() to be safe if value type non-object type
        // because types like 'int' etc. will be auto-boxed
        if (java.util.Objects.toString(b.value).equals(s)) {
          return b;
        }
      }
      return null;
    }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

  public static OuterEnum fromValue(String value) {
    for (OuterEnum b : OuterEnum.values()) {
      if (b.value.equals(value)) {
        return b;
      }
    }
    return null;
  }

  public static final class Serializer implements JsonbSerializer<OuterEnum> {
    @Override
    public void serialize(OuterEnum obj, JsonGenerator generator, SerializationContext ctx) {
      ctx.serialize(obj.value, generator);
    }
  }

  public static final class Deserializer implements JsonbDeserializer<OuterEnum> {
    @Override
    public OuterEnum deserialize(JsonParser parser, DeserializationContext ctx, java.lang.reflect.Type rtType) {
      if (parser.getValue().getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
        return null;
      }
      return fromValue(ctx.deserialize(String.class, parser));
    }
  }
}


