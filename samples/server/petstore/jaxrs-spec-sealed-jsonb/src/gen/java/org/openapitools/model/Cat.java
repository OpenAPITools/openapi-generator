package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.Pet;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Cat.JsonbPolymorphicDeserializer.class)
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public abstract sealed class Cat extends Pet implements Serializable permits PersianCat {
  private Boolean indoor;

  protected Cat(CatBuilder<?, ?> b) {
    super(b);
    this.indoor = b.indoor;
  }

  protected Cat() {
  }

  protected Cat(
    @JsonbProperty("petType") String petType
  ) {
    super(
      petType
    );
  }

  /**
   **/
  public Cat indoor(Boolean indoor) {
    this.indoor = indoor;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("indoor")
  public Boolean getIndoor() {
    return indoor;
  }

  @JsonbProperty("indoor")
  public void setIndoor(Boolean indoor) {
    this.indoor = indoor;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Cat cat = (Cat) o;
    return Objects.equals(this.indoor, cat.indoor) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(indoor, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Cat {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    indoor: ").append(toIndentedString(indoor)).append("\n");
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


  public static abstract class CatBuilder<C extends Cat, B extends CatBuilder<C, B>> extends PetBuilder<C, B> {
    private Boolean indoor;

    public B indoor(Boolean indoor) {
      this.indoor = indoor;
      return self();
    }
  }

  /**
   * Resolves the concrete class of a JSON object by its discriminator property "petType".
   */
  public static final class JsonbPolymorphicDeserializer implements jakarta.json.bind.serializer.JsonbDeserializer<org.openapitools.model.Cat> {

    private static final jakarta.json.spi.JsonProvider JSON_PROVIDER = jakarta.json.spi.JsonProvider.provider();

    @Override
    public org.openapitools.model.Cat deserialize(jakarta.json.stream.JsonParser parser, jakarta.json.bind.serializer.DeserializationContext ctx, java.lang.reflect.Type rtType) {
      jakarta.json.JsonValue value = parser.getValue();
      if (value.getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
        return null;
      }
      if (value.getValueType() != jakarta.json.JsonValue.ValueType.OBJECT) {
        throw new jakarta.json.bind.JsonbException("Expected a JSON object for Cat but got " + value.getValueType());
      }
      org.openapitools.model.Cat result = fromJsonObject(value.asJsonObject(), ctx);
      // some implementations (Johnzon) also apply this deserializer when a subclass of Cat is requested
      if (rtType instanceof Class && result != null && !((Class<?>) rtType).isInstance(result)) {
        throw new jakarta.json.bind.JsonbException("The discriminator of the JSON object resolves to " + result.getClass().getName()
            + ", which is not a " + ((Class<?>) rtType).getName());
      }
      return result;
    }

    public static org.openapitools.model.Cat fromJsonObject(jakarta.json.JsonObject json, jakarta.json.bind.serializer.DeserializationContext ctx) {
      jakarta.json.JsonValue discriminator = json.get("petType");
      String discriminatorValue = discriminator == null || discriminator.getValueType() == jakarta.json.JsonValue.ValueType.NULL ? null
          : discriminator.getValueType() == jakarta.json.JsonValue.ValueType.STRING ? ((jakarta.json.JsonString) discriminator).getString()
          : discriminator.toString();
      if ("PERSIAN".equals(discriminatorValue)) {
        try (jakarta.json.stream.JsonParser parser = parserOf(json)) {
          return ctx.deserialize(org.openapitools.model.PersianCat.class, parser);
        }
      }
      throw new jakarta.json.bind.JsonbException("Unknown or missing discriminator value for 'petType' of Cat: " + discriminatorValue);
    }

    private static jakarta.json.stream.JsonParser parserOf(jakarta.json.JsonObject json) {
      return JSON_PROVIDER.createParser(new java.io.StringReader(json.toString()));
    }
  }

  /**
   * Deserializes a list, set or map property (also a nested one) of Cat elements with the deserializer of
   * Cat. Yasson 3.0.2 to 3.0.4 does not apply the deserializer of a class to the values of a map and the
   * elements of an array or a nested container (Yasson 3.0.2 also to the elements of a list), and caches that binding
   * for the class (fixed in Yasson 3.0.5); it does apply the deserializer of a property. A root container, such as a
   * request body of type {@code Map<String, Cat>}, has no property and is not covered.
   * <p>
   * Johnzon 2.0.2 to 2.2.0 fails internally, before calling this deserializer, when a required container (a parameter
   * of a JSON-B creator) is missing in the JSON; an explicit JSON null, a missing optional container and the contents
   * of a present container are deserialized as usual.
   */
  public static final class JsonbPolymorphicContainerDeserializer implements jakarta.json.bind.serializer.JsonbDeserializer<Object> {

    @Override
    public Object deserialize(jakarta.json.stream.JsonParser parser, jakarta.json.bind.serializer.DeserializationContext ctx, java.lang.reflect.Type rtType) {
      return read(parser.getValue(), rtType, ctx);
    }

    private static Object read(jakarta.json.JsonValue value, java.lang.reflect.Type type, jakarta.json.bind.serializer.DeserializationContext ctx) {
      if (value == null || value.getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
        return null;
      }
      Class<?> rawType = type instanceof java.lang.reflect.ParameterizedType
          ? (Class<?>) ((java.lang.reflect.ParameterizedType) type).getRawType() : type instanceof Class ? (Class<?>) type : Object.class;
      if (java.util.Map.class.isAssignableFrom(rawType)) {
        java.lang.reflect.Type valueType = typeArgument(type, 1);
        java.util.Map<String, Object> result = new java.util.LinkedHashMap<>();
        for (java.util.Map.Entry<String, jakarta.json.JsonValue> entry : asStructure(value, jakarta.json.JsonValue.ValueType.OBJECT, type).asJsonObject().entrySet()) {
          result.put(entry.getKey(), read(entry.getValue(), valueType, ctx));
        }
        return result;
      }
      if (java.util.Collection.class.isAssignableFrom(rawType)) {
        java.lang.reflect.Type elementType = typeArgument(type, 0);
        java.util.Collection<Object> result = java.util.Set.class.isAssignableFrom(rawType) ? new java.util.LinkedHashSet<>() : new java.util.ArrayList<>();
        for (jakarta.json.JsonValue element : asStructure(value, jakarta.json.JsonValue.ValueType.ARRAY, type).asJsonArray()) {
          result.add(read(element, elementType, ctx));
        }
        return result;
      }
      return org.openapitools.model.Cat.JsonbPolymorphicDeserializer.fromJsonObject(asStructure(value, jakarta.json.JsonValue.ValueType.OBJECT, type).asJsonObject(), ctx);
    }

    private static jakarta.json.JsonValue asStructure(jakarta.json.JsonValue value, jakarta.json.JsonValue.ValueType expected, java.lang.reflect.Type type) {
      if (value.getValueType() != expected) {
        throw new jakarta.json.bind.JsonbException("Expected a JSON " + expected.name().toLowerCase(java.util.Locale.ROOT) + " for "
            + type.getTypeName() + " but got " + value.getValueType());
      }
      return value;
    }

    private static java.lang.reflect.Type typeArgument(java.lang.reflect.Type type, int index) {
      if (!(type instanceof java.lang.reflect.ParameterizedType)) {
        throw new jakarta.json.bind.JsonbException("The element type of " + type.getTypeName() + " is unknown");
      }
      return ((java.lang.reflect.ParameterizedType) type).getActualTypeArguments()[index];
    }
  }
}
