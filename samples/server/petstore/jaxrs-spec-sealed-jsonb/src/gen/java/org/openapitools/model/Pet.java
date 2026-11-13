package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicDeserializer.class)
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public abstract sealed class Pet  implements Serializable permits Cat, Dog {
  private String petType;
  private String name;

  protected Pet(PetBuilder<?, ?> b) {
    this.petType = b.petType;
    this.name = b.name;
  }

  protected Pet() {
  }

  protected Pet(
    @JsonbProperty("petType") String petType
  ) {
    this.petType = petType;
  }

  /**
   **/
  public Pet petType(String petType) {
    this.petType = petType;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonbProperty("petType")
  @NotNull public String getPetType() {
    return petType;
  }

  @JsonbProperty("petType")
  public void setPetType(String petType) {
    this.petType = petType;
  }

  /**
   **/
  public Pet name(String name) {
    this.name = name;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("name")
  public String getName() {
    return name;
  }

  @JsonbProperty("name")
  public void setName(String name) {
    this.name = name;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Pet pet = (Pet) o;
    return Objects.equals(this.petType, pet.petType) &&
        Objects.equals(this.name, pet.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(petType, name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pet {\n");
    
    sb.append("    petType: ").append(toIndentedString(petType)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
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


  public static abstract class PetBuilder<C extends Pet, B extends PetBuilder<C, B>>  {
    private String petType;
    private String name;
    protected abstract B self();

    public abstract C build();

    public B petType(String petType) {
      this.petType = petType;
      return self();
    }
    public B name(String name) {
      this.name = name;
      return self();
    }
  }

  /**
   * Resolves the concrete class of a JSON object by its discriminator property "petType".
   */
  public static final class JsonbPolymorphicDeserializer implements jakarta.json.bind.serializer.JsonbDeserializer<org.openapitools.model.Pet> {

    private static final jakarta.json.spi.JsonProvider JSON_PROVIDER = jakarta.json.spi.JsonProvider.provider();

    @Override
    public org.openapitools.model.Pet deserialize(jakarta.json.stream.JsonParser parser, jakarta.json.bind.serializer.DeserializationContext ctx, java.lang.reflect.Type rtType) {
      jakarta.json.JsonValue value = parser.getValue();
      if (value.getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
        return null;
      }
      if (value.getValueType() != jakarta.json.JsonValue.ValueType.OBJECT) {
        throw new jakarta.json.bind.JsonbException("Expected a JSON object for Pet but got " + value.getValueType());
      }
      org.openapitools.model.Pet result = fromJsonObject(value.asJsonObject(), ctx);
      // some implementations (Johnzon) also apply this deserializer when a subclass of Pet is requested
      if (rtType instanceof Class && result != null && !((Class<?>) rtType).isInstance(result)) {
        throw new jakarta.json.bind.JsonbException("The discriminator of the JSON object resolves to " + result.getClass().getName()
            + ", which is not a " + ((Class<?>) rtType).getName());
      }
      return result;
    }

    public static org.openapitools.model.Pet fromJsonObject(jakarta.json.JsonObject json, jakarta.json.bind.serializer.DeserializationContext ctx) {
      jakarta.json.JsonValue discriminator = json.get("petType");
      String discriminatorValue = discriminator == null || discriminator.getValueType() == jakarta.json.JsonValue.ValueType.NULL ? null
          : discriminator.getValueType() == jakarta.json.JsonValue.ValueType.STRING ? ((jakarta.json.JsonString) discriminator).getString()
          : discriminator.toString();
      if ("DOG".equals(discriminatorValue)) {
        try (jakarta.json.stream.JsonParser parser = parserOf(json)) {
          return ctx.deserialize(org.openapitools.model.Dog.class, parser);
        }
      }
      if ("PERSIAN".equals(discriminatorValue)) {
        try (jakarta.json.stream.JsonParser parser = parserOf(json)) {
          return ctx.deserialize(org.openapitools.model.PersianCat.class, parser);
        }
      }
      throw new jakarta.json.bind.JsonbException("Unknown or missing discriminator value for 'petType' of Pet: " + discriminatorValue);
    }

    private static jakarta.json.stream.JsonParser parserOf(jakarta.json.JsonObject json) {
      return JSON_PROVIDER.createParser(new java.io.StringReader(json.toString()));
    }
  }

  /**
   * Deserializes a list, set or map property (also a nested one) of Pet elements with the deserializer of
   * Pet. Yasson 3.0.2 to 3.0.4 does not apply the deserializer of a class to the values of a map and the
   * elements of an array or a nested container (Yasson 3.0.2 also to the elements of a list), and caches that binding
   * for the class (fixed in Yasson 3.0.5); it does apply the deserializer of a property. A root container, such as a
   * request body of type {@code Map<String, Pet>}, has no property and is not covered.
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
      return org.openapitools.model.Pet.JsonbPolymorphicDeserializer.fromJsonObject(asStructure(value, jakarta.json.JsonValue.ValueType.OBJECT, type).asJsonObject(), ctx);
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
