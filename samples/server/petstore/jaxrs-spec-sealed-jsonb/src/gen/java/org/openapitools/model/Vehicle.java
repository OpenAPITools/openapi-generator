package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Vehicle.JsonbPolymorphicDeserializer.class)
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public sealed class Vehicle  implements Serializable permits Car {
  private String kind;
  private Integer wheels;
  private List<@Valid Vehicle> trailers = new ArrayList<>();

  protected Vehicle(VehicleBuilder<?, ?> b) {
    this.kind = b.kind;
    this.wheels = b.wheels;
    this.trailers = b.trailers;
  }

  public Vehicle() {
    this.setKind("VEHICLE");
  }

  @JsonbCreator
  public Vehicle(
    @JsonbProperty("kind") String kind
  ) {
    this.kind = kind;
  }

  /**
   **/
  public Vehicle kind(String kind) {
    this.kind = kind;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonbProperty("kind")
  @NotNull public String getKind() {
    return kind;
  }

  @JsonbProperty("kind")
  public void setKind(String kind) {
    this.kind = kind;
  }

  /**
   **/
  public Vehicle wheels(Integer wheels) {
    this.wheels = wheels;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("wheels")
  public Integer getWheels() {
    return wheels;
  }

  @JsonbProperty("wheels")
  public void setWheels(Integer wheels) {
    this.wheels = wheels;
  }

  /**
   **/
  public Vehicle trailers(List<@Valid Vehicle> trailers) {
    this.trailers = trailers;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("trailers")
  public List<@Valid Vehicle> getTrailers() {
    return trailers;
  }

  @JsonbProperty("trailers")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Vehicle.JsonbPolymorphicContainerDeserializer.class)
  public void setTrailers(List<@Valid Vehicle> trailers) {
    this.trailers = trailers;
  }

  public Vehicle addTrailersItem(Vehicle trailersItem) {
    if (this.trailers == null) {
      this.trailers = new ArrayList<>();
    }

    this.trailers.add(trailersItem);
    return this;
  }

  public Vehicle removeTrailersItem(Vehicle trailersItem) {
    if (trailersItem != null && this.trailers != null) {
      this.trailers.remove(trailersItem);
    }

    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Vehicle vehicle = (Vehicle) o;
    return Objects.equals(this.kind, vehicle.kind) &&
        Objects.equals(this.wheels, vehicle.wheels) &&
        Objects.equals(this.trailers, vehicle.trailers);
  }

  @Override
  public int hashCode() {
    return Objects.hash(kind, wheels, trailers);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Vehicle {\n");
    
    sb.append("    kind: ").append(toIndentedString(kind)).append("\n");
    sb.append("    wheels: ").append(toIndentedString(wheels)).append("\n");
    sb.append("    trailers: ").append(toIndentedString(trailers)).append("\n");
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


  public static VehicleBuilder<?, ?> builder() {
    return new VehicleBuilderImpl();
  }

  private static final class VehicleBuilderImpl extends VehicleBuilder<Vehicle, VehicleBuilderImpl> {

    @Override
    protected VehicleBuilderImpl self() {
      return this;
    }

    @Override
    public Vehicle build() {
      return new Vehicle(this);
    }
  }

  public static abstract class VehicleBuilder<C extends Vehicle, B extends VehicleBuilder<C, B>>  {
    private String kind;
    private Integer wheels;
    private List<Vehicle> trailers = new ArrayList<>();

    {
      // the discriminator values of Vehicle, which the caller may override
      this.kind("VEHICLE");
    }
    protected abstract B self();

    public abstract C build();

    public B kind(String kind) {
      this.kind = kind;
      return self();
    }
    public B wheels(Integer wheels) {
      this.wheels = wheels;
      return self();
    }
    public B trailers(List<Vehicle> trailers) {
      this.trailers = trailers;
      return self();
    }
  }

  /**
   * Resolves the concrete class of a JSON object by its discriminator property "kind".
   */
  public static final class JsonbPolymorphicDeserializer implements jakarta.json.bind.serializer.JsonbDeserializer<org.openapitools.model.Vehicle> {

    private static final jakarta.json.spi.JsonProvider JSON_PROVIDER = jakarta.json.spi.JsonProvider.provider();

    @Override
    public org.openapitools.model.Vehicle deserialize(jakarta.json.stream.JsonParser parser, jakarta.json.bind.serializer.DeserializationContext ctx, java.lang.reflect.Type rtType) {
      jakarta.json.JsonValue value = parser.getValue();
      if (value.getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
        return null;
      }
      if (value.getValueType() != jakarta.json.JsonValue.ValueType.OBJECT) {
        throw new jakarta.json.bind.JsonbException("Expected a JSON object for Vehicle but got " + value.getValueType());
      }
      org.openapitools.model.Vehicle result = fromJsonObject(value.asJsonObject(), ctx);
      // some implementations (Johnzon) also apply this deserializer when a subclass of Vehicle is requested
      if (rtType instanceof Class && result != null && !((Class<?>) rtType).isInstance(result)) {
        throw new jakarta.json.bind.JsonbException("The discriminator of the JSON object resolves to " + result.getClass().getName()
            + ", which is not a " + ((Class<?>) rtType).getName());
      }
      return result;
    }

    public static org.openapitools.model.Vehicle fromJsonObject(jakarta.json.JsonObject json, jakarta.json.bind.serializer.DeserializationContext ctx) {
      jakarta.json.JsonValue discriminator = json.get("kind");
      String discriminatorValue = discriminator == null || discriminator.getValueType() == jakarta.json.JsonValue.ValueType.NULL ? null
          : discriminator.getValueType() == jakarta.json.JsonValue.ValueType.STRING ? ((jakarta.json.JsonString) discriminator).getString()
          : discriminator.toString();
      if ("CAR".equals(discriminatorValue)) {
        try (jakarta.json.stream.JsonParser parser = parserOf(json)) {
          return ctx.deserialize(org.openapitools.model.Car.class, parser);
        }
      }
      if ("VEHICLE".equals(discriminatorValue)) {
        return fromSelf(json, ctx);
      }
      throw new jakarta.json.bind.JsonbException("Unknown or missing discriminator value for 'kind' of Vehicle: " + discriminatorValue);
    }

    private static org.openapitools.model.Vehicle fromSelf(jakarta.json.JsonObject json, jakarta.json.bind.serializer.DeserializationContext ctx) {
      JsonbData data;
      try (jakarta.json.stream.JsonParser parser = parserOf(json)) {
        data = ctx.deserialize(JsonbData.class, parser);
      }
      org.openapitools.model.Vehicle result = new org.openapitools.model.Vehicle();
      result.setKind(data.kind);
      result.setWheels(data.wheels);
      result.setTrailers(data.trailers);
      return result;
    }

    private static jakarta.json.stream.JsonParser parserOf(jakarta.json.JsonObject json) {
      return JSON_PROVIDER.createParser(new java.io.StringReader(json.toString()));
    }

    /**
     * Binding target for an object that maps to Vehicle itself: it is no subtype of Vehicle and has no
     * deserializer, so binding it does not recurse into this deserializer.
     */
    public static final class JsonbData {
      @jakarta.json.bind.annotation.JsonbProperty("kind")
      public String kind;
      @jakarta.json.bind.annotation.JsonbProperty("wheels")
      public Integer wheels;
      @jakarta.json.bind.annotation.JsonbProperty("trailers")
      @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Vehicle.JsonbPolymorphicContainerDeserializer.class)
      public List<Vehicle> trailers = new ArrayList<>();
    }
  }

  /**
   * Deserializes a list, set or map property (also a nested one) of Vehicle elements with the deserializer of
   * Vehicle. Yasson 3.0.2 to 3.0.4 does not apply the deserializer of a class to the values of a map and the
   * elements of an array or a nested container (Yasson 3.0.2 also to the elements of a list), and caches that binding
   * for the class (fixed in Yasson 3.0.5); it does apply the deserializer of a property. A root container, such as a
   * request body of type {@code Map<String, Vehicle>}, has no property and is not covered.
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
      return org.openapitools.model.Vehicle.JsonbPolymorphicDeserializer.fromJsonObject(asStructure(value, jakarta.json.JsonValue.ValueType.OBJECT, type).asJsonObject(), ctx);
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
