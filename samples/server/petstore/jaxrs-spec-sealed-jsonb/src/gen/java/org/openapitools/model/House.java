package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.HashMap;
import java.util.Map;
import org.openapitools.model.Pet;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.House.JsonbPolymorphicDeserializer.class)
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public sealed class House  implements Serializable permits Shelter {
  private String houseType;
  private Map<String, @Valid Pet> residents = new HashMap<>();

  protected House(HouseBuilder<?, ?> b) {
    this.houseType = b.houseType;
    this.residents = b.residents;
  }

  public House() {
    this.setHouseType("HOUSE");
  }

  @JsonbCreator
  public House(
    @JsonbProperty("houseType") String houseType,
    @JsonbProperty("residents") @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class) Map<String, @Valid Pet> residents
  ) {
    this.houseType = houseType;
    this.residents = residents;
  }

  /**
   **/
  public House houseType(String houseType) {
    this.houseType = houseType;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonbProperty("houseType")
  @NotNull public String getHouseType() {
    return houseType;
  }

  @JsonbProperty("houseType")
  public void setHouseType(String houseType) {
    this.houseType = houseType;
  }

  /**
   **/
  public House residents(Map<String, @Valid Pet> residents) {
    this.residents = residents;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonbProperty("residents")
  @NotNull public Map<String, @Valid Pet> getResidents() {
    return residents;
  }

  @JsonbProperty("residents")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setResidents(Map<String, @Valid Pet> residents) {
    this.residents = residents;
  }

  public House putResidentsItem(String key, Pet residentsItem) {
    if (this.residents == null) {
      this.residents = new HashMap<>();
    }

    this.residents.put(key, residentsItem);
    return this;
  }

  public House removeResidentsItem(String key) {
    if (this.residents != null) {
      this.residents.remove(key);
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
    House house = (House) o;
    return Objects.equals(this.houseType, house.houseType) &&
        Objects.equals(this.residents, house.residents);
  }

  @Override
  public int hashCode() {
    return Objects.hash(houseType, residents);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class House {\n");
    
    sb.append("    houseType: ").append(toIndentedString(houseType)).append("\n");
    sb.append("    residents: ").append(toIndentedString(residents)).append("\n");
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


  public static HouseBuilder<?, ?> builder() {
    return new HouseBuilderImpl();
  }

  private static final class HouseBuilderImpl extends HouseBuilder<House, HouseBuilderImpl> {

    @Override
    protected HouseBuilderImpl self() {
      return this;
    }

    @Override
    public House build() {
      return new House(this);
    }
  }

  public static abstract class HouseBuilder<C extends House, B extends HouseBuilder<C, B>>  {
    private String houseType;
    private Map<String, Pet> residents = new HashMap<>();

    {
      // the discriminator values of House, which the caller may override
      this.houseType("HOUSE");
    }
    protected abstract B self();

    public abstract C build();

    public B houseType(String houseType) {
      this.houseType = houseType;
      return self();
    }
    public B residents(Map<String, Pet> residents) {
      this.residents = residents;
      return self();
    }
  }

  /**
   * Resolves the concrete class of a JSON object by its discriminator property "houseType".
   */
  public static final class JsonbPolymorphicDeserializer implements jakarta.json.bind.serializer.JsonbDeserializer<org.openapitools.model.House> {

    private static final jakarta.json.spi.JsonProvider JSON_PROVIDER = jakarta.json.spi.JsonProvider.provider();

    @Override
    public org.openapitools.model.House deserialize(jakarta.json.stream.JsonParser parser, jakarta.json.bind.serializer.DeserializationContext ctx, java.lang.reflect.Type rtType) {
      jakarta.json.JsonValue value = parser.getValue();
      if (value.getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
        return null;
      }
      if (value.getValueType() != jakarta.json.JsonValue.ValueType.OBJECT) {
        throw new jakarta.json.bind.JsonbException("Expected a JSON object for House but got " + value.getValueType());
      }
      org.openapitools.model.House result = fromJsonObject(value.asJsonObject(), ctx);
      // some implementations (Johnzon) also apply this deserializer when a subclass of House is requested
      if (rtType instanceof Class && result != null && !((Class<?>) rtType).isInstance(result)) {
        throw new jakarta.json.bind.JsonbException("The discriminator of the JSON object resolves to " + result.getClass().getName()
            + ", which is not a " + ((Class<?>) rtType).getName());
      }
      return result;
    }

    public static org.openapitools.model.House fromJsonObject(jakarta.json.JsonObject json, jakarta.json.bind.serializer.DeserializationContext ctx) {
      jakarta.json.JsonValue discriminator = json.get("houseType");
      String discriminatorValue = discriminator == null || discriminator.getValueType() == jakarta.json.JsonValue.ValueType.NULL ? null
          : discriminator.getValueType() == jakarta.json.JsonValue.ValueType.STRING ? ((jakarta.json.JsonString) discriminator).getString()
          : discriminator.toString();
      if ("HOUSE".equals(discriminatorValue)) {
        return fromSelf(json, ctx);
      }
      if ("SHELTER".equals(discriminatorValue)) {
        try (jakarta.json.stream.JsonParser parser = parserOf(json)) {
          return ctx.deserialize(org.openapitools.model.Shelter.class, parser);
        }
      }
      throw new jakarta.json.bind.JsonbException("Unknown or missing discriminator value for 'houseType' of House: " + discriminatorValue);
    }

    private static org.openapitools.model.House fromSelf(jakarta.json.JsonObject json, jakarta.json.bind.serializer.DeserializationContext ctx) {
      JsonbData data;
      try (jakarta.json.stream.JsonParser parser = parserOf(json)) {
        data = ctx.deserialize(JsonbData.class, parser);
      }
      org.openapitools.model.House result = new org.openapitools.model.House();
      result.setHouseType(data.houseType);
      result.setResidents(data.residents);
      return result;
    }

    private static jakarta.json.stream.JsonParser parserOf(jakarta.json.JsonObject json) {
      return JSON_PROVIDER.createParser(new java.io.StringReader(json.toString()));
    }

    /**
     * Binding target for an object that maps to House itself: it is no subtype of House and has no
     * deserializer, so binding it does not recurse into this deserializer.
     */
    public static final class JsonbData {
      @jakarta.json.bind.annotation.JsonbProperty("houseType")
      public String houseType;
      @jakarta.json.bind.annotation.JsonbProperty("residents")
      @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
      public Map<String, Pet> residents = new HashMap<>();
    }
  }
}
