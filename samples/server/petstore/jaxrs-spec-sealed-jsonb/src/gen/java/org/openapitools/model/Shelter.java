package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.HashMap;
import java.util.Map;
import org.openapitools.model.House;
import org.openapitools.model.Pet;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public final class Shelter extends House implements Serializable {
  private String city;

  protected Shelter(ShelterBuilder<?, ?> b) {
    super(b);
    this.city = b.city;
  }

  public Shelter() {
    this.setHouseType("SHELTER");
  }

  @JsonbCreator
  public Shelter(
    @JsonbProperty("houseType") String houseType,
    @JsonbProperty("residents") @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class) Map<String, @Valid Pet> residents
  ) {
    super(
      houseType,
      residents
    );
  }

  /**
   **/
  public Shelter city(String city) {
    this.city = city;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("city")
  public String getCity() {
    return city;
  }

  @JsonbProperty("city")
  public void setCity(String city) {
    this.city = city;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Shelter shelter = (Shelter) o;
    return Objects.equals(this.city, shelter.city) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(city, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Shelter {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    city: ").append(toIndentedString(city)).append("\n");
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


  public static ShelterBuilder<?, ?> builder() {
    return new ShelterBuilderImpl();
  }

  private static final class ShelterBuilderImpl extends ShelterBuilder<Shelter, ShelterBuilderImpl> {

    @Override
    protected ShelterBuilderImpl self() {
      return this;
    }

    @Override
    public Shelter build() {
      return new Shelter(this);
    }
  }

  public static abstract class ShelterBuilder<C extends Shelter, B extends ShelterBuilder<C, B>> extends HouseBuilder<C, B> {
    private String city;

    {
      // the discriminator values of Shelter, which the caller may override
      this.houseType("SHELTER");
    }

    public B city(String city) {
      this.city = city;
      return self();
    }
  }
}
