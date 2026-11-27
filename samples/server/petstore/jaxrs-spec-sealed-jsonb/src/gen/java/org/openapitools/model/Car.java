package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.Vehicle;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public final class Car extends Vehicle implements Serializable {
  private Integer doors;

  protected Car(CarBuilder<?, ?> b) {
    super(b);
    this.doors = b.doors;
  }

  public Car() {
    this.setKind("CAR");
  }

  @JsonbCreator
  public Car(
    @JsonbProperty("kind") String kind
  ) {
    super(
      kind
    );
  }

  /**
   **/
  public Car doors(Integer doors) {
    this.doors = doors;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("doors")
  public Integer getDoors() {
    return doors;
  }

  @JsonbProperty("doors")
  public void setDoors(Integer doors) {
    this.doors = doors;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Car car = (Car) o;
    return Objects.equals(this.doors, car.doors) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(doors, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Car {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    doors: ").append(toIndentedString(doors)).append("\n");
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


  public static CarBuilder<?, ?> builder() {
    return new CarBuilderImpl();
  }

  private static final class CarBuilderImpl extends CarBuilder<Car, CarBuilderImpl> {

    @Override
    protected CarBuilderImpl self() {
      return this;
    }

    @Override
    public Car build() {
      return new Car(this);
    }
  }

  public static abstract class CarBuilder<C extends Car, B extends CarBuilder<C, B>> extends VehicleBuilder<C, B> {
    private Integer doors;

    {
      // the discriminator values of Car, which the caller may override
      this.kind("CAR");
    }

    public B doors(Integer doors) {
      this.doors = doors;
      return self();
    }
  }
}
