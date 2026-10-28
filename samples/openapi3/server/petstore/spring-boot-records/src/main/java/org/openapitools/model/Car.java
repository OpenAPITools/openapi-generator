package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonValue;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.Owner;
import org.openapitools.model.Vehicle;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * Car
 *
 * @param type Get type
 * @param wheels Get wheels
 * @param fuel Get fuel
 * @param owner Get owner
 * @param previousOwners Get previousOwners
 * @param registeredAt Get registeredAt
 * @param doors Get doors
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public record Car(
    @JsonInclude(JsonInclude.Include.NON_NULL)
    @NotNull 
    @Schema(name = "type", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonProperty("type")
    String type,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "wheels", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("wheels")
    @Nullable Integer wheels,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "fuel", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("fuel")
    FuelEnum fuel,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @Valid 
    @Schema(name = "owner", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("owner")
    @Nullable Owner owner,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "previousOwners", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("previousOwners")
    List<@Valid Owner> previousOwners,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
    @Valid 
    @Schema(name = "registeredAt", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("registeredAt")
    @Nullable OffsetDateTime registeredAt,

    @JsonInclude(JsonInclude.Include.NON_NULL)
    
    @Schema(name = "doors", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
    @JsonProperty("doors")
    @Nullable Integer doors
) implements Vehicle {

  public Car {
    if (fuel == null) {
      fuel = FuelEnum.PETROL;
    }
    if (previousOwners == null) {
      previousOwners = new ArrayList<>();
    }
  }

  /**
   * Builder of Car. Components that are not set get their initial value from the constructor.
   * The builder can be used again after {@link #build()}.
   */
  public static class Builder {

    private String type;

    private Integer wheels;

    private FuelEnum fuel;

    private Owner owner;

    private List<Owner> previousOwners;

    private OffsetDateTime registeredAt;

    private Integer doors;

    public Builder() {
    }

    protected Builder copyOf(Car value) {
      this.type = value.type();
      this.wheels = value.wheels();
      this.fuel = value.fuel();
      this.owner = value.owner();
      this.previousOwners = value.previousOwners();
      this.registeredAt = value.registeredAt();
      this.doors = value.doors();
      return this;
    }

    public Car.Builder type(String type) {
      this.type = type;
      return this;
    }

    public Car.Builder wheels(Integer wheels) {
      this.wheels = wheels;
      return this;
    }

    public Car.Builder fuel(FuelEnum fuel) {
      this.fuel = fuel;
      return this;
    }

    public Car.Builder owner(Owner owner) {
      this.owner = owner;
      return this;
    }

    public Car.Builder previousOwners(List<Owner> previousOwners) {
      this.previousOwners = previousOwners;
      return this;
    }

    public Car.Builder registeredAt(OffsetDateTime registeredAt) {
      this.registeredAt = registeredAt;
      return this;
    }

    public Car.Builder doors(Integer doors) {
      this.doors = doors;
      return this;
    }

    /**
     * Returns a new Car from the values set so far; the builder stays usable.
     */
    public Car build() {
      return new Car(this.type, this.wheels, this.fuel, this.owner, this.previousOwners, this.registeredAt, this.doors);
    }
  }

  /**
   * Create a builder with no initialized component (the constructor applies the initial values).
   */
  public static Car.Builder builder() {
    return new Car.Builder();
  }

  /**
   * Create a builder holding every component of this record.
   */
  public Car.Builder toBuilder() {
    return new Car.Builder().copyOf(this);
  }

}

