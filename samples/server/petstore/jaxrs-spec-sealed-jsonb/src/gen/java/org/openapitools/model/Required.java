package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openapitools.model.Pet;
import org.openapitools.model.Vehicle;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public final class Required  implements Serializable {
  private Map<String, @Valid Pet> petMap = new HashMap<>();
  private List<@Valid Vehicle> vehicleList = new ArrayList<>();
  private String note;

  protected Required(RequiredBuilder<?, ?> b) {
    this.petMap = b.petMap;
    this.vehicleList = b.vehicleList;
    this.note = b.note;
  }

  public Required() {
  }

  @JsonbCreator
  public Required(
    @JsonbProperty("petMap") @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class) Map<String, @Valid Pet> petMap,
    @JsonbProperty("vehicleList") @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Vehicle.JsonbPolymorphicContainerDeserializer.class) List<@Valid Vehicle> vehicleList
  ) {
    this.petMap = petMap;
    this.vehicleList = vehicleList;
  }

  /**
   **/
  public Required petMap(Map<String, @Valid Pet> petMap) {
    this.petMap = petMap;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonbProperty("petMap")
  @NotNull public Map<String, @Valid Pet> getPetMap() {
    return petMap;
  }

  @JsonbProperty("petMap")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPetMap(Map<String, @Valid Pet> petMap) {
    this.petMap = petMap;
  }

  public Required putPetMapItem(String key, Pet petMapItem) {
    if (this.petMap == null) {
      this.petMap = new HashMap<>();
    }

    this.petMap.put(key, petMapItem);
    return this;
  }

  public Required removePetMapItem(String key) {
    if (this.petMap != null) {
      this.petMap.remove(key);
    }

    return this;
  }
  /**
   **/
  public Required vehicleList(List<@Valid Vehicle> vehicleList) {
    this.vehicleList = vehicleList;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonbProperty("vehicleList")
  @NotNull public List<@Valid Vehicle> getVehicleList() {
    return vehicleList;
  }

  @JsonbProperty("vehicleList")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Vehicle.JsonbPolymorphicContainerDeserializer.class)
  public void setVehicleList(List<@Valid Vehicle> vehicleList) {
    this.vehicleList = vehicleList;
  }

  public Required addVehicleListItem(Vehicle vehicleListItem) {
    if (this.vehicleList == null) {
      this.vehicleList = new ArrayList<>();
    }

    this.vehicleList.add(vehicleListItem);
    return this;
  }

  public Required removeVehicleListItem(Vehicle vehicleListItem) {
    if (vehicleListItem != null && this.vehicleList != null) {
      this.vehicleList.remove(vehicleListItem);
    }

    return this;
  }
  /**
   **/
  public Required note(String note) {
    this.note = note;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("note")
  public String getNote() {
    return note;
  }

  @JsonbProperty("note")
  public void setNote(String note) {
    this.note = note;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Required required = (Required) o;
    return Objects.equals(this.petMap, required.petMap) &&
        Objects.equals(this.vehicleList, required.vehicleList) &&
        Objects.equals(this.note, required.note);
  }

  @Override
  public int hashCode() {
    return Objects.hash(petMap, vehicleList, note);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Required {\n");
    
    sb.append("    petMap: ").append(toIndentedString(petMap)).append("\n");
    sb.append("    vehicleList: ").append(toIndentedString(vehicleList)).append("\n");
    sb.append("    note: ").append(toIndentedString(note)).append("\n");
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


  public static RequiredBuilder<?, ?> builder() {
    return new RequiredBuilderImpl();
  }

  private static final class RequiredBuilderImpl extends RequiredBuilder<Required, RequiredBuilderImpl> {

    @Override
    protected RequiredBuilderImpl self() {
      return this;
    }

    @Override
    public Required build() {
      return new Required(this);
    }
  }

  public static abstract class RequiredBuilder<C extends Required, B extends RequiredBuilder<C, B>>  {
    private Map<String, Pet> petMap = new HashMap<>();
    private List<Vehicle> vehicleList = new ArrayList<>();
    private String note;
    protected abstract B self();

    public abstract C build();

    public B petMap(Map<String, Pet> petMap) {
      this.petMap = petMap;
      return self();
    }
    public B vehicleList(List<Vehicle> vehicleList) {
      this.vehicleList = vehicleList;
      return self();
    }
    public B note(String note) {
      this.note = note;
      return self();
    }
  }
}
