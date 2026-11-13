package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.openapitools.model.Car;
import org.openapitools.model.Cat;
import org.openapitools.model.PersianCat;
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
public final class Containers  implements Serializable {
  private List<@Valid Pet> petList = new ArrayList<>();
  private Set<@Valid Pet> petSet = new LinkedHashSet<>();
  private Map<String, @Valid Pet> petMap = new HashMap<>();
  private Map<String, List<@Valid Pet>> petMapOfLists = new HashMap<>();
  private List<Map<String, @Valid Pet>> petListOfMaps = new ArrayList<>();
  private Map<String, Map<String, @Valid Pet>> petMapOfMaps = new HashMap<>();
  private List<List<@Valid Pet>> petListOfLists = new ArrayList<>();
  private Map<String, @Valid Cat> catMap = new HashMap<>();
  private Map<String, @Valid Vehicle> vehicleMap = new HashMap<>();
  private List<@Valid Vehicle> vehicleList = new ArrayList<>();
  private Map<String, @Valid Car> carMap = new HashMap<>();
  private Map<String, @Valid PersianCat> persianMap = new HashMap<>();
  private Map<String, String> stringMap = new HashMap<>();
  private List<List<String>> stringLists = new ArrayList<>();

  protected Containers(ContainersBuilder<?, ?> b) {
    this.petList = b.petList;
    this.petSet = b.petSet;
    this.petMap = b.petMap;
    this.petMapOfLists = b.petMapOfLists;
    this.petListOfMaps = b.petListOfMaps;
    this.petMapOfMaps = b.petMapOfMaps;
    this.petListOfLists = b.petListOfLists;
    this.catMap = b.catMap;
    this.vehicleMap = b.vehicleMap;
    this.vehicleList = b.vehicleList;
    this.carMap = b.carMap;
    this.persianMap = b.persianMap;
    this.stringMap = b.stringMap;
    this.stringLists = b.stringLists;
  }

  public Containers() {
  }

  /**
   **/
  public Containers petList(List<@Valid Pet> petList) {
    this.petList = petList;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("petList")
  public List<@Valid Pet> getPetList() {
    return petList;
  }

  @JsonbProperty("petList")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPetList(List<@Valid Pet> petList) {
    this.petList = petList;
  }

  public Containers addPetListItem(Pet petListItem) {
    if (this.petList == null) {
      this.petList = new ArrayList<>();
    }

    this.petList.add(petListItem);
    return this;
  }

  public Containers removePetListItem(Pet petListItem) {
    if (petListItem != null && this.petList != null) {
      this.petList.remove(petListItem);
    }

    return this;
  }
  /**
   **/
  public Containers petSet(Set<@Valid Pet> petSet) {
    this.petSet = petSet;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("petSet")
  public Set<@Valid Pet> getPetSet() {
    return petSet;
  }

  @JsonbProperty("petSet")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPetSet(Set<@Valid Pet> petSet) {
    this.petSet = petSet;
  }

  public Containers addPetSetItem(Pet petSetItem) {
    if (this.petSet == null) {
      this.petSet = new LinkedHashSet<>();
    }

    this.petSet.add(petSetItem);
    return this;
  }

  public Containers removePetSetItem(Pet petSetItem) {
    if (petSetItem != null && this.petSet != null) {
      this.petSet.remove(petSetItem);
    }

    return this;
  }
  /**
   **/
  public Containers petMap(Map<String, @Valid Pet> petMap) {
    this.petMap = petMap;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("petMap")
  public Map<String, @Valid Pet> getPetMap() {
    return petMap;
  }

  @JsonbProperty("petMap")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPetMap(Map<String, @Valid Pet> petMap) {
    this.petMap = petMap;
  }

  public Containers putPetMapItem(String key, Pet petMapItem) {
    if (this.petMap == null) {
      this.petMap = new HashMap<>();
    }

    this.petMap.put(key, petMapItem);
    return this;
  }

  public Containers removePetMapItem(String key) {
    if (this.petMap != null) {
      this.petMap.remove(key);
    }

    return this;
  }
  /**
   **/
  public Containers petMapOfLists(Map<String, List<@Valid Pet>> petMapOfLists) {
    this.petMapOfLists = petMapOfLists;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("petMapOfLists")
  public Map<String, List<@Valid Pet>> getPetMapOfLists() {
    return petMapOfLists;
  }

  @JsonbProperty("petMapOfLists")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPetMapOfLists(Map<String, List<@Valid Pet>> petMapOfLists) {
    this.petMapOfLists = petMapOfLists;
  }

  public Containers putPetMapOfListsItem(String key, List<@Valid Pet> petMapOfListsItem) {
    if (this.petMapOfLists == null) {
      this.petMapOfLists = new HashMap<>();
    }

    this.petMapOfLists.put(key, petMapOfListsItem);
    return this;
  }

  public Containers removePetMapOfListsItem(String key) {
    if (this.petMapOfLists != null) {
      this.petMapOfLists.remove(key);
    }

    return this;
  }
  /**
   **/
  public Containers petListOfMaps(List<Map<String, @Valid Pet>> petListOfMaps) {
    this.petListOfMaps = petListOfMaps;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("petListOfMaps")
  public List<Map<String, @Valid Pet>> getPetListOfMaps() {
    return petListOfMaps;
  }

  @JsonbProperty("petListOfMaps")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPetListOfMaps(List<Map<String, @Valid Pet>> petListOfMaps) {
    this.petListOfMaps = petListOfMaps;
  }

  public Containers addPetListOfMapsItem(Map<String, @Valid Pet> petListOfMapsItem) {
    if (this.petListOfMaps == null) {
      this.petListOfMaps = new ArrayList<>();
    }

    this.petListOfMaps.add(petListOfMapsItem);
    return this;
  }

  public Containers removePetListOfMapsItem(Map<String, @Valid Pet> petListOfMapsItem) {
    if (petListOfMapsItem != null && this.petListOfMaps != null) {
      this.petListOfMaps.remove(petListOfMapsItem);
    }

    return this;
  }
  /**
   **/
  public Containers petMapOfMaps(Map<String, Map<String, @Valid Pet>> petMapOfMaps) {
    this.petMapOfMaps = petMapOfMaps;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("petMapOfMaps")
  public Map<String, Map<String, @Valid Pet>> getPetMapOfMaps() {
    return petMapOfMaps;
  }

  @JsonbProperty("petMapOfMaps")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPetMapOfMaps(Map<String, Map<String, @Valid Pet>> petMapOfMaps) {
    this.petMapOfMaps = petMapOfMaps;
  }

  public Containers putPetMapOfMapsItem(String key, Map<String, @Valid Pet> petMapOfMapsItem) {
    if (this.petMapOfMaps == null) {
      this.petMapOfMaps = new HashMap<>();
    }

    this.petMapOfMaps.put(key, petMapOfMapsItem);
    return this;
  }

  public Containers removePetMapOfMapsItem(String key) {
    if (this.petMapOfMaps != null) {
      this.petMapOfMaps.remove(key);
    }

    return this;
  }
  /**
   **/
  public Containers petListOfLists(List<List<@Valid Pet>> petListOfLists) {
    this.petListOfLists = petListOfLists;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("petListOfLists")
  public List<List<@Valid Pet>> getPetListOfLists() {
    return petListOfLists;
  }

  @JsonbProperty("petListOfLists")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPetListOfLists(List<List<@Valid Pet>> petListOfLists) {
    this.petListOfLists = petListOfLists;
  }

  public Containers addPetListOfListsItem(List<@Valid Pet> petListOfListsItem) {
    if (this.petListOfLists == null) {
      this.petListOfLists = new ArrayList<>();
    }

    this.petListOfLists.add(petListOfListsItem);
    return this;
  }

  public Containers removePetListOfListsItem(List<@Valid Pet> petListOfListsItem) {
    if (petListOfListsItem != null && this.petListOfLists != null) {
      this.petListOfLists.remove(petListOfListsItem);
    }

    return this;
  }
  /**
   **/
  public Containers catMap(Map<String, @Valid Cat> catMap) {
    this.catMap = catMap;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("catMap")
  public Map<String, @Valid Cat> getCatMap() {
    return catMap;
  }

  @JsonbProperty("catMap")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Cat.JsonbPolymorphicContainerDeserializer.class)
  public void setCatMap(Map<String, @Valid Cat> catMap) {
    this.catMap = catMap;
  }

  public Containers putCatMapItem(String key, Cat catMapItem) {
    if (this.catMap == null) {
      this.catMap = new HashMap<>();
    }

    this.catMap.put(key, catMapItem);
    return this;
  }

  public Containers removeCatMapItem(String key) {
    if (this.catMap != null) {
      this.catMap.remove(key);
    }

    return this;
  }
  /**
   **/
  public Containers vehicleMap(Map<String, @Valid Vehicle> vehicleMap) {
    this.vehicleMap = vehicleMap;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("vehicleMap")
  public Map<String, @Valid Vehicle> getVehicleMap() {
    return vehicleMap;
  }

  @JsonbProperty("vehicleMap")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Vehicle.JsonbPolymorphicContainerDeserializer.class)
  public void setVehicleMap(Map<String, @Valid Vehicle> vehicleMap) {
    this.vehicleMap = vehicleMap;
  }

  public Containers putVehicleMapItem(String key, Vehicle vehicleMapItem) {
    if (this.vehicleMap == null) {
      this.vehicleMap = new HashMap<>();
    }

    this.vehicleMap.put(key, vehicleMapItem);
    return this;
  }

  public Containers removeVehicleMapItem(String key) {
    if (this.vehicleMap != null) {
      this.vehicleMap.remove(key);
    }

    return this;
  }
  /**
   **/
  public Containers vehicleList(List<@Valid Vehicle> vehicleList) {
    this.vehicleList = vehicleList;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("vehicleList")
  public List<@Valid Vehicle> getVehicleList() {
    return vehicleList;
  }

  @JsonbProperty("vehicleList")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Vehicle.JsonbPolymorphicContainerDeserializer.class)
  public void setVehicleList(List<@Valid Vehicle> vehicleList) {
    this.vehicleList = vehicleList;
  }

  public Containers addVehicleListItem(Vehicle vehicleListItem) {
    if (this.vehicleList == null) {
      this.vehicleList = new ArrayList<>();
    }

    this.vehicleList.add(vehicleListItem);
    return this;
  }

  public Containers removeVehicleListItem(Vehicle vehicleListItem) {
    if (vehicleListItem != null && this.vehicleList != null) {
      this.vehicleList.remove(vehicleListItem);
    }

    return this;
  }
  /**
   **/
  public Containers carMap(Map<String, @Valid Car> carMap) {
    this.carMap = carMap;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("carMap")
  public Map<String, @Valid Car> getCarMap() {
    return carMap;
  }

  @JsonbProperty("carMap")
  public void setCarMap(Map<String, @Valid Car> carMap) {
    this.carMap = carMap;
  }

  public Containers putCarMapItem(String key, Car carMapItem) {
    if (this.carMap == null) {
      this.carMap = new HashMap<>();
    }

    this.carMap.put(key, carMapItem);
    return this;
  }

  public Containers removeCarMapItem(String key) {
    if (this.carMap != null) {
      this.carMap.remove(key);
    }

    return this;
  }
  /**
   **/
  public Containers persianMap(Map<String, @Valid PersianCat> persianMap) {
    this.persianMap = persianMap;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("persianMap")
  public Map<String, @Valid PersianCat> getPersianMap() {
    return persianMap;
  }

  @JsonbProperty("persianMap")
  public void setPersianMap(Map<String, @Valid PersianCat> persianMap) {
    this.persianMap = persianMap;
  }

  public Containers putPersianMapItem(String key, PersianCat persianMapItem) {
    if (this.persianMap == null) {
      this.persianMap = new HashMap<>();
    }

    this.persianMap.put(key, persianMapItem);
    return this;
  }

  public Containers removePersianMapItem(String key) {
    if (this.persianMap != null) {
      this.persianMap.remove(key);
    }

    return this;
  }
  /**
   **/
  public Containers stringMap(Map<String, String> stringMap) {
    this.stringMap = stringMap;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("stringMap")
  public Map<String, String> getStringMap() {
    return stringMap;
  }

  @JsonbProperty("stringMap")
  public void setStringMap(Map<String, String> stringMap) {
    this.stringMap = stringMap;
  }

  public Containers putStringMapItem(String key, String stringMapItem) {
    if (this.stringMap == null) {
      this.stringMap = new HashMap<>();
    }

    this.stringMap.put(key, stringMapItem);
    return this;
  }

  public Containers removeStringMapItem(String key) {
    if (this.stringMap != null) {
      this.stringMap.remove(key);
    }

    return this;
  }
  /**
   **/
  public Containers stringLists(List<List<String>> stringLists) {
    this.stringLists = stringLists;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("stringLists")
  public List<List<String>> getStringLists() {
    return stringLists;
  }

  @JsonbProperty("stringLists")
  public void setStringLists(List<List<String>> stringLists) {
    this.stringLists = stringLists;
  }

  public Containers addStringListsItem(List<String> stringListsItem) {
    if (this.stringLists == null) {
      this.stringLists = new ArrayList<>();
    }

    this.stringLists.add(stringListsItem);
    return this;
  }

  public Containers removeStringListsItem(List<String> stringListsItem) {
    if (stringListsItem != null && this.stringLists != null) {
      this.stringLists.remove(stringListsItem);
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
    Containers containers = (Containers) o;
    return Objects.equals(this.petList, containers.petList) &&
        Objects.equals(this.petSet, containers.petSet) &&
        Objects.equals(this.petMap, containers.petMap) &&
        Objects.equals(this.petMapOfLists, containers.petMapOfLists) &&
        Objects.equals(this.petListOfMaps, containers.petListOfMaps) &&
        Objects.equals(this.petMapOfMaps, containers.petMapOfMaps) &&
        Objects.equals(this.petListOfLists, containers.petListOfLists) &&
        Objects.equals(this.catMap, containers.catMap) &&
        Objects.equals(this.vehicleMap, containers.vehicleMap) &&
        Objects.equals(this.vehicleList, containers.vehicleList) &&
        Objects.equals(this.carMap, containers.carMap) &&
        Objects.equals(this.persianMap, containers.persianMap) &&
        Objects.equals(this.stringMap, containers.stringMap) &&
        Objects.equals(this.stringLists, containers.stringLists);
  }

  @Override
  public int hashCode() {
    return Objects.hash(petList, petSet, petMap, petMapOfLists, petListOfMaps, petMapOfMaps, petListOfLists, catMap, vehicleMap, vehicleList, carMap, persianMap, stringMap, stringLists);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Containers {\n");
    
    sb.append("    petList: ").append(toIndentedString(petList)).append("\n");
    sb.append("    petSet: ").append(toIndentedString(petSet)).append("\n");
    sb.append("    petMap: ").append(toIndentedString(petMap)).append("\n");
    sb.append("    petMapOfLists: ").append(toIndentedString(petMapOfLists)).append("\n");
    sb.append("    petListOfMaps: ").append(toIndentedString(petListOfMaps)).append("\n");
    sb.append("    petMapOfMaps: ").append(toIndentedString(petMapOfMaps)).append("\n");
    sb.append("    petListOfLists: ").append(toIndentedString(petListOfLists)).append("\n");
    sb.append("    catMap: ").append(toIndentedString(catMap)).append("\n");
    sb.append("    vehicleMap: ").append(toIndentedString(vehicleMap)).append("\n");
    sb.append("    vehicleList: ").append(toIndentedString(vehicleList)).append("\n");
    sb.append("    carMap: ").append(toIndentedString(carMap)).append("\n");
    sb.append("    persianMap: ").append(toIndentedString(persianMap)).append("\n");
    sb.append("    stringMap: ").append(toIndentedString(stringMap)).append("\n");
    sb.append("    stringLists: ").append(toIndentedString(stringLists)).append("\n");
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


  public static ContainersBuilder<?, ?> builder() {
    return new ContainersBuilderImpl();
  }

  private static final class ContainersBuilderImpl extends ContainersBuilder<Containers, ContainersBuilderImpl> {

    @Override
    protected ContainersBuilderImpl self() {
      return this;
    }

    @Override
    public Containers build() {
      return new Containers(this);
    }
  }

  public static abstract class ContainersBuilder<C extends Containers, B extends ContainersBuilder<C, B>>  {
    private List<Pet> petList = new ArrayList<>();
    private Set<Pet> petSet = new LinkedHashSet<>();
    private Map<String, Pet> petMap = new HashMap<>();
    private Map<String, List<Pet>> petMapOfLists = new HashMap<>();
    private List<Map<String, Pet>> petListOfMaps = new ArrayList<>();
    private Map<String, Map<String, Pet>> petMapOfMaps = new HashMap<>();
    private List<List<Pet>> petListOfLists = new ArrayList<>();
    private Map<String, Cat> catMap = new HashMap<>();
    private Map<String, Vehicle> vehicleMap = new HashMap<>();
    private List<Vehicle> vehicleList = new ArrayList<>();
    private Map<String, Car> carMap = new HashMap<>();
    private Map<String, PersianCat> persianMap = new HashMap<>();
    private Map<String, String> stringMap = new HashMap<>();
    private List<List<String>> stringLists = new ArrayList<>();
    protected abstract B self();

    public abstract C build();

    public B petList(List<Pet> petList) {
      this.petList = petList;
      return self();
    }
    public B petSet(Set<Pet> petSet) {
      this.petSet = petSet;
      return self();
    }
    public B petMap(Map<String, Pet> petMap) {
      this.petMap = petMap;
      return self();
    }
    public B petMapOfLists(Map<String, List<Pet>> petMapOfLists) {
      this.petMapOfLists = petMapOfLists;
      return self();
    }
    public B petListOfMaps(List<Map<String, Pet>> petListOfMaps) {
      this.petListOfMaps = petListOfMaps;
      return self();
    }
    public B petMapOfMaps(Map<String, Map<String, Pet>> petMapOfMaps) {
      this.petMapOfMaps = petMapOfMaps;
      return self();
    }
    public B petListOfLists(List<List<Pet>> petListOfLists) {
      this.petListOfLists = petListOfLists;
      return self();
    }
    public B catMap(Map<String, Cat> catMap) {
      this.catMap = catMap;
      return self();
    }
    public B vehicleMap(Map<String, Vehicle> vehicleMap) {
      this.vehicleMap = vehicleMap;
      return self();
    }
    public B vehicleList(List<Vehicle> vehicleList) {
      this.vehicleList = vehicleList;
      return self();
    }
    public B carMap(Map<String, Car> carMap) {
      this.carMap = carMap;
      return self();
    }
    public B persianMap(Map<String, PersianCat> persianMap) {
      this.persianMap = persianMap;
      return self();
    }
    public B stringMap(Map<String, String> stringMap) {
      this.stringMap = stringMap;
      return self();
    }
    public B stringLists(List<List<String>> stringLists) {
      this.stringLists = stringLists;
      return self();
    }
  }
}
