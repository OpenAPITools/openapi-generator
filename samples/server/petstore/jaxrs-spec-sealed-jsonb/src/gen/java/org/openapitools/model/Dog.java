package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openapitools.model.Pet;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public final class Dog extends Pet implements Serializable {
  private Integer barks;
  private List<@Valid Pet> pack = new ArrayList<>();
  private Map<String, @Valid Pet> friends = new HashMap<>();

  protected Dog(DogBuilder<?, ?> b) {
    super(b);
    this.barks = b.barks;
    this.pack = b.pack;
    this.friends = b.friends;
  }

  public Dog() {
    this.setPetType("DOG");
  }

  @JsonbCreator
  public Dog(
    @JsonbProperty("petType") String petType
  ) {
    super(
      petType
    );
  }

  /**
   **/
  public Dog barks(Integer barks) {
    this.barks = barks;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("barks")
  public Integer getBarks() {
    return barks;
  }

  @JsonbProperty("barks")
  public void setBarks(Integer barks) {
    this.barks = barks;
  }

  /**
   **/
  public Dog pack(List<@Valid Pet> pack) {
    this.pack = pack;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("pack")
  public List<@Valid Pet> getPack() {
    return pack;
  }

  @JsonbProperty("pack")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setPack(List<@Valid Pet> pack) {
    this.pack = pack;
  }

  public Dog addPackItem(Pet packItem) {
    if (this.pack == null) {
      this.pack = new ArrayList<>();
    }

    this.pack.add(packItem);
    return this;
  }

  public Dog removePackItem(Pet packItem) {
    if (packItem != null && this.pack != null) {
      this.pack.remove(packItem);
    }

    return this;
  }
  /**
   **/
  public Dog friends(Map<String, @Valid Pet> friends) {
    this.friends = friends;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("friends")
  public Map<String, @Valid Pet> getFriends() {
    return friends;
  }

  @JsonbProperty("friends")
  @jakarta.json.bind.annotation.JsonbTypeDeserializer(org.openapitools.model.Pet.JsonbPolymorphicContainerDeserializer.class)
  public void setFriends(Map<String, @Valid Pet> friends) {
    this.friends = friends;
  }

  public Dog putFriendsItem(String key, Pet friendsItem) {
    if (this.friends == null) {
      this.friends = new HashMap<>();
    }

    this.friends.put(key, friendsItem);
    return this;
  }

  public Dog removeFriendsItem(String key) {
    if (this.friends != null) {
      this.friends.remove(key);
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
    Dog dog = (Dog) o;
    return Objects.equals(this.barks, dog.barks) &&
        Objects.equals(this.pack, dog.pack) &&
        Objects.equals(this.friends, dog.friends) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(barks, pack, friends, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Dog {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    barks: ").append(toIndentedString(barks)).append("\n");
    sb.append("    pack: ").append(toIndentedString(pack)).append("\n");
    sb.append("    friends: ").append(toIndentedString(friends)).append("\n");
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


  public static DogBuilder<?, ?> builder() {
    return new DogBuilderImpl();
  }

  private static final class DogBuilderImpl extends DogBuilder<Dog, DogBuilderImpl> {

    @Override
    protected DogBuilderImpl self() {
      return this;
    }

    @Override
    public Dog build() {
      return new Dog(this);
    }
  }

  public static abstract class DogBuilder<C extends Dog, B extends DogBuilder<C, B>> extends PetBuilder<C, B> {
    private Integer barks;
    private List<Pet> pack = new ArrayList<>();
    private Map<String, Pet> friends = new HashMap<>();

    {
      // the discriminator values of Dog, which the caller may override
      this.petType("DOG");
    }

    public B barks(Integer barks) {
      this.barks = barks;
      return self();
    }
    public B pack(List<Pet> pack) {
      this.pack = pack;
      return self();
    }
    public B friends(Map<String, Pet> friends) {
      this.friends = friends;
      return self();
    }
  }
}
