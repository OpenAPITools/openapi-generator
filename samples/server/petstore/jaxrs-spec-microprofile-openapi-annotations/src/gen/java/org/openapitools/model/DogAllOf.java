package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("Dog_allOf")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class DogAllOf  implements Serializable {
  private @Valid String breed;

  protected DogAllOf(DogAllOfBuilder<?, ?> b) {
    this.breed = b.breed;
  }

  public DogAllOf() {
  }

  /**
   **/
  public DogAllOf breed(String breed) {
    this.breed = breed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("breed")
  public String getBreed() {
    return breed;
  }

  @JsonProperty("breed")
  public void setBreed(String breed) {
    this.breed = breed;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DogAllOf dogAllOf = (DogAllOf) o;
    return Objects.equals(this.breed, dogAllOf.breed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(breed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DogAllOf {\n");
    
    sb.append("    breed: ").append(toIndentedString(breed)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }


  public static DogAllOfBuilder<?, ?> builder() {
    return new DogAllOfBuilderImpl();
  }

  private static final class DogAllOfBuilderImpl extends DogAllOfBuilder<DogAllOf, DogAllOfBuilderImpl> {

    @Override
    protected DogAllOfBuilderImpl self() {
      return this;
    }

    @Override
    public DogAllOf build() {
      return new DogAllOf(this);
    }
  }

  public static abstract class DogAllOfBuilder<C extends DogAllOf, B extends DogAllOfBuilder<C, B>>  {
    private String breed;
    protected abstract B self();

    public abstract C build();

    public B breed(String breed) {
      this.breed = breed;
      return self();
    }
  }
}

