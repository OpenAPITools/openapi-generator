package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.Animal;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("Cat")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class Cat extends Animal implements Serializable {
  private @Valid Boolean declawed;

  protected Cat(CatBuilder<?, ?> b) {
    super(b);
    this.declawed = b.declawed;
  }

  public Cat() {
  }

  /**
   **/
  public Cat declawed(Boolean declawed) {
    this.declawed = declawed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("declawed")
  public Boolean getDeclawed() {
    return declawed;
  }

  @JsonProperty("declawed")
  public void setDeclawed(Boolean declawed) {
    this.declawed = declawed;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Cat cat = (Cat) o;
    return Objects.equals(this.declawed, cat.declawed) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(declawed, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Cat {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    declawed: ").append(toIndentedString(declawed)).append("\n");
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


  public static CatBuilder<?, ?> builder() {
    return new CatBuilderImpl();
  }

  private static final class CatBuilderImpl extends CatBuilder<Cat, CatBuilderImpl> {

    @Override
    protected CatBuilderImpl self() {
      return this;
    }

    @Override
    public Cat build() {
      return new Cat(this);
    }
  }

  public static abstract class CatBuilder<C extends Cat, B extends CatBuilder<C, B>> extends AnimalBuilder<C, B> {
    private Boolean declawed;

    public B declawed(Boolean declawed) {
      this.declawed = declawed;
      return self();
    }
  }
}

