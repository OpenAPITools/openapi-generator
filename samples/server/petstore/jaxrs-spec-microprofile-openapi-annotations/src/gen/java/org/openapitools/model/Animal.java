package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
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

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "className", visible = true)
@JsonSubTypes({
  @JsonSubTypes.Type(value = BigCat.class, name = "BigCat"),
  @JsonSubTypes.Type(value = Cat.class, name = "Cat"),
  @JsonSubTypes.Type(value = Dog.class, name = "Dog"),
})


@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("Animal")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class Animal  implements Serializable {
  private @Valid String className;
  private @Valid String color = "red";

  protected Animal(AnimalBuilder<?, ?> b) {
    this.className = b.className;
    this.color = b.color;
  }

  public Animal() {
  }

  /**
   **/
  public Animal className(String className) {
    this.className = className;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @org.eclipse.microprofile.openapi.annotations.media.Schema(required = true, description = "")
  @JsonProperty("className")
  @NotNull
  public String getClassName() {
    return className;
  }

  @JsonProperty("className")
  public void setClassName(String className) {
    this.className = className;
  }

  /**
   **/
  public Animal color(String color) {
    this.color = color;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("color")
  public String getColor() {
    return color;
  }

  @JsonProperty("color")
  public void setColor(String color) {
    this.color = color;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Animal animal = (Animal) o;
    return Objects.equals(this.className, animal.className) &&
        Objects.equals(this.color, animal.color);
  }

  @Override
  public int hashCode() {
    return Objects.hash(className, color);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Animal {\n");
    
    sb.append("    className: ").append(toIndentedString(className)).append("\n");
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
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


  public static AnimalBuilder<?, ?> builder() {
    return new AnimalBuilderImpl();
  }

  private static final class AnimalBuilderImpl extends AnimalBuilder<Animal, AnimalBuilderImpl> {

    @Override
    protected AnimalBuilderImpl self() {
      return this;
    }

    @Override
    public Animal build() {
      return new Animal(this);
    }
  }

  public static abstract class AnimalBuilder<C extends Animal, B extends AnimalBuilder<C, B>>  {
    private String className;
    private String color = "red";
    protected abstract B self();

    public abstract C build();

    public B className(String className) {
      this.className = className;
      return self();
    }
    public B color(String color) {
      this.color = color;
      return self();
    }
  }
}

