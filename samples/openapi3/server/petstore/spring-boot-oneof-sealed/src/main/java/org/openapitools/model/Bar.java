package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.model.Entity;
import org.openapitools.model.FooRefOrValue;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Bar
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public final class Bar extends Entity implements BarRefOrValue {

  private String id;

  private @Nullable String barPropA;

  private @Nullable String fooPropB;

  private @Nullable FooRefOrValue foo;

  public Bar() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Bar(String id, String atType) {
    super(atType);
    this.id = id;
  }

  public Bar id(String id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
   */
  @NotNull 
  @Schema(name = "id", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("id")
  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public Bar barPropA(@Nullable String barPropA) {
    this.barPropA = barPropA;
    return this;
  }

  /**
   * Get barPropA
   * @return barPropA
   */
  
  @Schema(name = "barPropA", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("barPropA")
  public @Nullable String getBarPropA() {
    return barPropA;
  }

  public void setBarPropA(@Nullable String barPropA) {
    this.barPropA = barPropA;
  }

  public Bar fooPropB(@Nullable String fooPropB) {
    this.fooPropB = fooPropB;
    return this;
  }

  /**
   * Get fooPropB
   * @return fooPropB
   */
  
  @Schema(name = "fooPropB", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("fooPropB")
  public @Nullable String getFooPropB() {
    return fooPropB;
  }

  public void setFooPropB(@Nullable String fooPropB) {
    this.fooPropB = fooPropB;
  }

  public Bar foo(@Nullable FooRefOrValue foo) {
    this.foo = foo;
    return this;
  }

  /**
   * Get foo
   * @return foo
   */
  @Valid 
  @Schema(name = "foo", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("foo")
  public @Nullable FooRefOrValue getFoo() {
    return foo;
  }

  public void setFoo(@Nullable FooRefOrValue foo) {
    this.foo = foo;
  }


  public Bar href(String href) {
    super.href(href);
    return this;
  }

  public Bar atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public Bar atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public Bar atType(String atType) {
    super.atType(atType);
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
    Bar bar = (Bar) o;
    return Objects.equals(this.id, bar.id) &&
        Objects.equals(this.barPropA, bar.barPropA) &&
        Objects.equals(this.fooPropB, bar.fooPropB) &&
        Objects.equals(this.foo, bar.foo) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, barPropA, fooPropB, foo, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Bar {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    barPropA: ").append(toIndentedString(barPropA)).append("\n");
    sb.append("    fooPropB: ").append(toIndentedString(fooPropB)).append("\n");
    sb.append("    foo: ").append(toIndentedString(foo)).append("\n");
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
  
  public static class Builder extends Entity.Builder {

    private Bar instance;

    public Builder() {
      this(new Bar());
    }

    protected Builder(Bar instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(Bar value) { 
      super.copyOf(value);
      this.instance.setId(value.id);
      this.instance.setBarPropA(value.barPropA);
      this.instance.setFooPropB(value.fooPropB);
      this.instance.setFoo(value.foo);
      return this;
    }

    public Bar.Builder id(String id) {
      this.instance.id(id);
      return this;
    }
    
    public Bar.Builder barPropA(String barPropA) {
      this.instance.barPropA(barPropA);
      return this;
    }
    
    public Bar.Builder fooPropB(String fooPropB) {
      this.instance.fooPropB(fooPropB);
      return this;
    }
    
    public Bar.Builder foo(FooRefOrValue foo) {
      this.instance.foo(foo);
      return this;
    }
    
    @Override
    public Bar.Builder href(String href) {
      this.instance.href(href);
      return this;
    }
    
    @Override
    public Bar.Builder atSchemaLocation(String atSchemaLocation) {
      this.instance.atSchemaLocation(atSchemaLocation);
      return this;
    }
    
    @Override
    public Bar.Builder atBaseType(String atBaseType) {
      this.instance.atBaseType(atBaseType);
      return this;
    }
    
    @Override
    public Bar.Builder atType(String atType) {
      this.instance.atType(atType);
      return this;
    }
    
    /**
    * returns a built Bar instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Bar build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        super.build();
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static Bar.Builder builder() {
    return new Bar.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Bar.Builder toBuilder() {
    Bar.Builder builder = new Bar.Builder();
    return builder.copyOf(this);
  }

}

