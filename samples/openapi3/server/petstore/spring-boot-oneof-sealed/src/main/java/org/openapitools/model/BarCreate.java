package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
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
 * BarCreate
 */


@JsonTypeName("Bar_Create")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public class BarCreate extends Entity {

  private @Nullable String barPropA;

  private @Nullable String fooPropB;

  private @Nullable FooRefOrValue foo;

  public BarCreate() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public BarCreate(String atType) {
    super(atType);
  }

  public BarCreate barPropA(@Nullable String barPropA) {
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

  public BarCreate fooPropB(@Nullable String fooPropB) {
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

  public BarCreate foo(@Nullable FooRefOrValue foo) {
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


  public BarCreate href(String href) {
    super.href(href);
    return this;
  }

  public BarCreate id(String id) {
    super.id(id);
    return this;
  }

  public BarCreate atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public BarCreate atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public BarCreate atType(String atType) {
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
    BarCreate barCreate = (BarCreate) o;
    return Objects.equals(this.barPropA, barCreate.barPropA) &&
        Objects.equals(this.fooPropB, barCreate.fooPropB) &&
        Objects.equals(this.foo, barCreate.foo) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(barPropA, fooPropB, foo, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class BarCreate {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
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

    private BarCreate instance;

    public Builder() {
      this(new BarCreate());
    }

    protected Builder(BarCreate instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(BarCreate value) { 
      super.copyOf(value);
      this.instance.setBarPropA(value.barPropA);
      this.instance.setFooPropB(value.fooPropB);
      this.instance.setFoo(value.foo);
      return this;
    }

    public BarCreate.Builder barPropA(String barPropA) {
      this.instance.barPropA(barPropA);
      return this;
    }
    
    public BarCreate.Builder fooPropB(String fooPropB) {
      this.instance.fooPropB(fooPropB);
      return this;
    }
    
    public BarCreate.Builder foo(FooRefOrValue foo) {
      this.instance.foo(foo);
      return this;
    }
    
    @Override
    public BarCreate.Builder href(String href) {
      this.instance.href(href);
      return this;
    }
    
    @Override
    public BarCreate.Builder id(String id) {
      this.instance.id(id);
      return this;
    }
    
    @Override
    public BarCreate.Builder atSchemaLocation(String atSchemaLocation) {
      this.instance.atSchemaLocation(atSchemaLocation);
      return this;
    }
    
    @Override
    public BarCreate.Builder atBaseType(String atBaseType) {
      this.instance.atBaseType(atBaseType);
      return this;
    }
    
    @Override
    public BarCreate.Builder atType(String atType) {
      this.instance.atType(atType);
      return this;
    }
    
    /**
    * returns a built BarCreate instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public BarCreate build() {
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
  public static BarCreate.Builder builder() {
    return new BarCreate.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public BarCreate.Builder toBuilder() {
    BarCreate.Builder builder = new BarCreate.Builder();
    return builder.copyOf(this);
  }

}

