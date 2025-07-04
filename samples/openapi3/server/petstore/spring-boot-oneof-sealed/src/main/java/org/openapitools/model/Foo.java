package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.model.Entity;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * Foo
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public final class Foo extends Entity implements FooRefOrValue {

  private @Nullable String fooPropA;

  private @Nullable String fooPropB;

  public Foo() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Foo(String atType) {
    super(atType);
  }

  public Foo fooPropA(@Nullable String fooPropA) {
    this.fooPropA = fooPropA;
    return this;
  }

  /**
   * Get fooPropA
   * @return fooPropA
   */
  
  @Schema(name = "fooPropA", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("fooPropA")
  public @Nullable String getFooPropA() {
    return fooPropA;
  }

  public void setFooPropA(@Nullable String fooPropA) {
    this.fooPropA = fooPropA;
  }

  public Foo fooPropB(@Nullable String fooPropB) {
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


  public Foo href(String href) {
    super.href(href);
    return this;
  }

  public Foo id(String id) {
    super.id(id);
    return this;
  }

  public Foo atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public Foo atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public Foo atType(String atType) {
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
    Foo foo = (Foo) o;
    return Objects.equals(this.fooPropA, foo.fooPropA) &&
        Objects.equals(this.fooPropB, foo.fooPropB) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(fooPropA, fooPropB, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Foo {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    fooPropA: ").append(toIndentedString(fooPropA)).append("\n");
    sb.append("    fooPropB: ").append(toIndentedString(fooPropB)).append("\n");
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

    private Foo instance;

    public Builder() {
      this(new Foo());
    }

    protected Builder(Foo instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(Foo value) { 
      super.copyOf(value);
      this.instance.setFooPropA(value.fooPropA);
      this.instance.setFooPropB(value.fooPropB);
      return this;
    }

    public Foo.Builder fooPropA(String fooPropA) {
      this.instance.fooPropA(fooPropA);
      return this;
    }
    
    public Foo.Builder fooPropB(String fooPropB) {
      this.instance.fooPropB(fooPropB);
      return this;
    }
    
    @Override
    public Foo.Builder href(String href) {
      this.instance.href(href);
      return this;
    }
    
    @Override
    public Foo.Builder id(String id) {
      this.instance.id(id);
      return this;
    }
    
    @Override
    public Foo.Builder atSchemaLocation(String atSchemaLocation) {
      this.instance.atSchemaLocation(atSchemaLocation);
      return this;
    }
    
    @Override
    public Foo.Builder atBaseType(String atBaseType) {
      this.instance.atBaseType(atBaseType);
      return this;
    }
    
    @Override
    public Foo.Builder atType(String atType) {
      this.instance.atType(atType);
      return this;
    }
    
    /**
    * returns a built Foo instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Foo build() {
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
  public static Foo.Builder builder() {
    return new Foo.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Foo.Builder toBuilder() {
    Foo.Builder builder = new Foo.Builder();
    return builder.copyOf(this);
  }

}

