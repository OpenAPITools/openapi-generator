package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import java.math.BigDecimal;
import org.openapitools.model.Pizza;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * PizzaSpeziale
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.15.0-SNAPSHOT")
public class PizzaSpeziale extends Pizza {

  private @Nullable String toppings;

  public PizzaSpeziale() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public PizzaSpeziale(String atType) {
    super(atType);
  }

  public PizzaSpeziale toppings(@Nullable String toppings) {
    this.toppings = toppings;
    return this;
  }

  /**
   * Get toppings
   * @return toppings
   */
  
  @Schema(name = "toppings", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("toppings")
  public @Nullable String getToppings() {
    return toppings;
  }

  public void setToppings(@Nullable String toppings) {
    this.toppings = toppings;
  }


  public PizzaSpeziale pizzaSize(BigDecimal pizzaSize) {
    super.pizzaSize(pizzaSize);
    return this;
  }

  public PizzaSpeziale href(String href) {
    super.href(href);
    return this;
  }

  public PizzaSpeziale id(String id) {
    super.id(id);
    return this;
  }

  public PizzaSpeziale atSchemaLocation(String atSchemaLocation) {
    super.atSchemaLocation(atSchemaLocation);
    return this;
  }

  public PizzaSpeziale atBaseType(String atBaseType) {
    super.atBaseType(atBaseType);
    return this;
  }

  public PizzaSpeziale atType(String atType) {
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
    PizzaSpeziale pizzaSpeziale = (PizzaSpeziale) o;
    return Objects.equals(this.toppings, pizzaSpeziale.toppings) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(toppings, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PizzaSpeziale {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    toppings: ").append(toIndentedString(toppings)).append("\n");
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
  
  public static class Builder extends Pizza.Builder {

    private PizzaSpeziale instance;

    public Builder() {
      this(new PizzaSpeziale());
    }

    protected Builder(PizzaSpeziale instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(PizzaSpeziale value) { 
      super.copyOf(value);
      this.instance.setToppings(value.toppings);
      return this;
    }

    public PizzaSpeziale.Builder toppings(String toppings) {
      this.instance.toppings(toppings);
      return this;
    }
    
    @Override
    public PizzaSpeziale.Builder pizzaSize(BigDecimal pizzaSize) {
      this.instance.pizzaSize(pizzaSize);
      return this;
    }
    
    @Override
    public PizzaSpeziale.Builder href(String href) {
      this.instance.href(href);
      return this;
    }
    
    @Override
    public PizzaSpeziale.Builder id(String id) {
      this.instance.id(id);
      return this;
    }
    
    @Override
    public PizzaSpeziale.Builder atSchemaLocation(String atSchemaLocation) {
      this.instance.atSchemaLocation(atSchemaLocation);
      return this;
    }
    
    @Override
    public PizzaSpeziale.Builder atBaseType(String atBaseType) {
      this.instance.atBaseType(atBaseType);
      return this;
    }
    
    @Override
    public PizzaSpeziale.Builder atType(String atType) {
      this.instance.atType(atType);
      return this;
    }
    
    /**
    * returns a built PizzaSpeziale instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public PizzaSpeziale build() {
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
  public static PizzaSpeziale.Builder builder() {
    return new PizzaSpeziale.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public PizzaSpeziale.Builder toBuilder() {
    PizzaSpeziale.Builder builder = new PizzaSpeziale.Builder();
    return builder.copyOf(this);
  }

}

