package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.model.Pizza;
import java.math.BigDecimal;
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


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class PizzaSpeziale extends Pizza {

  private String toppings;

  public PizzaSpeziale() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public PizzaSpeziale(String atType) {
    super(atType);
  }

  public PizzaSpeziale toppings(String toppings) {
    this.toppings = toppings;
    return this;
  }

  /**
   * Get toppings
   * @return toppings
  */
  
  @Schema(name = "toppings", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("toppings")
  public String getToppings() {
    return toppings;
  }

  public void setToppings(String toppings) {
    this.toppings = toppings;
  }

  public PizzaSpeziale pizzaSize(BigDecimal pizzaSize) {
    super.setPizzaSize(pizzaSize);
    return this;
  }

  public PizzaSpeziale href(String href) {
    super.setHref(href);
    return this;
  }

  public PizzaSpeziale id(String id) {
    super.setId(id);
    return this;
  }

  public PizzaSpeziale atSchemaLocation(String atSchemaLocation) {
    super.setAtSchemaLocation(atSchemaLocation);
    return this;
  }

  public PizzaSpeziale atBaseType(String atBaseType) {
    super.setAtBaseType(atBaseType);
    return this;
  }

  public PizzaSpeziale atType(String atType) {
    super.setAtType(atType);
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
}

