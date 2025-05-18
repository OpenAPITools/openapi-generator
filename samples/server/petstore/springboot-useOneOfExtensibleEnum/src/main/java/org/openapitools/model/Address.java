package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.Country;
import org.openapitools.model.CountryOrOther;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * Address
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class Address {

  private @Nullable String street;

  private @Nullable Country countryOrString;

  private @Nullable CountryOrOther countryOrOther;

  public Address street(String street) {
    this.street = street;
    return this;
  }

  /**
   * Get street
   * @return street
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("street")
  public String getStreet() {
    return street;
  }

  public void setStreet(String street) {
    this.street = street;
  }

  public Address countryOrString(Country countryOrString) {
    this.countryOrString = countryOrString;
    return this;
  }

  /**
   * Get countryOrString
   * @return countryOrString
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("countryOrString")
  public Country getCountryOrString() {
    return countryOrString;
  }

  public void setCountryOrString(Country countryOrString) {
    this.countryOrString = countryOrString;
  }

  public Address countryOrOther(CountryOrOther countryOrOther) {
    this.countryOrOther = countryOrOther;
    return this;
  }

  /**
   * Get countryOrOther
   * @return countryOrOther
   */
  @Valid 
  @ApiModelProperty(value = "")
  @JsonProperty("countryOrOther")
  public CountryOrOther getCountryOrOther() {
    return countryOrOther;
  }

  public void setCountryOrOther(CountryOrOther countryOrOther) {
    this.countryOrOther = countryOrOther;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Address address = (Address) o;
    return Objects.equals(this.street, address.street) &&
        Objects.equals(this.countryOrString, address.countryOrString) &&
        Objects.equals(this.countryOrOther, address.countryOrOther);
  }

  @Override
  public int hashCode() {
    return Objects.hash(street, countryOrString, countryOrOther);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Address {\n");
    sb.append("    street: ").append(toIndentedString(street)).append("\n");
    sb.append("    countryOrString: ").append(toIndentedString(countryOrString)).append("\n");
    sb.append("    countryOrOther: ").append(toIndentedString(countryOrOther)).append("\n");
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
  
  public static class Builder {

    private Address instance;

    public Builder() {
      this(new Address());
    }

    protected Builder(Address instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Address value) { 
      this.instance.setStreet(value.street);
      this.instance.setCountryOrString(value.countryOrString);
      this.instance.setCountryOrOther(value.countryOrOther);
      return this;
    }

    public Address.Builder street(String street) {
      this.instance.street(street);
      return this;
    }
    
    public Address.Builder countryOrString(Country countryOrString) {
      this.instance.countryOrString(countryOrString);
      return this;
    }
    
    public Address.Builder countryOrOther(CountryOrOther countryOrOther) {
      this.instance.countryOrOther(countryOrOther);
      return this;
    }
    
    /**
    * returns a built Address instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Address build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
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
  public static Address.Builder builder() {
    return new Address.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Address.Builder toBuilder() {
    Address.Builder builder = new Address.Builder();
    return builder.copyOf(this);
  }

}

