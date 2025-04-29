package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

import java.util.Map;
import java.util.HashMap;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
/**
 * AdditionalPropertiesInteger
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class AdditionalPropertiesInteger {

  private Optional<String> name = Optional.empty();

  public AdditionalPropertiesInteger name(String name) {
    this.name = Optional.ofNullable(name);
    return this;
  }

  /**
   * Get name
   * @return name
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("name")
  public Optional<String> getName() {
    return name;
  }

  public void setName(Optional<String> name) {
    this.name = name;
  }
    /**
    * A container for additional, undeclared properties.
    * This is a holder for any undeclared properties as specified with
    * the 'additionalProperties' keyword in the OAS document.
    */
    private Map<String, Integer> additionalProperties;

    /**
    * Set the additional (undeclared) property with the specified name and value.
    * If the property does not already exist, create it otherwise replace it.
    */
    @JsonAnySetter
    public AdditionalPropertiesInteger putAdditionalProperty(String key, Integer value) {
        if (this.additionalProperties == null) {
            this.additionalProperties = new HashMap<String, Integer>();
        }
        this.additionalProperties.put(key, value);
        return this;
    }

    /**
    * Return the additional (undeclared) property.
    */
    @JsonAnyGetter
    public Map<String, Integer> getAdditionalProperties() {
        return additionalProperties;
    }

    /**
    * Return the additional (undeclared) property with the specified name.
    */
    public Integer getAdditionalProperty(String key) {
        if (this.additionalProperties == null) {
            return null;
        }
        return this.additionalProperties.get(key);
    }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    AdditionalPropertiesInteger additionalPropertiesInteger = (AdditionalPropertiesInteger) o;
    return Objects.equals(this.name, additionalPropertiesInteger.name) &&
    Objects.equals(this.additionalProperties, additionalPropertiesInteger.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, additionalProperties);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertiesInteger {\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    
    sb.append("    additionalProperties: ").append(toIndentedString(additionalProperties)).append("\n");
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

    private AdditionalPropertiesInteger instance;

    public Builder() {
      this(new AdditionalPropertiesInteger());
    }

    protected Builder(AdditionalPropertiesInteger instance) {
      this.instance = instance;
    }

    protected Builder copyOf(AdditionalPropertiesInteger value) { 
      this.instance.setName(value.name);
      return this;
    }

    public AdditionalPropertiesInteger.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    public AdditionalPropertiesInteger.Builder additionalProperties(Map<String, Integer> additionalProperties) {
      this.instance.additionalProperties = additionalProperties;
      return this;
    }

    /**
    * returns a built AdditionalPropertiesInteger instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public AdditionalPropertiesInteger build() {
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
  public static AdditionalPropertiesInteger.Builder builder() {
    return new AdditionalPropertiesInteger.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public AdditionalPropertiesInteger.Builder toBuilder() {
    AdditionalPropertiesInteger.Builder builder = new AdditionalPropertiesInteger.Builder();
    return builder.copyOf(this);
  }

}

