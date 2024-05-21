package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Map;
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
 * AdditionalPropertiesObject
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.7.0-SNAPSHOT")
public class AdditionalPropertiesObject {

  private Optional<String> name = Optional.empty();

  public AdditionalPropertiesObject name(String name) {
    this.name = Optional.of(name);
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
    private Map<String, Map> additionalProperties;

    /**
    * Set the additional (undeclared) property with the specified name and value.
    * If the property does not already exist, create it otherwise replace it.
    */
    @JsonAnySetter
    public AdditionalPropertiesObject putAdditionalProperty(String key, Map value) {
        if (this.additionalProperties == null) {
            this.additionalProperties = new HashMap<String, Map>();
        }
        this.additionalProperties.put(key, value);
        return this;
    }

    /**
    * Return the additional (undeclared) property.
    */
    @JsonAnyGetter
    public Map<String, Map> getAdditionalProperties() {
        return additionalProperties;
    }

    /**
    * Return the additional (undeclared) property with the specified name.
    */
    public Map getAdditionalProperty(String key) {
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
    AdditionalPropertiesObject additionalPropertiesObject = (AdditionalPropertiesObject) o;
    return Objects.equals(this.name, additionalPropertiesObject.name) &&
    Objects.equals(this.additionalProperties, additionalPropertiesObject.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, additionalProperties);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertiesObject {\n");
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

    private AdditionalPropertiesObject instance;

    public Builder() {
      this(new AdditionalPropertiesObject());
    }

    protected Builder(AdditionalPropertiesObject instance) {
      this.instance = instance;
    }

    protected Builder copyOf(AdditionalPropertiesObject value) { 
      this.instance.setName(value.name);
      return this;
    }

    public AdditionalPropertiesObject.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    public AdditionalPropertiesObject.Builder additionalProperties(Map<String, Map> additionalProperties) {
      this.instance.additionalProperties = additionalProperties;
      return this;
    }

    /**
    * returns a built AdditionalPropertiesObject instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public AdditionalPropertiesObject build() {
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
  public static AdditionalPropertiesObject.Builder builder() {
    return new AdditionalPropertiesObject.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public AdditionalPropertiesObject.Builder toBuilder() {
    AdditionalPropertiesObject.Builder builder = new AdditionalPropertiesObject.Builder();
    return builder.copyOf(this);
  }

}

