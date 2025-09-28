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
 * AdditionalPropertiesBoolean
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class AdditionalPropertiesBoolean {

  private Optional<String> name = Optional.empty();

  public AdditionalPropertiesBoolean name(String name) {
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
    private Map<String, Boolean> additionalProperties;

    /**
    * Set the additional (undeclared) property with the specified name and value.
    * If the property does not already exist, create it otherwise replace it.
    */
    @JsonAnySetter
    public AdditionalPropertiesBoolean putAdditionalProperty(String key, Boolean value) {
        if (this.additionalProperties == null) {
            this.additionalProperties = new HashMap<String, Boolean>();
        }
        this.additionalProperties.put(key, value);
        return this;
    }

    /**
    * Return the additional (undeclared) property.
    */
    @JsonAnyGetter
    public Map<String, Boolean> getAdditionalProperties() {
        return additionalProperties;
    }

    /**
    * Return the additional (undeclared) property with the specified name.
    */
    public Boolean getAdditionalProperty(String key) {
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
    AdditionalPropertiesBoolean additionalPropertiesBoolean = (AdditionalPropertiesBoolean) o;
    return Objects.equals(this.name, additionalPropertiesBoolean.name) &&
    Objects.equals(this.additionalProperties, additionalPropertiesBoolean.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, additionalProperties);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertiesBoolean {\n");
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

    private AdditionalPropertiesBoolean instance;

    public Builder() {
      this(new AdditionalPropertiesBoolean());
    }

    protected Builder(AdditionalPropertiesBoolean instance) {
      this.instance = instance;
    }

    protected Builder copyOf(AdditionalPropertiesBoolean value) { 
      this.instance.setName(value.name);
      return this;
    }

    public AdditionalPropertiesBoolean.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    public AdditionalPropertiesBoolean.Builder additionalProperties(Map<String, Boolean> additionalProperties) {
      this.instance.additionalProperties = additionalProperties;
      return this;
    }

    /**
    * returns a built AdditionalPropertiesBoolean instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public AdditionalPropertiesBoolean build() {
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
  public static AdditionalPropertiesBoolean.Builder builder() {
    return new AdditionalPropertiesBoolean.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public AdditionalPropertiesBoolean.Builder toBuilder() {
    AdditionalPropertiesBoolean.Builder builder = new AdditionalPropertiesBoolean.Builder();
    return builder.copyOf(this);
  }

}

