package org.openapitools.model;

import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Map;
import java.util.HashMap;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("AdditionalPropertiesBoolean")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class AdditionalPropertiesBoolean  implements Serializable {
  private String name;

  public AdditionalPropertiesBoolean() {
  }

  /**
   **/
  public AdditionalPropertiesBoolean name(String name) {
    this.name = name;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("name")
  public String getName() {
    return name;
  }

  @JsonProperty("name")
  public void setName(String name) {
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
    return Objects.equals(this.name, additionalPropertiesBoolean.name) && Objects.equals(this.additionalProperties, additionalPropertiesBoolean.additionalProperties);
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


}

