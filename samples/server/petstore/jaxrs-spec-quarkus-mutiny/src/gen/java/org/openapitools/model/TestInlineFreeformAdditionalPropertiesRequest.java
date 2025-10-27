package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
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
@JsonTypeName("testInlineFreeformAdditionalProperties_request")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class TestInlineFreeformAdditionalPropertiesRequest  implements Serializable {
  private String someProperty;

  public TestInlineFreeformAdditionalPropertiesRequest() {
  }

  /**
   **/
  public TestInlineFreeformAdditionalPropertiesRequest someProperty(String someProperty) {
    this.someProperty = someProperty;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("someProperty")
  public String getSomeProperty() {
    return someProperty;
  }

  @JsonProperty("someProperty")
  public void setSomeProperty(String someProperty) {
    this.someProperty = someProperty;
  }

      /**
      * A container for additional, undeclared properties.
      * This is a holder for any undeclared properties as specified with
      * the 'additionalProperties' keyword in the OAS document.
      */
      private Map<String, Object> additionalProperties;
  
      /**
      * Set the additional (undeclared) property with the specified name and value.
      * If the property does not already exist, create it otherwise replace it.
      */
      @JsonAnySetter
      public TestInlineFreeformAdditionalPropertiesRequest putAdditionalProperty(String key, Object value) {
          if (this.additionalProperties == null) {
              this.additionalProperties = new HashMap<String, Object>();
          }
          this.additionalProperties.put(key, value);
          return this;
      }
  
      /**
      * Return the additional (undeclared) property.
      */
      @JsonAnyGetter
      public Map<String, Object> getAdditionalProperties() {
          return additionalProperties;
      }
  
      /**
      * Return the additional (undeclared) property with the specified name.
      */
      public Object getAdditionalProperty(String key) {
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
    TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest = (TestInlineFreeformAdditionalPropertiesRequest) o;
    return Objects.equals(this.someProperty, testInlineFreeformAdditionalPropertiesRequest.someProperty) && Objects.equals(this.additionalProperties, testInlineFreeformAdditionalPropertiesRequest.additionalProperties);
  }

  @Override
  public int hashCode() {
    return Objects.hash(someProperty, additionalProperties);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestInlineFreeformAdditionalPropertiesRequest {\n");
    
    sb.append("    someProperty: ").append(toIndentedString(someProperty)).append("\n");
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

