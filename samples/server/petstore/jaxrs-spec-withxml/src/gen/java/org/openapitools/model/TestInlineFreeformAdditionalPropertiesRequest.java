package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.HashMap;
import java.util.Map;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;



@JsonTypeName("testInlineFreeformAdditionalProperties_request")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "TestInlineFreeformAdditionalPropertiesRequest", propOrder =
    { "someProperty"
    })
    
    

public class TestInlineFreeformAdditionalPropertiesRequest extends HashMap<String, Object> implements Serializable {
  private String someProperty;

  public TestInlineFreeformAdditionalPropertiesRequest() {
  }

  /**
   **/
  public TestInlineFreeformAdditionalPropertiesRequest someProperty(String someProperty) {
    this.someProperty = someProperty;
    return this;
  }

      @XmlElement(name="someProperty")
  
  @ApiModelProperty(value = "")
  @JsonProperty("someProperty")
  public String getSomeProperty() {
    return someProperty;
  }

  @JsonProperty("someProperty")
  public void setSomeProperty(String someProperty) {
    this.someProperty = someProperty;
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
    return Objects.equals(this.someProperty, testInlineFreeformAdditionalPropertiesRequest.someProperty) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(someProperty, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestInlineFreeformAdditionalPropertiesRequest {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    someProperty: ").append(toIndentedString(someProperty)).append("\n");
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

