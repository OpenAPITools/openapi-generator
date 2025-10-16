package org.openapitools.model;

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



@JsonTypeName("AdditionalPropertiesClass")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")    @XmlAccessorType(XmlAccessType.FIELD)
     @XmlType(name = "AdditionalPropertiesClass", propOrder =
    { "mapProperty", "mapOfMapProperty"
    })
    
    @XmlRootElement(name="AdditionalPropertiesClass")

public class AdditionalPropertiesClass  implements Serializable {
  private @Valid Map<String, String> mapProperty = new HashMap<>();
  private @Valid Map<String, Map<String, String>> mapOfMapProperty = new HashMap<>();

  protected AdditionalPropertiesClass(AdditionalPropertiesClassBuilder<?, ?> b) {
    this.mapProperty = b.mapProperty;
    this.mapOfMapProperty = b.mapOfMapProperty;
  }

  public AdditionalPropertiesClass() {
  }

  /**
   **/
  public AdditionalPropertiesClass mapProperty(Map<String, String> mapProperty) {
    this.mapProperty = mapProperty;
    return this;
  }

      @XmlElement(name="map_property")
  
  @ApiModelProperty(value = "")
  @JsonProperty("map_property")
  public Map<String, String> getMapProperty() {
    return mapProperty;
  }

  @JsonProperty("map_property")
  public void setMapProperty(Map<String, String> mapProperty) {
    this.mapProperty = mapProperty;
  }

  public AdditionalPropertiesClass putMapPropertyItem(String key, String mapPropertyItem) {
    if (this.mapProperty == null) {
      this.mapProperty = new HashMap<>();
    }

    this.mapProperty.put(key, mapPropertyItem);
    return this;
  }

  public AdditionalPropertiesClass removeMapPropertyItem(String key) {
    if (this.mapProperty != null) {
      this.mapProperty.remove(key);
    }

    return this;
  }
  /**
   **/
  public AdditionalPropertiesClass mapOfMapProperty(Map<String, Map<String, String>> mapOfMapProperty) {
    this.mapOfMapProperty = mapOfMapProperty;
    return this;
  }

      @XmlElement(name="map_of_map_property")
  
  @ApiModelProperty(value = "")
  @JsonProperty("map_of_map_property")
  public Map<String, Map<String, String>> getMapOfMapProperty() {
    return mapOfMapProperty;
  }

  @JsonProperty("map_of_map_property")
  public void setMapOfMapProperty(Map<String, Map<String, String>> mapOfMapProperty) {
    this.mapOfMapProperty = mapOfMapProperty;
  }

  public AdditionalPropertiesClass putMapOfMapPropertyItem(String key, Map<String, String> mapOfMapPropertyItem) {
    if (this.mapOfMapProperty == null) {
      this.mapOfMapProperty = new HashMap<>();
    }

    this.mapOfMapProperty.put(key, mapOfMapPropertyItem);
    return this;
  }

  public AdditionalPropertiesClass removeMapOfMapPropertyItem(String key) {
    if (this.mapOfMapProperty != null) {
      this.mapOfMapProperty.remove(key);
    }

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
    AdditionalPropertiesClass additionalPropertiesClass = (AdditionalPropertiesClass) o;
    return Objects.equals(this.mapProperty, additionalPropertiesClass.mapProperty) &&
        Objects.equals(this.mapOfMapProperty, additionalPropertiesClass.mapOfMapProperty);
  }

  @Override
  public int hashCode() {
    return Objects.hash(mapProperty, mapOfMapProperty);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertiesClass {\n");
    
    sb.append("    mapProperty: ").append(toIndentedString(mapProperty)).append("\n");
    sb.append("    mapOfMapProperty: ").append(toIndentedString(mapOfMapProperty)).append("\n");
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


  public static AdditionalPropertiesClassBuilder<?, ?> builder() {
    return new AdditionalPropertiesClassBuilderImpl();
  }

  private static final class AdditionalPropertiesClassBuilderImpl extends AdditionalPropertiesClassBuilder<AdditionalPropertiesClass, AdditionalPropertiesClassBuilderImpl> {

    @Override
    protected AdditionalPropertiesClassBuilderImpl self() {
      return this;
    }

    @Override
    public AdditionalPropertiesClass build() {
      return new AdditionalPropertiesClass(this);
    }
  }

  public static abstract class AdditionalPropertiesClassBuilder<C extends AdditionalPropertiesClass, B extends AdditionalPropertiesClassBuilder<C, B>>  {
    private Map<String, String> mapProperty = new HashMap<>();
    private Map<String, Map<String, String>> mapOfMapProperty = new HashMap<>();
    protected abstract B self();

    public abstract C build();

    public B mapProperty(Map<String, String> mapProperty) {
      this.mapProperty = mapProperty;
      return self();
    }
    public B mapOfMapProperty(Map<String, Map<String, String>> mapOfMapProperty) {
      this.mapOfMapProperty = mapOfMapProperty;
      return self();
    }
  }
}

