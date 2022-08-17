package org.openapitools.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;




@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public class MapTest  implements Serializable {
  
  private @Valid Map<String, Map<String, String>> mapMapOfString = new HashMap<>();

public enum InnerEnum {

    UPPER(String.valueOf("UPPER")), LOWER(String.valueOf("lower"));


    private String value;

    InnerEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

}

  private @Valid Map<String, InnerEnum> mapOfEnumString = new HashMap<>();
  private @Valid Map<String, Boolean> directMap = new HashMap<>();
  private @Valid Map<String, Boolean> indirectMap = new HashMap<>();

  /**
   **/
  public MapTest mapMapOfString(Map<String, Map<String, String>> mapMapOfString) {
    this.mapMapOfString = mapMapOfString;
    return this;
  }

  

  
  public Map<String, Map<String, String>> getMapMapOfString() {
    return mapMapOfString;
  }

  public void setMapMapOfString(Map<String, Map<String, String>> mapMapOfString) {
    this.mapMapOfString = mapMapOfString;
  }

/**
   **/
  public MapTest mapOfEnumString(Map<String, InnerEnum> mapOfEnumString) {
    this.mapOfEnumString = mapOfEnumString;
    return this;
  }

  

  
  public Map<String, InnerEnum> getMapOfEnumString() {
    return mapOfEnumString;
  }

  public void setMapOfEnumString(Map<String, InnerEnum> mapOfEnumString) {
    this.mapOfEnumString = mapOfEnumString;
  }

/**
   **/
  public MapTest directMap(Map<String, Boolean> directMap) {
    this.directMap = directMap;
    return this;
  }

  

  
  public Map<String, Boolean> getDirectMap() {
    return directMap;
  }

  public void setDirectMap(Map<String, Boolean> directMap) {
    this.directMap = directMap;
  }

/**
   **/
  public MapTest indirectMap(Map<String, Boolean> indirectMap) {
    this.indirectMap = indirectMap;
    return this;
  }

  

  
  public Map<String, Boolean> getIndirectMap() {
    return indirectMap;
  }

  public void setIndirectMap(Map<String, Boolean> indirectMap) {
    this.indirectMap = indirectMap;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    MapTest mapTest = (MapTest) o;
    return Objects.equals(this.mapMapOfString, mapTest.mapMapOfString) &&
        Objects.equals(this.mapOfEnumString, mapTest.mapOfEnumString) &&
        Objects.equals(this.directMap, mapTest.directMap) &&
        Objects.equals(this.indirectMap, mapTest.indirectMap);
  }

  @Override
  public int hashCode() {
    return Objects.hash(mapMapOfString, mapOfEnumString, directMap, indirectMap);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MapTest {\n");
    
    sb.append("    mapMapOfString: ").append(toIndentedString(mapMapOfString)).append("\n");
    sb.append("    mapOfEnumString: ").append(toIndentedString(mapOfEnumString)).append("\n");
    sb.append("    directMap: ").append(toIndentedString(directMap)).append("\n");
    sb.append("    indirectMap: ").append(toIndentedString(indirectMap)).append("\n");
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

