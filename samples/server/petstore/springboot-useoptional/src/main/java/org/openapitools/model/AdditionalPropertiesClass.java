package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openapitools.jackson.nullable.JsonNullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * AdditionalPropertiesClass
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.10.0-SNAPSHOT")
public class AdditionalPropertiesClass {

  private Map<String, Optional<String>> mapString = new HashMap<>();

  private Map<String, Optional<BigDecimal>> mapNumber = new HashMap<>();

  private Map<String, Optional<Integer>> mapInteger = new HashMap<>();

  private Map<String, Optional<Boolean>> mapBoolean = new HashMap<>();

  private Map<String, List<Optional<Integer>>> mapArrayInteger = new HashMap<>();

  private Map<String, List<Optional<Object>>> mapArrayAnytype = new HashMap<>();

  private Map<String, Map<String, Optional<String>>> mapMapString = new HashMap<>();

  private Map<String, Map<String, Optional<Object>>> mapMapAnytype = new HashMap<>();

  private Optional<Object> anytype1 = Optional.empty();

  private JsonNullable<Object> anytype2 = JsonNullable.of(null);

  private Optional<Object> anytype3 = Optional.empty();

  public AdditionalPropertiesClass mapString(Map<String, Optional<String>> mapString) {
    this.mapString = mapString;
    return this;
  }

  public AdditionalPropertiesClass putMapStringItem(String key, Optional<String> mapStringItem) {
    if (this.mapString == null) {
      this.mapString = new HashMap<>();
    }
    this.mapString.put(key, mapStringItem);
    return this;
  }

  /**
   * Get mapString
   * @return mapString
   */
  @ApiModelProperty(value = "")
  @JsonProperty("map_string")
  public Map<String, Optional<String>> getMapString() {
    return mapString;
  }

  public void setMapString(Map<String, Optional<String>> mapString) {
    this.mapString = mapString;
  }

  public AdditionalPropertiesClass mapNumber(Map<String, Optional<BigDecimal>> mapNumber) {
    this.mapNumber = mapNumber;
    return this;
  }

  public AdditionalPropertiesClass putMapNumberItem(String key, Optional<BigDecimal> mapNumberItem) {
    if (this.mapNumber == null) {
      this.mapNumber = new HashMap<>();
    }
    this.mapNumber.put(key, mapNumberItem);
    return this;
  }

  /**
   * Get mapNumber
   * @return mapNumber
   */
  @ApiModelProperty(value = "")
  @JsonProperty("map_number")
  public Map<String, Optional<BigDecimal>> getMapNumber() {
    return mapNumber;
  }

  public void setMapNumber(Map<String, Optional<BigDecimal>> mapNumber) {
    this.mapNumber = mapNumber;
  }

  public AdditionalPropertiesClass mapInteger(Map<String, Optional<Integer>> mapInteger) {
    this.mapInteger = mapInteger;
    return this;
  }

  public AdditionalPropertiesClass putMapIntegerItem(String key, Optional<Integer> mapIntegerItem) {
    if (this.mapInteger == null) {
      this.mapInteger = new HashMap<>();
    }
    this.mapInteger.put(key, mapIntegerItem);
    return this;
  }

  /**
   * Get mapInteger
   * @return mapInteger
   */
  @ApiModelProperty(value = "")
  @JsonProperty("map_integer")
  public Map<String, Optional<Integer>> getMapInteger() {
    return mapInteger;
  }

  public void setMapInteger(Map<String, Optional<Integer>> mapInteger) {
    this.mapInteger = mapInteger;
  }

  public AdditionalPropertiesClass mapBoolean(Map<String, Optional<Boolean>> mapBoolean) {
    this.mapBoolean = mapBoolean;
    return this;
  }

  public AdditionalPropertiesClass putMapBooleanItem(String key, Optional<Boolean> mapBooleanItem) {
    if (this.mapBoolean == null) {
      this.mapBoolean = new HashMap<>();
    }
    this.mapBoolean.put(key, mapBooleanItem);
    return this;
  }

  /**
   * Get mapBoolean
   * @return mapBoolean
   */
  @ApiModelProperty(value = "")
  @JsonProperty("map_boolean")
  public Map<String, Optional<Boolean>> getMapBoolean() {
    return mapBoolean;
  }

  public void setMapBoolean(Map<String, Optional<Boolean>> mapBoolean) {
    this.mapBoolean = mapBoolean;
  }

  public AdditionalPropertiesClass mapArrayInteger(Map<String, List<Optional<Integer>>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
    return this;
  }

  public AdditionalPropertiesClass putMapArrayIntegerItem(String key, List<Optional<Integer>> mapArrayIntegerItem) {
    if (this.mapArrayInteger == null) {
      this.mapArrayInteger = new HashMap<>();
    }
    this.mapArrayInteger.put(key, mapArrayIntegerItem);
    return this;
  }

  /**
   * Get mapArrayInteger
   * @return mapArrayInteger
   */
  @ApiModelProperty(value = "")
  @JsonProperty("map_array_integer")
  public Map<String, List<Optional<Integer>>> getMapArrayInteger() {
    return mapArrayInteger;
  }

  public void setMapArrayInteger(Map<String, List<Optional<Integer>>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
  }

  public AdditionalPropertiesClass mapArrayAnytype(Map<String, List<Optional<Object>>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
    return this;
  }

  public AdditionalPropertiesClass putMapArrayAnytypeItem(String key, List<Optional<Object>> mapArrayAnytypeItem) {
    if (this.mapArrayAnytype == null) {
      this.mapArrayAnytype = new HashMap<>();
    }
    this.mapArrayAnytype.put(key, mapArrayAnytypeItem);
    return this;
  }

  /**
   * Get mapArrayAnytype
   * @return mapArrayAnytype
   */
  @ApiModelProperty(value = "")
  @JsonProperty("map_array_anytype")
  public Map<String, List<Optional<Object>>> getMapArrayAnytype() {
    return mapArrayAnytype;
  }

  public void setMapArrayAnytype(Map<String, List<Optional<Object>>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
  }

  public AdditionalPropertiesClass mapMapString(Map<String, Map<String, Optional<String>>> mapMapString) {
    this.mapMapString = mapMapString;
    return this;
  }

  public AdditionalPropertiesClass putMapMapStringItem(String key, Map<String, Optional<String>> mapMapStringItem) {
    if (this.mapMapString == null) {
      this.mapMapString = new HashMap<>();
    }
    this.mapMapString.put(key, mapMapStringItem);
    return this;
  }

  /**
   * Get mapMapString
   * @return mapMapString
   */
  @ApiModelProperty(value = "")
  @JsonProperty("map_map_string")
  public Map<String, Map<String, Optional<String>>> getMapMapString() {
    return mapMapString;
  }

  public void setMapMapString(Map<String, Map<String, Optional<String>>> mapMapString) {
    this.mapMapString = mapMapString;
  }

  public AdditionalPropertiesClass mapMapAnytype(Map<String, Map<String, Optional<Object>>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
    return this;
  }

  public AdditionalPropertiesClass putMapMapAnytypeItem(String key, Map<String, Optional<Object>> mapMapAnytypeItem) {
    if (this.mapMapAnytype == null) {
      this.mapMapAnytype = new HashMap<>();
    }
    this.mapMapAnytype.put(key, mapMapAnytypeItem);
    return this;
  }

  /**
   * Get mapMapAnytype
   * @return mapMapAnytype
   */
  @ApiModelProperty(value = "")
  @JsonProperty("map_map_anytype")
  public Map<String, Map<String, Optional<Object>>> getMapMapAnytype() {
    return mapMapAnytype;
  }

  public void setMapMapAnytype(Map<String, Map<String, Optional<Object>>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
  }

  public AdditionalPropertiesClass anytype1(Optional<Object> anytype1) {
    this.anytype1 = anytype1;
    return this;
  }

  /**
   * Get anytype1
   * @return anytype1
   */
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_1")
  public Optional<Object> getAnytype1() {
    return anytype1;
  }

  public void setAnytype1(Optional<Object> anytype1) {
    this.anytype1 = anytype1;
  }

  public AdditionalPropertiesClass anytype2(JsonNullable<Object> anytype2) {
    this.anytype2 = anytype2;
    return this;
  }

  /**
   * Get anytype2
   * @return anytype2
   */
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_2")
  public JsonNullable<Object> getAnytype2() {
    return anytype2;
  }

  public void setAnytype2(JsonNullable<Object> anytype2) {
    this.anytype2 = anytype2;
  }

  public AdditionalPropertiesClass anytype3(Optional<Object> anytype3) {
    this.anytype3 = anytype3;
    return this;
  }

  /**
   * Get anytype3
   * @return anytype3
   */
  @ApiModelProperty(value = "")
  @JsonProperty("anytype_3")
  public Optional<Object> getAnytype3() {
    return anytype3;
  }

  public void setAnytype3(Optional<Object> anytype3) {
    this.anytype3 = anytype3;
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
    return Objects.equals(this.mapString, additionalPropertiesClass.mapString) &&
        Objects.equals(this.mapNumber, additionalPropertiesClass.mapNumber) &&
        Objects.equals(this.mapInteger, additionalPropertiesClass.mapInteger) &&
        Objects.equals(this.mapBoolean, additionalPropertiesClass.mapBoolean) &&
        Objects.equals(this.mapArrayInteger, additionalPropertiesClass.mapArrayInteger) &&
        Objects.equals(this.mapArrayAnytype, additionalPropertiesClass.mapArrayAnytype) &&
        Objects.equals(this.mapMapString, additionalPropertiesClass.mapMapString) &&
        Objects.equals(this.mapMapAnytype, additionalPropertiesClass.mapMapAnytype) &&
        Objects.equals(this.anytype1, additionalPropertiesClass.anytype1) &&
        equalsNullable(this.anytype2, additionalPropertiesClass.anytype2) &&
        Objects.equals(this.anytype3, additionalPropertiesClass.anytype3);
  }

  private static <T> boolean equalsNullable(JsonNullable<T> a, JsonNullable<T> b) {
    return a == b || (a != null && b != null && a.isPresent() && b.isPresent() && Objects.deepEquals(a.get(), b.get()));
  }

  @Override
  public int hashCode() {
    return Objects.hash(mapString, mapNumber, mapInteger, mapBoolean, mapArrayInteger, mapArrayAnytype, mapMapString, mapMapAnytype, anytype1, hashCodeNullable(anytype2), anytype3);
  }

  private static <T> int hashCodeNullable(JsonNullable<T> a) {
    if (a == null) {
      return 1;
    }
    return a.isPresent() ? Arrays.deepHashCode(new Object[]{a.get()}) : 31;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class AdditionalPropertiesClass {\n");
    sb.append("    mapString: ").append(toIndentedString(mapString)).append("\n");
    sb.append("    mapNumber: ").append(toIndentedString(mapNumber)).append("\n");
    sb.append("    mapInteger: ").append(toIndentedString(mapInteger)).append("\n");
    sb.append("    mapBoolean: ").append(toIndentedString(mapBoolean)).append("\n");
    sb.append("    mapArrayInteger: ").append(toIndentedString(mapArrayInteger)).append("\n");
    sb.append("    mapArrayAnytype: ").append(toIndentedString(mapArrayAnytype)).append("\n");
    sb.append("    mapMapString: ").append(toIndentedString(mapMapString)).append("\n");
    sb.append("    mapMapAnytype: ").append(toIndentedString(mapMapAnytype)).append("\n");
    sb.append("    anytype1: ").append(toIndentedString(anytype1)).append("\n");
    sb.append("    anytype2: ").append(toIndentedString(anytype2)).append("\n");
    sb.append("    anytype3: ").append(toIndentedString(anytype3)).append("\n");
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

    private AdditionalPropertiesClass instance;

    public Builder() {
      this(new AdditionalPropertiesClass());
    }

    protected Builder(AdditionalPropertiesClass instance) {
      this.instance = instance;
    }

    protected Builder copyOf(AdditionalPropertiesClass value) { 
      this.instance.setMapString(value.mapString);
      this.instance.setMapNumber(value.mapNumber);
      this.instance.setMapInteger(value.mapInteger);
      this.instance.setMapBoolean(value.mapBoolean);
      this.instance.setMapArrayInteger(value.mapArrayInteger);
      this.instance.setMapArrayAnytype(value.mapArrayAnytype);
      this.instance.setMapMapString(value.mapMapString);
      this.instance.setMapMapAnytype(value.mapMapAnytype);
      this.instance.setAnytype1(value.anytype1);
      this.instance.setAnytype2(value.anytype2);
      this.instance.setAnytype3(value.anytype3);
      return this;
    }

    public AdditionalPropertiesClass.Builder mapString(Map<String, Optional<String>> mapString) {
      this.instance.mapString(mapString);
      return this;
    }
    public AdditionalPropertiesClass.Builder mapNumber(Map<String, Optional<BigDecimal>> mapNumber) {
      this.instance.mapNumber(mapNumber);
      return this;
    }
    public AdditionalPropertiesClass.Builder mapInteger(Map<String, Optional<Integer>> mapInteger) {
      this.instance.mapInteger(mapInteger);
      return this;
    }
    public AdditionalPropertiesClass.Builder mapBoolean(Map<String, Optional<Boolean>> mapBoolean) {
      this.instance.mapBoolean(mapBoolean);
      return this;
    }
    public AdditionalPropertiesClass.Builder mapArrayInteger(Map<String, List<Optional<Integer>>> mapArrayInteger) {
      this.instance.mapArrayInteger(mapArrayInteger);
      return this;
    }
    public AdditionalPropertiesClass.Builder mapArrayAnytype(Map<String, List<Optional<Object>>> mapArrayAnytype) {
      this.instance.mapArrayAnytype(mapArrayAnytype);
      return this;
    }
    public AdditionalPropertiesClass.Builder mapMapString(Map<String, Map<String, Optional<String>>> mapMapString) {
      this.instance.mapMapString(mapMapString);
      return this;
    }
    public AdditionalPropertiesClass.Builder mapMapAnytype(Map<String, Map<String, Optional<Object>>> mapMapAnytype) {
      this.instance.mapMapAnytype(mapMapAnytype);
      return this;
    }
    public AdditionalPropertiesClass.Builder anytype1(Optional<Object> anytype1) {
      this.instance.anytype1(anytype1);
      return this;
    }
    public AdditionalPropertiesClass.Builder anytype2(JsonNullable<Object> anytype2) {
      this.instance.anytype2(anytype2);
      return this;
    }
    public AdditionalPropertiesClass.Builder anytype3(Optional<Object> anytype3) {
      this.instance.anytype3(anytype3);
      return this;
    }
    /**
    * returns a built AdditionalPropertiesClass instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public AdditionalPropertiesClass build() {
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
  public static AdditionalPropertiesClass.Builder builder() {
    return new AdditionalPropertiesClass.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public AdditionalPropertiesClass.Builder toBuilder() {
    AdditionalPropertiesClass.Builder builder = new AdditionalPropertiesClass.Builder();
    return builder.copyOf(this);
  }

}
