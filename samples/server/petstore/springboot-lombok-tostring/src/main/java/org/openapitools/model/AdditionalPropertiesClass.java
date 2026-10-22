package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openapitools.jackson.nullable.JsonNullable;
import org.springframework.lang.Nullable;
import java.util.NoSuchElementException;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * AdditionalPropertiesClass
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class AdditionalPropertiesClass {

  
  @Schema(name = "map_string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, String> mapString = new HashMap<>();

  
  @Schema(name = "map_number", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map_number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, BigDecimal> mapNumber = new HashMap<>();

  
  @Schema(name = "map_integer", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map_integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Integer> mapInteger = new HashMap<>();

  
  @Schema(name = "map_boolean", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map_boolean")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Boolean> mapBoolean = new HashMap<>();

  
  @Schema(name = "map_array_integer", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map_array_integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, List<Integer>> mapArrayInteger = new HashMap<>();

  
  @Schema(name = "map_array_anytype", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map_array_anytype")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, List<Object>> mapArrayAnytype = new HashMap<>();

  
  @Schema(name = "map_map_string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map_map_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Map<String, String>> mapMapString = new HashMap<>();

  
  @Schema(name = "map_map_anytype", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("map_map_anytype")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private Map<String, Map<String, Object>> mapMapAnytype = new HashMap<>();

  
  @Schema(name = "anytype_1", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("anytype_1")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Object anytype1;

  
  @Schema(name = "anytype_2", requiredMode = Schema.RequiredMode.NOT_REQUIRED, nullable = true)
  @JsonProperty("anytype_2")
  private JsonNullable<Object> anytype2 = JsonNullable.<Object>undefined();

  
  @Schema(name = "anytype_3", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("anytype_3")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Object anytype3;

  public AdditionalPropertiesClass mapString(Map<String, String> mapString) {
    this.mapString = mapString;
    return this;
  }

  public AdditionalPropertiesClass putMapStringItem(String key, String mapStringItem) {
    if (this.mapString == null) {
      this.mapString = new HashMap<>();
    }
    this.mapString.put(key, mapStringItem);
    return this;
  }


  public AdditionalPropertiesClass mapNumber(Map<String, BigDecimal> mapNumber) {
    this.mapNumber = mapNumber;
    return this;
  }

  public AdditionalPropertiesClass putMapNumberItem(String key, BigDecimal mapNumberItem) {
    if (this.mapNumber == null) {
      this.mapNumber = new HashMap<>();
    }
    this.mapNumber.put(key, mapNumberItem);
    return this;
  }


  public AdditionalPropertiesClass mapInteger(Map<String, Integer> mapInteger) {
    this.mapInteger = mapInteger;
    return this;
  }

  public AdditionalPropertiesClass putMapIntegerItem(String key, Integer mapIntegerItem) {
    if (this.mapInteger == null) {
      this.mapInteger = new HashMap<>();
    }
    this.mapInteger.put(key, mapIntegerItem);
    return this;
  }


  public AdditionalPropertiesClass mapBoolean(Map<String, Boolean> mapBoolean) {
    this.mapBoolean = mapBoolean;
    return this;
  }

  public AdditionalPropertiesClass putMapBooleanItem(String key, Boolean mapBooleanItem) {
    if (this.mapBoolean == null) {
      this.mapBoolean = new HashMap<>();
    }
    this.mapBoolean.put(key, mapBooleanItem);
    return this;
  }


  public AdditionalPropertiesClass mapArrayInteger(Map<String, List<Integer>> mapArrayInteger) {
    this.mapArrayInteger = mapArrayInteger;
    return this;
  }

  public AdditionalPropertiesClass putMapArrayIntegerItem(String key, List<Integer> mapArrayIntegerItem) {
    if (this.mapArrayInteger == null) {
      this.mapArrayInteger = new HashMap<>();
    }
    this.mapArrayInteger.put(key, mapArrayIntegerItem);
    return this;
  }


  public AdditionalPropertiesClass mapArrayAnytype(Map<String, List<Object>> mapArrayAnytype) {
    this.mapArrayAnytype = mapArrayAnytype;
    return this;
  }

  public AdditionalPropertiesClass putMapArrayAnytypeItem(String key, List<Object> mapArrayAnytypeItem) {
    if (this.mapArrayAnytype == null) {
      this.mapArrayAnytype = new HashMap<>();
    }
    this.mapArrayAnytype.put(key, mapArrayAnytypeItem);
    return this;
  }


  public AdditionalPropertiesClass mapMapString(Map<String, Map<String, String>> mapMapString) {
    this.mapMapString = mapMapString;
    return this;
  }

  public AdditionalPropertiesClass putMapMapStringItem(String key, Map<String, String> mapMapStringItem) {
    if (this.mapMapString == null) {
      this.mapMapString = new HashMap<>();
    }
    this.mapMapString.put(key, mapMapStringItem);
    return this;
  }


  public AdditionalPropertiesClass mapMapAnytype(Map<String, Map<String, Object>> mapMapAnytype) {
    this.mapMapAnytype = mapMapAnytype;
    return this;
  }

  public AdditionalPropertiesClass putMapMapAnytypeItem(String key, Map<String, Object> mapMapAnytypeItem) {
    if (this.mapMapAnytype == null) {
      this.mapMapAnytype = new HashMap<>();
    }
    this.mapMapAnytype.put(key, mapMapAnytypeItem);
    return this;
  }


  public AdditionalPropertiesClass anytype1(@Nullable Object anytype1) {
    this.anytype1 = anytype1;
    return this;
  }


  public AdditionalPropertiesClass anytype2(Object anytype2) {
    this.anytype2 = JsonNullable.of(anytype2);
    return this;
  }


  public AdditionalPropertiesClass anytype3(@Nullable Object anytype3) {
    this.anytype3 = anytype3;
    return this;
  }



}

