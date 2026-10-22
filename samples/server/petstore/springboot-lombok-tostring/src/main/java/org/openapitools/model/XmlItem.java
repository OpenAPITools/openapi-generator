package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import org.hibernate.validator.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * XmlItem
 */
@lombok.Getter
@lombok.Setter
@lombok.ToString
@lombok.EqualsAndHashCode

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
public class XmlItem {

  
  @Schema(name = "attribute_string", example = "string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("attribute_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String attributeString;

  @Valid 
  @Schema(name = "attribute_number", example = "1.234", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("attribute_number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal attributeNumber;

  
  @Schema(name = "attribute_integer", example = "-2", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("attribute_integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer attributeInteger;

  
  @Schema(name = "attribute_boolean", example = "true", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("attribute_boolean")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Boolean attributeBoolean;

  
  @Schema(name = "wrapped_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("wrapped_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> wrappedArray = new ArrayList<>();

  
  @Schema(name = "name_string", example = "string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("name_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String nameString;

  @Valid 
  @Schema(name = "name_number", example = "1.234", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("name_number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal nameNumber;

  
  @Schema(name = "name_integer", example = "-2", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("name_integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer nameInteger;

  
  @Schema(name = "name_boolean", example = "true", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("name_boolean")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Boolean nameBoolean;

  
  @Schema(name = "name_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("name_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> nameArray = new ArrayList<>();

  
  @Schema(name = "name_wrapped_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("name_wrapped_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> nameWrappedArray = new ArrayList<>();

  
  @Schema(name = "prefix_string", example = "string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String prefixString;

  @Valid 
  @Schema(name = "prefix_number", example = "1.234", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal prefixNumber;

  
  @Schema(name = "prefix_integer", example = "-2", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer prefixInteger;

  
  @Schema(name = "prefix_boolean", example = "true", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_boolean")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Boolean prefixBoolean;

  
  @Schema(name = "prefix_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> prefixArray = new ArrayList<>();

  
  @Schema(name = "prefix_wrapped_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_wrapped_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> prefixWrappedArray = new ArrayList<>();

  
  @Schema(name = "namespace_string", example = "string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("namespace_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String namespaceString;

  @Valid 
  @Schema(name = "namespace_number", example = "1.234", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("namespace_number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal namespaceNumber;

  
  @Schema(name = "namespace_integer", example = "-2", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("namespace_integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer namespaceInteger;

  
  @Schema(name = "namespace_boolean", example = "true", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("namespace_boolean")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Boolean namespaceBoolean;

  
  @Schema(name = "namespace_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("namespace_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> namespaceArray = new ArrayList<>();

  
  @Schema(name = "namespace_wrapped_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("namespace_wrapped_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> namespaceWrappedArray = new ArrayList<>();

  
  @Schema(name = "prefix_ns_string", example = "string", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_ns_string")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable String prefixNsString;

  @Valid 
  @Schema(name = "prefix_ns_number", example = "1.234", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_ns_number")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable BigDecimal prefixNsNumber;

  
  @Schema(name = "prefix_ns_integer", example = "-2", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_ns_integer")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Integer prefixNsInteger;

  
  @Schema(name = "prefix_ns_boolean", example = "true", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_ns_boolean")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private @Nullable Boolean prefixNsBoolean;

  
  @Schema(name = "prefix_ns_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_ns_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> prefixNsArray = new ArrayList<>();

  
  @Schema(name = "prefix_ns_wrapped_array", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("prefix_ns_wrapped_array")
  @JsonInclude(JsonInclude.Include.NON_NULL)
  private List<Integer> prefixNsWrappedArray = new ArrayList<>();

  public XmlItem attributeString(@Nullable String attributeString) {
    this.attributeString = attributeString;
    return this;
  }


  public XmlItem attributeNumber(@Nullable BigDecimal attributeNumber) {
    this.attributeNumber = attributeNumber;
    return this;
  }


  public XmlItem attributeInteger(@Nullable Integer attributeInteger) {
    this.attributeInteger = attributeInteger;
    return this;
  }


  public XmlItem attributeBoolean(@Nullable Boolean attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
    return this;
  }


  public XmlItem wrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
    return this;
  }

  public XmlItem addWrappedArrayItem(Integer wrappedArrayItem) {
    if (this.wrappedArray == null) {
      this.wrappedArray = new ArrayList<>();
    }
    this.wrappedArray.add(wrappedArrayItem);
    return this;
  }


  public XmlItem nameString(@Nullable String nameString) {
    this.nameString = nameString;
    return this;
  }


  public XmlItem nameNumber(@Nullable BigDecimal nameNumber) {
    this.nameNumber = nameNumber;
    return this;
  }


  public XmlItem nameInteger(@Nullable Integer nameInteger) {
    this.nameInteger = nameInteger;
    return this;
  }


  public XmlItem nameBoolean(@Nullable Boolean nameBoolean) {
    this.nameBoolean = nameBoolean;
    return this;
  }


  public XmlItem nameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
    return this;
  }

  public XmlItem addNameArrayItem(Integer nameArrayItem) {
    if (this.nameArray == null) {
      this.nameArray = new ArrayList<>();
    }
    this.nameArray.add(nameArrayItem);
    return this;
  }


  public XmlItem nameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
    return this;
  }

  public XmlItem addNameWrappedArrayItem(Integer nameWrappedArrayItem) {
    if (this.nameWrappedArray == null) {
      this.nameWrappedArray = new ArrayList<>();
    }
    this.nameWrappedArray.add(nameWrappedArrayItem);
    return this;
  }


  public XmlItem prefixString(@Nullable String prefixString) {
    this.prefixString = prefixString;
    return this;
  }


  public XmlItem prefixNumber(@Nullable BigDecimal prefixNumber) {
    this.prefixNumber = prefixNumber;
    return this;
  }


  public XmlItem prefixInteger(@Nullable Integer prefixInteger) {
    this.prefixInteger = prefixInteger;
    return this;
  }


  public XmlItem prefixBoolean(@Nullable Boolean prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
    return this;
  }


  public XmlItem prefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
    return this;
  }

  public XmlItem addPrefixArrayItem(Integer prefixArrayItem) {
    if (this.prefixArray == null) {
      this.prefixArray = new ArrayList<>();
    }
    this.prefixArray.add(prefixArrayItem);
    return this;
  }


  public XmlItem prefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
    return this;
  }

  public XmlItem addPrefixWrappedArrayItem(Integer prefixWrappedArrayItem) {
    if (this.prefixWrappedArray == null) {
      this.prefixWrappedArray = new ArrayList<>();
    }
    this.prefixWrappedArray.add(prefixWrappedArrayItem);
    return this;
  }


  public XmlItem namespaceString(@Nullable String namespaceString) {
    this.namespaceString = namespaceString;
    return this;
  }


  public XmlItem namespaceNumber(@Nullable BigDecimal namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
    return this;
  }


  public XmlItem namespaceInteger(@Nullable Integer namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
    return this;
  }


  public XmlItem namespaceBoolean(@Nullable Boolean namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
    return this;
  }


  public XmlItem namespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
    return this;
  }

  public XmlItem addNamespaceArrayItem(Integer namespaceArrayItem) {
    if (this.namespaceArray == null) {
      this.namespaceArray = new ArrayList<>();
    }
    this.namespaceArray.add(namespaceArrayItem);
    return this;
  }


  public XmlItem namespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
    return this;
  }

  public XmlItem addNamespaceWrappedArrayItem(Integer namespaceWrappedArrayItem) {
    if (this.namespaceWrappedArray == null) {
      this.namespaceWrappedArray = new ArrayList<>();
    }
    this.namespaceWrappedArray.add(namespaceWrappedArrayItem);
    return this;
  }


  public XmlItem prefixNsString(@Nullable String prefixNsString) {
    this.prefixNsString = prefixNsString;
    return this;
  }


  public XmlItem prefixNsNumber(@Nullable BigDecimal prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
    return this;
  }


  public XmlItem prefixNsInteger(@Nullable Integer prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
    return this;
  }


  public XmlItem prefixNsBoolean(@Nullable Boolean prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
    return this;
  }


  public XmlItem prefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
    return this;
  }

  public XmlItem addPrefixNsArrayItem(Integer prefixNsArrayItem) {
    if (this.prefixNsArray == null) {
      this.prefixNsArray = new ArrayList<>();
    }
    this.prefixNsArray.add(prefixNsArrayItem);
    return this;
  }


  public XmlItem prefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
    this.prefixNsWrappedArray = prefixNsWrappedArray;
    return this;
  }

  public XmlItem addPrefixNsWrappedArrayItem(Integer prefixNsWrappedArrayItem) {
    if (this.prefixNsWrappedArray == null) {
      this.prefixNsWrappedArray = new ArrayList<>();
    }
    this.prefixNsWrappedArray.add(prefixNsWrappedArrayItem);
    return this;
  }



}

