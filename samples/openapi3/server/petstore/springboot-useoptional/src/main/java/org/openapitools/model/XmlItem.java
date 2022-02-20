package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;
import io.swagger.v3.oas.annotations.media.Schema;


import java.util.*;
import javax.annotation.Generated;

/**
 * XmlItem
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class XmlItem   {

  @JsonProperty("attribute_string")
  private Optional<String> attributeString = Optional.empty();

  @JsonProperty("attribute_number")
  private Optional<BigDecimal> attributeNumber = Optional.empty();

  @JsonProperty("attribute_integer")
  private Optional<Integer> attributeInteger = Optional.empty();

  @JsonProperty("attribute_boolean")
  private Optional<Boolean> attributeBoolean = Optional.empty();

  @JsonProperty("wrapped_array")
  @Valid
  private List<Integer> wrappedArray = new ArrayList<>();

  @JsonProperty("name_string")
  private Optional<String> nameString = Optional.empty();

  @JsonProperty("name_number")
  private Optional<BigDecimal> nameNumber = Optional.empty();

  @JsonProperty("name_integer")
  private Optional<Integer> nameInteger = Optional.empty();

  @JsonProperty("name_boolean")
  private Optional<Boolean> nameBoolean = Optional.empty();

  @JsonProperty("name_array")
  @Valid
  private List<Integer> nameArray = new ArrayList<>();

  @JsonProperty("name_wrapped_array")
  @Valid
  private List<Integer> nameWrappedArray = new ArrayList<>();

  @JsonProperty("prefix_string")
  private Optional<String> prefixString = Optional.empty();

  @JsonProperty("prefix_number")
  private Optional<BigDecimal> prefixNumber = Optional.empty();

  @JsonProperty("prefix_integer")
  private Optional<Integer> prefixInteger = Optional.empty();

  @JsonProperty("prefix_boolean")
  private Optional<Boolean> prefixBoolean = Optional.empty();

  @JsonProperty("prefix_array")
  @Valid
  private List<Integer> prefixArray = new ArrayList<>();

  @JsonProperty("prefix_wrapped_array")
  @Valid
  private List<Integer> prefixWrappedArray = new ArrayList<>();

  @JsonProperty("namespace_string")
  private Optional<String> namespaceString = Optional.empty();

  @JsonProperty("namespace_number")
  private Optional<BigDecimal> namespaceNumber = Optional.empty();

  @JsonProperty("namespace_integer")
  private Optional<Integer> namespaceInteger = Optional.empty();

  @JsonProperty("namespace_boolean")
  private Optional<Boolean> namespaceBoolean = Optional.empty();

  @JsonProperty("namespace_array")
  @Valid
  private List<Integer> namespaceArray = new ArrayList<>();

  @JsonProperty("namespace_wrapped_array")
  @Valid
  private List<Integer> namespaceWrappedArray = new ArrayList<>();

  @JsonProperty("prefix_ns_string")
  private Optional<String> prefixNsString = Optional.empty();

  @JsonProperty("prefix_ns_number")
  private Optional<BigDecimal> prefixNsNumber = Optional.empty();

  @JsonProperty("prefix_ns_integer")
  private Optional<Integer> prefixNsInteger = Optional.empty();

  @JsonProperty("prefix_ns_boolean")
  private Optional<Boolean> prefixNsBoolean = Optional.empty();

  @JsonProperty("prefix_ns_array")
  @Valid
  private List<Integer> prefixNsArray = new ArrayList<>();

  @JsonProperty("prefix_ns_wrapped_array")
  @Valid
  private List<Integer> prefixNsWrappedArray = new ArrayList<>();

  public XmlItem attributeString(String attributeString) {
    this.attributeString = Optional.ofNullable(attributeString);
    return this;
  }

  /**
   * Get attributeString
   * @return attributeString
  */
  @Schema(name = "attribute_string", example = "string", required = false)
  public Optional<String> getAttributeString() {
    return attributeString;
  }

  @JsonIgnore
  public void setAttributeString(String attributeString) {
    this.attributeString = Optional.ofNullable(attributeString);
  }

  public XmlItem attributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = Optional.ofNullable(attributeNumber);
    return this;
  }

  /**
   * Get attributeNumber
   * @return attributeNumber
  */
  @Schema(name = "attribute_number", example = "1.234", required = false)
  public Optional<BigDecimal> getAttributeNumber() {
    return attributeNumber;
  }

  @JsonIgnore
  public void setAttributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = Optional.ofNullable(attributeNumber);
  }

  public XmlItem attributeInteger(Integer attributeInteger) {
    this.attributeInteger = Optional.ofNullable(attributeInteger);
    return this;
  }

  /**
   * Get attributeInteger
   * @return attributeInteger
  */
  @Schema(name = "attribute_integer", example = "-2", required = false)
  public Optional<Integer> getAttributeInteger() {
    return attributeInteger;
  }

  @JsonIgnore
  public void setAttributeInteger(Integer attributeInteger) {
    this.attributeInteger = Optional.ofNullable(attributeInteger);
  }

  public XmlItem attributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = Optional.ofNullable(attributeBoolean);
    return this;
  }

  /**
   * Get attributeBoolean
   * @return attributeBoolean
  */
  @Schema(name = "attribute_boolean", example = "true", required = false)
  public Optional<Boolean> getAttributeBoolean() {
    return attributeBoolean;
  }

  @JsonIgnore
  public void setAttributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = Optional.ofNullable(attributeBoolean);
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

  /**
   * Get wrappedArray
   * @return wrappedArray
  */
  
  @Schema(name = "wrapped_array", required = false)
  public List<Integer> getWrappedArray() {
    return wrappedArray;
  }

  public void setWrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
  }

  public XmlItem nameString(String nameString) {
    this.nameString = Optional.ofNullable(nameString);
    return this;
  }

  /**
   * Get nameString
   * @return nameString
  */
  @Schema(name = "name_string", example = "string", required = false)
  public Optional<String> getNameString() {
    return nameString;
  }

  @JsonIgnore
  public void setNameString(String nameString) {
    this.nameString = Optional.ofNullable(nameString);
  }

  public XmlItem nameNumber(BigDecimal nameNumber) {
    this.nameNumber = Optional.ofNullable(nameNumber);
    return this;
  }

  /**
   * Get nameNumber
   * @return nameNumber
  */
  @Schema(name = "name_number", example = "1.234", required = false)
  public Optional<BigDecimal> getNameNumber() {
    return nameNumber;
  }

  @JsonIgnore
  public void setNameNumber(BigDecimal nameNumber) {
    this.nameNumber = Optional.ofNullable(nameNumber);
  }

  public XmlItem nameInteger(Integer nameInteger) {
    this.nameInteger = Optional.ofNullable(nameInteger);
    return this;
  }

  /**
   * Get nameInteger
   * @return nameInteger
  */
  @Schema(name = "name_integer", example = "-2", required = false)
  public Optional<Integer> getNameInteger() {
    return nameInteger;
  }

  @JsonIgnore
  public void setNameInteger(Integer nameInteger) {
    this.nameInteger = Optional.ofNullable(nameInteger);
  }

  public XmlItem nameBoolean(Boolean nameBoolean) {
    this.nameBoolean = Optional.ofNullable(nameBoolean);
    return this;
  }

  /**
   * Get nameBoolean
   * @return nameBoolean
  */
  @Schema(name = "name_boolean", example = "true", required = false)
  public Optional<Boolean> getNameBoolean() {
    return nameBoolean;
  }

  @JsonIgnore
  public void setNameBoolean(Boolean nameBoolean) {
    this.nameBoolean = Optional.ofNullable(nameBoolean);
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

  /**
   * Get nameArray
   * @return nameArray
  */
  
  @Schema(name = "name_array", required = false)
  public List<Integer> getNameArray() {
    return nameArray;
  }

  public void setNameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
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

  /**
   * Get nameWrappedArray
   * @return nameWrappedArray
  */
  
  @Schema(name = "name_wrapped_array", required = false)
  public List<Integer> getNameWrappedArray() {
    return nameWrappedArray;
  }

  public void setNameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
  }

  public XmlItem prefixString(String prefixString) {
    this.prefixString = Optional.ofNullable(prefixString);
    return this;
  }

  /**
   * Get prefixString
   * @return prefixString
  */
  @Schema(name = "prefix_string", example = "string", required = false)
  public Optional<String> getPrefixString() {
    return prefixString;
  }

  @JsonIgnore
  public void setPrefixString(String prefixString) {
    this.prefixString = Optional.ofNullable(prefixString);
  }

  public XmlItem prefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = Optional.ofNullable(prefixNumber);
    return this;
  }

  /**
   * Get prefixNumber
   * @return prefixNumber
  */
  @Schema(name = "prefix_number", example = "1.234", required = false)
  public Optional<BigDecimal> getPrefixNumber() {
    return prefixNumber;
  }

  @JsonIgnore
  public void setPrefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = Optional.ofNullable(prefixNumber);
  }

  public XmlItem prefixInteger(Integer prefixInteger) {
    this.prefixInteger = Optional.ofNullable(prefixInteger);
    return this;
  }

  /**
   * Get prefixInteger
   * @return prefixInteger
  */
  @Schema(name = "prefix_integer", example = "-2", required = false)
  public Optional<Integer> getPrefixInteger() {
    return prefixInteger;
  }

  @JsonIgnore
  public void setPrefixInteger(Integer prefixInteger) {
    this.prefixInteger = Optional.ofNullable(prefixInteger);
  }

  public XmlItem prefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = Optional.ofNullable(prefixBoolean);
    return this;
  }

  /**
   * Get prefixBoolean
   * @return prefixBoolean
  */
  @Schema(name = "prefix_boolean", example = "true", required = false)
  public Optional<Boolean> getPrefixBoolean() {
    return prefixBoolean;
  }

  @JsonIgnore
  public void setPrefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = Optional.ofNullable(prefixBoolean);
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

  /**
   * Get prefixArray
   * @return prefixArray
  */
  
  @Schema(name = "prefix_array", required = false)
  public List<Integer> getPrefixArray() {
    return prefixArray;
  }

  public void setPrefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
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

  /**
   * Get prefixWrappedArray
   * @return prefixWrappedArray
  */
  
  @Schema(name = "prefix_wrapped_array", required = false)
  public List<Integer> getPrefixWrappedArray() {
    return prefixWrappedArray;
  }

  public void setPrefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
  }

  public XmlItem namespaceString(String namespaceString) {
    this.namespaceString = Optional.ofNullable(namespaceString);
    return this;
  }

  /**
   * Get namespaceString
   * @return namespaceString
  */
  @Schema(name = "namespace_string", example = "string", required = false)
  public Optional<String> getNamespaceString() {
    return namespaceString;
  }

  @JsonIgnore
  public void setNamespaceString(String namespaceString) {
    this.namespaceString = Optional.ofNullable(namespaceString);
  }

  public XmlItem namespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = Optional.ofNullable(namespaceNumber);
    return this;
  }

  /**
   * Get namespaceNumber
   * @return namespaceNumber
  */
  @Schema(name = "namespace_number", example = "1.234", required = false)
  public Optional<BigDecimal> getNamespaceNumber() {
    return namespaceNumber;
  }

  @JsonIgnore
  public void setNamespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = Optional.ofNullable(namespaceNumber);
  }

  public XmlItem namespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = Optional.ofNullable(namespaceInteger);
    return this;
  }

  /**
   * Get namespaceInteger
   * @return namespaceInteger
  */
  @Schema(name = "namespace_integer", example = "-2", required = false)
  public Optional<Integer> getNamespaceInteger() {
    return namespaceInteger;
  }

  @JsonIgnore
  public void setNamespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = Optional.ofNullable(namespaceInteger);
  }

  public XmlItem namespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = Optional.ofNullable(namespaceBoolean);
    return this;
  }

  /**
   * Get namespaceBoolean
   * @return namespaceBoolean
  */
  @Schema(name = "namespace_boolean", example = "true", required = false)
  public Optional<Boolean> getNamespaceBoolean() {
    return namespaceBoolean;
  }

  @JsonIgnore
  public void setNamespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = Optional.ofNullable(namespaceBoolean);
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

  /**
   * Get namespaceArray
   * @return namespaceArray
  */
  
  @Schema(name = "namespace_array", required = false)
  public List<Integer> getNamespaceArray() {
    return namespaceArray;
  }

  public void setNamespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
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

  /**
   * Get namespaceWrappedArray
   * @return namespaceWrappedArray
  */
  
  @Schema(name = "namespace_wrapped_array", required = false)
  public List<Integer> getNamespaceWrappedArray() {
    return namespaceWrappedArray;
  }

  public void setNamespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
  }

  public XmlItem prefixNsString(String prefixNsString) {
    this.prefixNsString = Optional.ofNullable(prefixNsString);
    return this;
  }

  /**
   * Get prefixNsString
   * @return prefixNsString
  */
  @Schema(name = "prefix_ns_string", example = "string", required = false)
  public Optional<String> getPrefixNsString() {
    return prefixNsString;
  }

  @JsonIgnore
  public void setPrefixNsString(String prefixNsString) {
    this.prefixNsString = Optional.ofNullable(prefixNsString);
  }

  public XmlItem prefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = Optional.ofNullable(prefixNsNumber);
    return this;
  }

  /**
   * Get prefixNsNumber
   * @return prefixNsNumber
  */
  @Schema(name = "prefix_ns_number", example = "1.234", required = false)
  public Optional<BigDecimal> getPrefixNsNumber() {
    return prefixNsNumber;
  }

  @JsonIgnore
  public void setPrefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = Optional.ofNullable(prefixNsNumber);
  }

  public XmlItem prefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = Optional.ofNullable(prefixNsInteger);
    return this;
  }

  /**
   * Get prefixNsInteger
   * @return prefixNsInteger
  */
  @Schema(name = "prefix_ns_integer", example = "-2", required = false)
  public Optional<Integer> getPrefixNsInteger() {
    return prefixNsInteger;
  }

  @JsonIgnore
  public void setPrefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = Optional.ofNullable(prefixNsInteger);
  }

  public XmlItem prefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = Optional.ofNullable(prefixNsBoolean);
    return this;
  }

  /**
   * Get prefixNsBoolean
   * @return prefixNsBoolean
  */
  @Schema(name = "prefix_ns_boolean", example = "true", required = false)
  public Optional<Boolean> getPrefixNsBoolean() {
    return prefixNsBoolean;
  }

  @JsonIgnore
  public void setPrefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = Optional.ofNullable(prefixNsBoolean);
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

  /**
   * Get prefixNsArray
   * @return prefixNsArray
  */
  
  @Schema(name = "prefix_ns_array", required = false)
  public List<Integer> getPrefixNsArray() {
    return prefixNsArray;
  }

  public void setPrefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
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

  /**
   * Get prefixNsWrappedArray
   * @return prefixNsWrappedArray
  */
  
  @Schema(name = "prefix_ns_wrapped_array", required = false)
  public List<Integer> getPrefixNsWrappedArray() {
    return prefixNsWrappedArray;
  }

  public void setPrefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
    this.prefixNsWrappedArray = prefixNsWrappedArray;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    XmlItem xmlItem = (XmlItem) o;
    return Objects.equals(this.attributeString, xmlItem.attributeString) &&
        Objects.equals(this.attributeNumber, xmlItem.attributeNumber) &&
        Objects.equals(this.attributeInteger, xmlItem.attributeInteger) &&
        Objects.equals(this.attributeBoolean, xmlItem.attributeBoolean) &&
        Objects.equals(this.wrappedArray, xmlItem.wrappedArray) &&
        Objects.equals(this.nameString, xmlItem.nameString) &&
        Objects.equals(this.nameNumber, xmlItem.nameNumber) &&
        Objects.equals(this.nameInteger, xmlItem.nameInteger) &&
        Objects.equals(this.nameBoolean, xmlItem.nameBoolean) &&
        Objects.equals(this.nameArray, xmlItem.nameArray) &&
        Objects.equals(this.nameWrappedArray, xmlItem.nameWrappedArray) &&
        Objects.equals(this.prefixString, xmlItem.prefixString) &&
        Objects.equals(this.prefixNumber, xmlItem.prefixNumber) &&
        Objects.equals(this.prefixInteger, xmlItem.prefixInteger) &&
        Objects.equals(this.prefixBoolean, xmlItem.prefixBoolean) &&
        Objects.equals(this.prefixArray, xmlItem.prefixArray) &&
        Objects.equals(this.prefixWrappedArray, xmlItem.prefixWrappedArray) &&
        Objects.equals(this.namespaceString, xmlItem.namespaceString) &&
        Objects.equals(this.namespaceNumber, xmlItem.namespaceNumber) &&
        Objects.equals(this.namespaceInteger, xmlItem.namespaceInteger) &&
        Objects.equals(this.namespaceBoolean, xmlItem.namespaceBoolean) &&
        Objects.equals(this.namespaceArray, xmlItem.namespaceArray) &&
        Objects.equals(this.namespaceWrappedArray, xmlItem.namespaceWrappedArray) &&
        Objects.equals(this.prefixNsString, xmlItem.prefixNsString) &&
        Objects.equals(this.prefixNsNumber, xmlItem.prefixNsNumber) &&
        Objects.equals(this.prefixNsInteger, xmlItem.prefixNsInteger) &&
        Objects.equals(this.prefixNsBoolean, xmlItem.prefixNsBoolean) &&
        Objects.equals(this.prefixNsArray, xmlItem.prefixNsArray) &&
        Objects.equals(this.prefixNsWrappedArray, xmlItem.prefixNsWrappedArray);
  }

  @Override
  public int hashCode() {
    return Objects.hash(attributeString, attributeNumber, attributeInteger, attributeBoolean, wrappedArray, nameString, nameNumber, nameInteger, nameBoolean, nameArray, nameWrappedArray, prefixString, prefixNumber, prefixInteger, prefixBoolean, prefixArray, prefixWrappedArray, namespaceString, namespaceNumber, namespaceInteger, namespaceBoolean, namespaceArray, namespaceWrappedArray, prefixNsString, prefixNsNumber, prefixNsInteger, prefixNsBoolean, prefixNsArray, prefixNsWrappedArray);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class XmlItem {\n");
    sb.append("    attributeString: ").append(toIndentedString(attributeString)).append("\n");
    sb.append("    attributeNumber: ").append(toIndentedString(attributeNumber)).append("\n");
    sb.append("    attributeInteger: ").append(toIndentedString(attributeInteger)).append("\n");
    sb.append("    attributeBoolean: ").append(toIndentedString(attributeBoolean)).append("\n");
    sb.append("    wrappedArray: ").append(toIndentedString(wrappedArray)).append("\n");
    sb.append("    nameString: ").append(toIndentedString(nameString)).append("\n");
    sb.append("    nameNumber: ").append(toIndentedString(nameNumber)).append("\n");
    sb.append("    nameInteger: ").append(toIndentedString(nameInteger)).append("\n");
    sb.append("    nameBoolean: ").append(toIndentedString(nameBoolean)).append("\n");
    sb.append("    nameArray: ").append(toIndentedString(nameArray)).append("\n");
    sb.append("    nameWrappedArray: ").append(toIndentedString(nameWrappedArray)).append("\n");
    sb.append("    prefixString: ").append(toIndentedString(prefixString)).append("\n");
    sb.append("    prefixNumber: ").append(toIndentedString(prefixNumber)).append("\n");
    sb.append("    prefixInteger: ").append(toIndentedString(prefixInteger)).append("\n");
    sb.append("    prefixBoolean: ").append(toIndentedString(prefixBoolean)).append("\n");
    sb.append("    prefixArray: ").append(toIndentedString(prefixArray)).append("\n");
    sb.append("    prefixWrappedArray: ").append(toIndentedString(prefixWrappedArray)).append("\n");
    sb.append("    namespaceString: ").append(toIndentedString(namespaceString)).append("\n");
    sb.append("    namespaceNumber: ").append(toIndentedString(namespaceNumber)).append("\n");
    sb.append("    namespaceInteger: ").append(toIndentedString(namespaceInteger)).append("\n");
    sb.append("    namespaceBoolean: ").append(toIndentedString(namespaceBoolean)).append("\n");
    sb.append("    namespaceArray: ").append(toIndentedString(namespaceArray)).append("\n");
    sb.append("    namespaceWrappedArray: ").append(toIndentedString(namespaceWrappedArray)).append("\n");
    sb.append("    prefixNsString: ").append(toIndentedString(prefixNsString)).append("\n");
    sb.append("    prefixNsNumber: ").append(toIndentedString(prefixNsNumber)).append("\n");
    sb.append("    prefixNsInteger: ").append(toIndentedString(prefixNsInteger)).append("\n");
    sb.append("    prefixNsBoolean: ").append(toIndentedString(prefixNsBoolean)).append("\n");
    sb.append("    prefixNsArray: ").append(toIndentedString(prefixNsArray)).append("\n");
    sb.append("    prefixNsWrappedArray: ").append(toIndentedString(prefixNsWrappedArray)).append("\n");
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

