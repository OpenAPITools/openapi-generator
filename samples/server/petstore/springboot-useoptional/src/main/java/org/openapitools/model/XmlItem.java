package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * XmlItem
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class XmlItem {

  private Optional<String> attributeString = Optional.empty();

  private Optional<BigDecimal> attributeNumber = Optional.empty();

  private Optional<Integer> attributeInteger = Optional.empty();

  private Optional<Boolean> attributeBoolean = Optional.empty();

  @Valid
  private List<Integer> wrappedArray = new ArrayList<>();

  private Optional<String> nameString = Optional.empty();

  private Optional<BigDecimal> nameNumber = Optional.empty();

  private Optional<Integer> nameInteger = Optional.empty();

  private Optional<Boolean> nameBoolean = Optional.empty();

  @Valid
  private List<Integer> nameArray = new ArrayList<>();

  @Valid
  private List<Integer> nameWrappedArray = new ArrayList<>();

  private Optional<String> prefixString = Optional.empty();

  private Optional<BigDecimal> prefixNumber = Optional.empty();

  private Optional<Integer> prefixInteger = Optional.empty();

  private Optional<Boolean> prefixBoolean = Optional.empty();

  @Valid
  private List<Integer> prefixArray = new ArrayList<>();

  @Valid
  private List<Integer> prefixWrappedArray = new ArrayList<>();

  private Optional<String> namespaceString = Optional.empty();

  private Optional<BigDecimal> namespaceNumber = Optional.empty();

  private Optional<Integer> namespaceInteger = Optional.empty();

  private Optional<Boolean> namespaceBoolean = Optional.empty();

  @Valid
  private List<Integer> namespaceArray = new ArrayList<>();

  @Valid
  private List<Integer> namespaceWrappedArray = new ArrayList<>();

  private Optional<String> prefixNsString = Optional.empty();

  private Optional<BigDecimal> prefixNsNumber = Optional.empty();

  private Optional<Integer> prefixNsInteger = Optional.empty();

  private Optional<Boolean> prefixNsBoolean = Optional.empty();

  @Valid
  private List<Integer> prefixNsArray = new ArrayList<>();

  @Valid
  private List<Integer> prefixNsWrappedArray = new ArrayList<>();

  public XmlItem attributeString(String attributeString) {
    this.attributeString = Optional.of(attributeString);
    return this;
  }

  /**
   * Get attributeString
   * @return attributeString
  */
  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("attribute_string")
  public Optional<String> getAttributeString() {
    return attributeString;
  }

  public void setAttributeString(Optional<String> attributeString) {
    this.attributeString = attributeString;
  }

  public XmlItem attributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = Optional.of(attributeNumber);
    return this;
  }

  /**
   * Get attributeNumber
   * @return attributeNumber
  */
  @Valid 
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("attribute_number")
  public Optional<BigDecimal> getAttributeNumber() {
    return attributeNumber;
  }

  public void setAttributeNumber(Optional<BigDecimal> attributeNumber) {
    this.attributeNumber = attributeNumber;
  }

  public XmlItem attributeInteger(Integer attributeInteger) {
    this.attributeInteger = Optional.of(attributeInteger);
    return this;
  }

  /**
   * Get attributeInteger
   * @return attributeInteger
  */
  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("attribute_integer")
  public Optional<Integer> getAttributeInteger() {
    return attributeInteger;
  }

  public void setAttributeInteger(Optional<Integer> attributeInteger) {
    this.attributeInteger = attributeInteger;
  }

  public XmlItem attributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = Optional.of(attributeBoolean);
    return this;
  }

  /**
   * Get attributeBoolean
   * @return attributeBoolean
  */
  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("attribute_boolean")
  public Optional<Boolean> getAttributeBoolean() {
    return attributeBoolean;
  }

  public void setAttributeBoolean(Optional<Boolean> attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("wrapped_array")
  public List<Integer> getWrappedArray() {
    return wrappedArray;
  }

  public void setWrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
  }

  public XmlItem nameString(String nameString) {
    this.nameString = Optional.of(nameString);
    return this;
  }

  /**
   * Get nameString
   * @return nameString
  */
  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("name_string")
  public Optional<String> getNameString() {
    return nameString;
  }

  public void setNameString(Optional<String> nameString) {
    this.nameString = nameString;
  }

  public XmlItem nameNumber(BigDecimal nameNumber) {
    this.nameNumber = Optional.of(nameNumber);
    return this;
  }

  /**
   * Get nameNumber
   * @return nameNumber
  */
  @Valid 
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("name_number")
  public Optional<BigDecimal> getNameNumber() {
    return nameNumber;
  }

  public void setNameNumber(Optional<BigDecimal> nameNumber) {
    this.nameNumber = nameNumber;
  }

  public XmlItem nameInteger(Integer nameInteger) {
    this.nameInteger = Optional.of(nameInteger);
    return this;
  }

  /**
   * Get nameInteger
   * @return nameInteger
  */
  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("name_integer")
  public Optional<Integer> getNameInteger() {
    return nameInteger;
  }

  public void setNameInteger(Optional<Integer> nameInteger) {
    this.nameInteger = nameInteger;
  }

  public XmlItem nameBoolean(Boolean nameBoolean) {
    this.nameBoolean = Optional.of(nameBoolean);
    return this;
  }

  /**
   * Get nameBoolean
   * @return nameBoolean
  */
  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("name_boolean")
  public Optional<Boolean> getNameBoolean() {
    return nameBoolean;
  }

  public void setNameBoolean(Optional<Boolean> nameBoolean) {
    this.nameBoolean = nameBoolean;
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("name_array")
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("name_wrapped_array")
  public List<Integer> getNameWrappedArray() {
    return nameWrappedArray;
  }

  public void setNameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
  }

  public XmlItem prefixString(String prefixString) {
    this.prefixString = Optional.of(prefixString);
    return this;
  }

  /**
   * Get prefixString
   * @return prefixString
  */
  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("prefix_string")
  public Optional<String> getPrefixString() {
    return prefixString;
  }

  public void setPrefixString(Optional<String> prefixString) {
    this.prefixString = prefixString;
  }

  public XmlItem prefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = Optional.of(prefixNumber);
    return this;
  }

  /**
   * Get prefixNumber
   * @return prefixNumber
  */
  @Valid 
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("prefix_number")
  public Optional<BigDecimal> getPrefixNumber() {
    return prefixNumber;
  }

  public void setPrefixNumber(Optional<BigDecimal> prefixNumber) {
    this.prefixNumber = prefixNumber;
  }

  public XmlItem prefixInteger(Integer prefixInteger) {
    this.prefixInteger = Optional.of(prefixInteger);
    return this;
  }

  /**
   * Get prefixInteger
   * @return prefixInteger
  */
  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("prefix_integer")
  public Optional<Integer> getPrefixInteger() {
    return prefixInteger;
  }

  public void setPrefixInteger(Optional<Integer> prefixInteger) {
    this.prefixInteger = prefixInteger;
  }

  public XmlItem prefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = Optional.of(prefixBoolean);
    return this;
  }

  /**
   * Get prefixBoolean
   * @return prefixBoolean
  */
  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("prefix_boolean")
  public Optional<Boolean> getPrefixBoolean() {
    return prefixBoolean;
  }

  public void setPrefixBoolean(Optional<Boolean> prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_array")
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_wrapped_array")
  public List<Integer> getPrefixWrappedArray() {
    return prefixWrappedArray;
  }

  public void setPrefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
  }

  public XmlItem namespaceString(String namespaceString) {
    this.namespaceString = Optional.of(namespaceString);
    return this;
  }

  /**
   * Get namespaceString
   * @return namespaceString
  */
  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("namespace_string")
  public Optional<String> getNamespaceString() {
    return namespaceString;
  }

  public void setNamespaceString(Optional<String> namespaceString) {
    this.namespaceString = namespaceString;
  }

  public XmlItem namespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = Optional.of(namespaceNumber);
    return this;
  }

  /**
   * Get namespaceNumber
   * @return namespaceNumber
  */
  @Valid 
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("namespace_number")
  public Optional<BigDecimal> getNamespaceNumber() {
    return namespaceNumber;
  }

  public void setNamespaceNumber(Optional<BigDecimal> namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
  }

  public XmlItem namespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = Optional.of(namespaceInteger);
    return this;
  }

  /**
   * Get namespaceInteger
   * @return namespaceInteger
  */
  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("namespace_integer")
  public Optional<Integer> getNamespaceInteger() {
    return namespaceInteger;
  }

  public void setNamespaceInteger(Optional<Integer> namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
  }

  public XmlItem namespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = Optional.of(namespaceBoolean);
    return this;
  }

  /**
   * Get namespaceBoolean
   * @return namespaceBoolean
  */
  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("namespace_boolean")
  public Optional<Boolean> getNamespaceBoolean() {
    return namespaceBoolean;
  }

  public void setNamespaceBoolean(Optional<Boolean> namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("namespace_array")
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("namespace_wrapped_array")
  public List<Integer> getNamespaceWrappedArray() {
    return namespaceWrappedArray;
  }

  public void setNamespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
  }

  public XmlItem prefixNsString(String prefixNsString) {
    this.prefixNsString = Optional.of(prefixNsString);
    return this;
  }

  /**
   * Get prefixNsString
   * @return prefixNsString
  */
  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("prefix_ns_string")
  public Optional<String> getPrefixNsString() {
    return prefixNsString;
  }

  public void setPrefixNsString(Optional<String> prefixNsString) {
    this.prefixNsString = prefixNsString;
  }

  public XmlItem prefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = Optional.of(prefixNsNumber);
    return this;
  }

  /**
   * Get prefixNsNumber
   * @return prefixNsNumber
  */
  @Valid 
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("prefix_ns_number")
  public Optional<BigDecimal> getPrefixNsNumber() {
    return prefixNsNumber;
  }

  public void setPrefixNsNumber(Optional<BigDecimal> prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
  }

  public XmlItem prefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = Optional.of(prefixNsInteger);
    return this;
  }

  /**
   * Get prefixNsInteger
   * @return prefixNsInteger
  */
  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("prefix_ns_integer")
  public Optional<Integer> getPrefixNsInteger() {
    return prefixNsInteger;
  }

  public void setPrefixNsInteger(Optional<Integer> prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
  }

  public XmlItem prefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = Optional.of(prefixNsBoolean);
    return this;
  }

  /**
   * Get prefixNsBoolean
   * @return prefixNsBoolean
  */
  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("prefix_ns_boolean")
  public Optional<Boolean> getPrefixNsBoolean() {
    return prefixNsBoolean;
  }

  public void setPrefixNsBoolean(Optional<Boolean> prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_ns_array")
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
  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_ns_wrapped_array")
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

