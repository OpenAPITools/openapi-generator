package org.openapitools.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class XmlItem  {
  
  @ApiModelProperty(example = "string", value = "")
  private String attributeString;

  @ApiModelProperty(example = "1.234", value = "")
  @Valid
  private BigDecimal attributeNumber;

  @ApiModelProperty(example = "-2", value = "")
  private Integer attributeInteger;

  @ApiModelProperty(example = "true", value = "")
  private Boolean attributeBoolean;

  @ApiModelProperty(value = "")
  private List<Integer> wrappedArray = null;

  @ApiModelProperty(example = "string", value = "")
  private String nameString;

  @ApiModelProperty(example = "1.234", value = "")
  @Valid
  private BigDecimal nameNumber;

  @ApiModelProperty(example = "-2", value = "")
  private Integer nameInteger;

  @ApiModelProperty(example = "true", value = "")
  private Boolean nameBoolean;

  @ApiModelProperty(value = "")
  private List<Integer> nameArray = null;

  @ApiModelProperty(value = "")
  private List<Integer> nameWrappedArray = null;

  @ApiModelProperty(example = "string", value = "")
  private String prefixString;

  @ApiModelProperty(example = "1.234", value = "")
  @Valid
  private BigDecimal prefixNumber;

  @ApiModelProperty(example = "-2", value = "")
  private Integer prefixInteger;

  @ApiModelProperty(example = "true", value = "")
  private Boolean prefixBoolean;

  @ApiModelProperty(value = "")
  private List<Integer> prefixArray = null;

  @ApiModelProperty(value = "")
  private List<Integer> prefixWrappedArray = null;

  @ApiModelProperty(example = "string", value = "")
  private String namespaceString;

  @ApiModelProperty(example = "1.234", value = "")
  @Valid
  private BigDecimal namespaceNumber;

  @ApiModelProperty(example = "-2", value = "")
  private Integer namespaceInteger;

  @ApiModelProperty(example = "true", value = "")
  private Boolean namespaceBoolean;

  @ApiModelProperty(value = "")
  private List<Integer> namespaceArray = null;

  @ApiModelProperty(value = "")
  private List<Integer> namespaceWrappedArray = null;

  @ApiModelProperty(example = "string", value = "")
  private String prefixNsString;

  @ApiModelProperty(example = "1.234", value = "")
  @Valid
  private BigDecimal prefixNsNumber;

  @ApiModelProperty(example = "-2", value = "")
  private Integer prefixNsInteger;

  @ApiModelProperty(example = "true", value = "")
  private Boolean prefixNsBoolean;

  @ApiModelProperty(value = "")
  private List<Integer> prefixNsArray = null;

  @ApiModelProperty(value = "")
  private List<Integer> prefixNsWrappedArray = null;
 /**
  * Get attributeString
  * @return attributeString
  */
  @JsonProperty("attribute_string")
  public String getAttributeString() {
    return attributeString;
  }

  /**
   * Sets the <code>attributeString</code> property.
   */
  public void setAttributeString(String attributeString) {
    this.attributeString = attributeString;
  }

  /**
   * Sets the <code>attributeString</code> property.
   */
  public XmlItem attributeString(String attributeString) {
    this.attributeString = attributeString;
    return this;
  }

 /**
  * Get attributeNumber
  * @return attributeNumber
  */
  @JsonProperty("attribute_number")
  public BigDecimal getAttributeNumber() {
    return attributeNumber;
  }

  /**
   * Sets the <code>attributeNumber</code> property.
   */
  public void setAttributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = attributeNumber;
  }

  /**
   * Sets the <code>attributeNumber</code> property.
   */
  public XmlItem attributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = attributeNumber;
    return this;
  }

 /**
  * Get attributeInteger
  * @return attributeInteger
  */
  @JsonProperty("attribute_integer")
  public Integer getAttributeInteger() {
    return attributeInteger;
  }

  /**
   * Sets the <code>attributeInteger</code> property.
   */
  public void setAttributeInteger(Integer attributeInteger) {
    this.attributeInteger = attributeInteger;
  }

  /**
   * Sets the <code>attributeInteger</code> property.
   */
  public XmlItem attributeInteger(Integer attributeInteger) {
    this.attributeInteger = attributeInteger;
    return this;
  }

 /**
  * Get attributeBoolean
  * @return attributeBoolean
  */
  @JsonProperty("attribute_boolean")
  public Boolean getAttributeBoolean() {
    return attributeBoolean;
  }

  /**
   * Sets the <code>attributeBoolean</code> property.
   */
  public void setAttributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
  }

  /**
   * Sets the <code>attributeBoolean</code> property.
   */
  public XmlItem attributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
    return this;
  }

 /**
  * Get wrappedArray
  * @return wrappedArray
  */
  @JsonProperty("wrapped_array")
  public List<Integer> getWrappedArray() {
    return wrappedArray;
  }

  /**
   * Sets the <code>wrappedArray</code> property.
   */
  public void setWrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
  }

  /**
   * Sets the <code>wrappedArray</code> property.
   */
  public XmlItem wrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
    return this;
  }

  /**
   * Adds a new item to the <code>wrappedArray</code> list.
   */
  public XmlItem addWrappedArrayItem(Integer wrappedArrayItem) {
    this.wrappedArray.add(wrappedArrayItem);
    return this;
  }

 /**
  * Get nameString
  * @return nameString
  */
  @JsonProperty("name_string")
  public String getNameString() {
    return nameString;
  }

  /**
   * Sets the <code>nameString</code> property.
   */
  public void setNameString(String nameString) {
    this.nameString = nameString;
  }

  /**
   * Sets the <code>nameString</code> property.
   */
  public XmlItem nameString(String nameString) {
    this.nameString = nameString;
    return this;
  }

 /**
  * Get nameNumber
  * @return nameNumber
  */
  @JsonProperty("name_number")
  public BigDecimal getNameNumber() {
    return nameNumber;
  }

  /**
   * Sets the <code>nameNumber</code> property.
   */
  public void setNameNumber(BigDecimal nameNumber) {
    this.nameNumber = nameNumber;
  }

  /**
   * Sets the <code>nameNumber</code> property.
   */
  public XmlItem nameNumber(BigDecimal nameNumber) {
    this.nameNumber = nameNumber;
    return this;
  }

 /**
  * Get nameInteger
  * @return nameInteger
  */
  @JsonProperty("name_integer")
  public Integer getNameInteger() {
    return nameInteger;
  }

  /**
   * Sets the <code>nameInteger</code> property.
   */
  public void setNameInteger(Integer nameInteger) {
    this.nameInteger = nameInteger;
  }

  /**
   * Sets the <code>nameInteger</code> property.
   */
  public XmlItem nameInteger(Integer nameInteger) {
    this.nameInteger = nameInteger;
    return this;
  }

 /**
  * Get nameBoolean
  * @return nameBoolean
  */
  @JsonProperty("name_boolean")
  public Boolean getNameBoolean() {
    return nameBoolean;
  }

  /**
   * Sets the <code>nameBoolean</code> property.
   */
  public void setNameBoolean(Boolean nameBoolean) {
    this.nameBoolean = nameBoolean;
  }

  /**
   * Sets the <code>nameBoolean</code> property.
   */
  public XmlItem nameBoolean(Boolean nameBoolean) {
    this.nameBoolean = nameBoolean;
    return this;
  }

 /**
  * Get nameArray
  * @return nameArray
  */
  @JsonProperty("name_array")
  public List<Integer> getNameArray() {
    return nameArray;
  }

  /**
   * Sets the <code>nameArray</code> property.
   */
  public void setNameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
  }

  /**
   * Sets the <code>nameArray</code> property.
   */
  public XmlItem nameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
    return this;
  }

  /**
   * Adds a new item to the <code>nameArray</code> list.
   */
  public XmlItem addNameArrayItem(Integer nameArrayItem) {
    this.nameArray.add(nameArrayItem);
    return this;
  }

 /**
  * Get nameWrappedArray
  * @return nameWrappedArray
  */
  @JsonProperty("name_wrapped_array")
  public List<Integer> getNameWrappedArray() {
    return nameWrappedArray;
  }

  /**
   * Sets the <code>nameWrappedArray</code> property.
   */
  public void setNameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
  }

  /**
   * Sets the <code>nameWrappedArray</code> property.
   */
  public XmlItem nameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
    return this;
  }

  /**
   * Adds a new item to the <code>nameWrappedArray</code> list.
   */
  public XmlItem addNameWrappedArrayItem(Integer nameWrappedArrayItem) {
    this.nameWrappedArray.add(nameWrappedArrayItem);
    return this;
  }

 /**
  * Get prefixString
  * @return prefixString
  */
  @JsonProperty("prefix_string")
  public String getPrefixString() {
    return prefixString;
  }

  /**
   * Sets the <code>prefixString</code> property.
   */
  public void setPrefixString(String prefixString) {
    this.prefixString = prefixString;
  }

  /**
   * Sets the <code>prefixString</code> property.
   */
  public XmlItem prefixString(String prefixString) {
    this.prefixString = prefixString;
    return this;
  }

 /**
  * Get prefixNumber
  * @return prefixNumber
  */
  @JsonProperty("prefix_number")
  public BigDecimal getPrefixNumber() {
    return prefixNumber;
  }

  /**
   * Sets the <code>prefixNumber</code> property.
   */
  public void setPrefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = prefixNumber;
  }

  /**
   * Sets the <code>prefixNumber</code> property.
   */
  public XmlItem prefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = prefixNumber;
    return this;
  }

 /**
  * Get prefixInteger
  * @return prefixInteger
  */
  @JsonProperty("prefix_integer")
  public Integer getPrefixInteger() {
    return prefixInteger;
  }

  /**
   * Sets the <code>prefixInteger</code> property.
   */
  public void setPrefixInteger(Integer prefixInteger) {
    this.prefixInteger = prefixInteger;
  }

  /**
   * Sets the <code>prefixInteger</code> property.
   */
  public XmlItem prefixInteger(Integer prefixInteger) {
    this.prefixInteger = prefixInteger;
    return this;
  }

 /**
  * Get prefixBoolean
  * @return prefixBoolean
  */
  @JsonProperty("prefix_boolean")
  public Boolean getPrefixBoolean() {
    return prefixBoolean;
  }

  /**
   * Sets the <code>prefixBoolean</code> property.
   */
  public void setPrefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
  }

  /**
   * Sets the <code>prefixBoolean</code> property.
   */
  public XmlItem prefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
    return this;
  }

 /**
  * Get prefixArray
  * @return prefixArray
  */
  @JsonProperty("prefix_array")
  public List<Integer> getPrefixArray() {
    return prefixArray;
  }

  /**
   * Sets the <code>prefixArray</code> property.
   */
  public void setPrefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
  }

  /**
   * Sets the <code>prefixArray</code> property.
   */
  public XmlItem prefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
    return this;
  }

  /**
   * Adds a new item to the <code>prefixArray</code> list.
   */
  public XmlItem addPrefixArrayItem(Integer prefixArrayItem) {
    this.prefixArray.add(prefixArrayItem);
    return this;
  }

 /**
  * Get prefixWrappedArray
  * @return prefixWrappedArray
  */
  @JsonProperty("prefix_wrapped_array")
  public List<Integer> getPrefixWrappedArray() {
    return prefixWrappedArray;
  }

  /**
   * Sets the <code>prefixWrappedArray</code> property.
   */
  public void setPrefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
  }

  /**
   * Sets the <code>prefixWrappedArray</code> property.
   */
  public XmlItem prefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
    return this;
  }

  /**
   * Adds a new item to the <code>prefixWrappedArray</code> list.
   */
  public XmlItem addPrefixWrappedArrayItem(Integer prefixWrappedArrayItem) {
    this.prefixWrappedArray.add(prefixWrappedArrayItem);
    return this;
  }

 /**
  * Get namespaceString
  * @return namespaceString
  */
  @JsonProperty("namespace_string")
  public String getNamespaceString() {
    return namespaceString;
  }

  /**
   * Sets the <code>namespaceString</code> property.
   */
  public void setNamespaceString(String namespaceString) {
    this.namespaceString = namespaceString;
  }

  /**
   * Sets the <code>namespaceString</code> property.
   */
  public XmlItem namespaceString(String namespaceString) {
    this.namespaceString = namespaceString;
    return this;
  }

 /**
  * Get namespaceNumber
  * @return namespaceNumber
  */
  @JsonProperty("namespace_number")
  public BigDecimal getNamespaceNumber() {
    return namespaceNumber;
  }

  /**
   * Sets the <code>namespaceNumber</code> property.
   */
  public void setNamespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
  }

  /**
   * Sets the <code>namespaceNumber</code> property.
   */
  public XmlItem namespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
    return this;
  }

 /**
  * Get namespaceInteger
  * @return namespaceInteger
  */
  @JsonProperty("namespace_integer")
  public Integer getNamespaceInteger() {
    return namespaceInteger;
  }

  /**
   * Sets the <code>namespaceInteger</code> property.
   */
  public void setNamespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
  }

  /**
   * Sets the <code>namespaceInteger</code> property.
   */
  public XmlItem namespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
    return this;
  }

 /**
  * Get namespaceBoolean
  * @return namespaceBoolean
  */
  @JsonProperty("namespace_boolean")
  public Boolean getNamespaceBoolean() {
    return namespaceBoolean;
  }

  /**
   * Sets the <code>namespaceBoolean</code> property.
   */
  public void setNamespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
  }

  /**
   * Sets the <code>namespaceBoolean</code> property.
   */
  public XmlItem namespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
    return this;
  }

 /**
  * Get namespaceArray
  * @return namespaceArray
  */
  @JsonProperty("namespace_array")
  public List<Integer> getNamespaceArray() {
    return namespaceArray;
  }

  /**
   * Sets the <code>namespaceArray</code> property.
   */
  public void setNamespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
  }

  /**
   * Sets the <code>namespaceArray</code> property.
   */
  public XmlItem namespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
    return this;
  }

  /**
   * Adds a new item to the <code>namespaceArray</code> list.
   */
  public XmlItem addNamespaceArrayItem(Integer namespaceArrayItem) {
    this.namespaceArray.add(namespaceArrayItem);
    return this;
  }

 /**
  * Get namespaceWrappedArray
  * @return namespaceWrappedArray
  */
  @JsonProperty("namespace_wrapped_array")
  public List<Integer> getNamespaceWrappedArray() {
    return namespaceWrappedArray;
  }

  /**
   * Sets the <code>namespaceWrappedArray</code> property.
   */
  public void setNamespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
  }

  /**
   * Sets the <code>namespaceWrappedArray</code> property.
   */
  public XmlItem namespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
    return this;
  }

  /**
   * Adds a new item to the <code>namespaceWrappedArray</code> list.
   */
  public XmlItem addNamespaceWrappedArrayItem(Integer namespaceWrappedArrayItem) {
    this.namespaceWrappedArray.add(namespaceWrappedArrayItem);
    return this;
  }

 /**
  * Get prefixNsString
  * @return prefixNsString
  */
  @JsonProperty("prefix_ns_string")
  public String getPrefixNsString() {
    return prefixNsString;
  }

  /**
   * Sets the <code>prefixNsString</code> property.
   */
  public void setPrefixNsString(String prefixNsString) {
    this.prefixNsString = prefixNsString;
  }

  /**
   * Sets the <code>prefixNsString</code> property.
   */
  public XmlItem prefixNsString(String prefixNsString) {
    this.prefixNsString = prefixNsString;
    return this;
  }

 /**
  * Get prefixNsNumber
  * @return prefixNsNumber
  */
  @JsonProperty("prefix_ns_number")
  public BigDecimal getPrefixNsNumber() {
    return prefixNsNumber;
  }

  /**
   * Sets the <code>prefixNsNumber</code> property.
   */
  public void setPrefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
  }

  /**
   * Sets the <code>prefixNsNumber</code> property.
   */
  public XmlItem prefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
    return this;
  }

 /**
  * Get prefixNsInteger
  * @return prefixNsInteger
  */
  @JsonProperty("prefix_ns_integer")
  public Integer getPrefixNsInteger() {
    return prefixNsInteger;
  }

  /**
   * Sets the <code>prefixNsInteger</code> property.
   */
  public void setPrefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
  }

  /**
   * Sets the <code>prefixNsInteger</code> property.
   */
  public XmlItem prefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
    return this;
  }

 /**
  * Get prefixNsBoolean
  * @return prefixNsBoolean
  */
  @JsonProperty("prefix_ns_boolean")
  public Boolean getPrefixNsBoolean() {
    return prefixNsBoolean;
  }

  /**
   * Sets the <code>prefixNsBoolean</code> property.
   */
  public void setPrefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
  }

  /**
   * Sets the <code>prefixNsBoolean</code> property.
   */
  public XmlItem prefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
    return this;
  }

 /**
  * Get prefixNsArray
  * @return prefixNsArray
  */
  @JsonProperty("prefix_ns_array")
  public List<Integer> getPrefixNsArray() {
    return prefixNsArray;
  }

  /**
   * Sets the <code>prefixNsArray</code> property.
   */
  public void setPrefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
  }

  /**
   * Sets the <code>prefixNsArray</code> property.
   */
  public XmlItem prefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
    return this;
  }

  /**
   * Adds a new item to the <code>prefixNsArray</code> list.
   */
  public XmlItem addPrefixNsArrayItem(Integer prefixNsArrayItem) {
    this.prefixNsArray.add(prefixNsArrayItem);
    return this;
  }

 /**
  * Get prefixNsWrappedArray
  * @return prefixNsWrappedArray
  */
  @JsonProperty("prefix_ns_wrapped_array")
  public List<Integer> getPrefixNsWrappedArray() {
    return prefixNsWrappedArray;
  }

  /**
   * Sets the <code>prefixNsWrappedArray</code> property.
   */
  public void setPrefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
    this.prefixNsWrappedArray = prefixNsWrappedArray;
  }

  /**
   * Sets the <code>prefixNsWrappedArray</code> property.
   */
  public XmlItem prefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
    this.prefixNsWrappedArray = prefixNsWrappedArray;
    return this;
  }

  /**
   * Adds a new item to the <code>prefixNsWrappedArray</code> list.
   */
  public XmlItem addPrefixNsWrappedArrayItem(Integer prefixNsWrappedArrayItem) {
    this.prefixNsWrappedArray.add(prefixNsWrappedArrayItem);
    return this;
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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

