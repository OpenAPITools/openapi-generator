package org.openapitools.model;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
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
  **/
  @JsonProperty("attribute_string")
  public String getAttributeString() {
    return attributeString;
  }

  public void setAttributeString(String attributeString) {
    this.attributeString = attributeString;
  }

  public XmlItem attributeString(String attributeString) {
    this.attributeString = attributeString;
    return this;
  }

 /**
   * Get attributeNumber
   * @return attributeNumber
  **/
  @JsonProperty("attribute_number")
  public BigDecimal getAttributeNumber() {
    return attributeNumber;
  }

  public void setAttributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = attributeNumber;
  }

  public XmlItem attributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = attributeNumber;
    return this;
  }

 /**
   * Get attributeInteger
   * @return attributeInteger
  **/
  @JsonProperty("attribute_integer")
  public Integer getAttributeInteger() {
    return attributeInteger;
  }

  public void setAttributeInteger(Integer attributeInteger) {
    this.attributeInteger = attributeInteger;
  }

  public XmlItem attributeInteger(Integer attributeInteger) {
    this.attributeInteger = attributeInteger;
    return this;
  }

 /**
   * Get attributeBoolean
   * @return attributeBoolean
  **/
  @JsonProperty("attribute_boolean")
  public Boolean getAttributeBoolean() {
    return attributeBoolean;
  }

  public void setAttributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
  }

  public XmlItem attributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
    return this;
  }

 /**
   * Get wrappedArray
   * @return wrappedArray
  **/
  @JsonProperty("wrapped_array")
  public List<Integer> getWrappedArray() {
    return wrappedArray;
  }

  public void setWrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
  }

  public XmlItem wrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
    return this;
  }

  public XmlItem addWrappedArrayItem(Integer wrappedArrayItem) {
    this.wrappedArray.add(wrappedArrayItem);
    return this;
  }

 /**
   * Get nameString
   * @return nameString
  **/
  @JsonProperty("name_string")
  public String getNameString() {
    return nameString;
  }

  public void setNameString(String nameString) {
    this.nameString = nameString;
  }

  public XmlItem nameString(String nameString) {
    this.nameString = nameString;
    return this;
  }

 /**
   * Get nameNumber
   * @return nameNumber
  **/
  @JsonProperty("name_number")
  public BigDecimal getNameNumber() {
    return nameNumber;
  }

  public void setNameNumber(BigDecimal nameNumber) {
    this.nameNumber = nameNumber;
  }

  public XmlItem nameNumber(BigDecimal nameNumber) {
    this.nameNumber = nameNumber;
    return this;
  }

 /**
   * Get nameInteger
   * @return nameInteger
  **/
  @JsonProperty("name_integer")
  public Integer getNameInteger() {
    return nameInteger;
  }

  public void setNameInteger(Integer nameInteger) {
    this.nameInteger = nameInteger;
  }

  public XmlItem nameInteger(Integer nameInteger) {
    this.nameInteger = nameInteger;
    return this;
  }

 /**
   * Get nameBoolean
   * @return nameBoolean
  **/
  @JsonProperty("name_boolean")
  public Boolean getNameBoolean() {
    return nameBoolean;
  }

  public void setNameBoolean(Boolean nameBoolean) {
    this.nameBoolean = nameBoolean;
  }

  public XmlItem nameBoolean(Boolean nameBoolean) {
    this.nameBoolean = nameBoolean;
    return this;
  }

 /**
   * Get nameArray
   * @return nameArray
  **/
  @JsonProperty("name_array")
  public List<Integer> getNameArray() {
    return nameArray;
  }

  public void setNameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
  }

  public XmlItem nameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
    return this;
  }

  public XmlItem addNameArrayItem(Integer nameArrayItem) {
    this.nameArray.add(nameArrayItem);
    return this;
  }

 /**
   * Get nameWrappedArray
   * @return nameWrappedArray
  **/
  @JsonProperty("name_wrapped_array")
  public List<Integer> getNameWrappedArray() {
    return nameWrappedArray;
  }

  public void setNameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
  }

  public XmlItem nameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
    return this;
  }

  public XmlItem addNameWrappedArrayItem(Integer nameWrappedArrayItem) {
    this.nameWrappedArray.add(nameWrappedArrayItem);
    return this;
  }

 /**
   * Get prefixString
   * @return prefixString
  **/
  @JsonProperty("prefix_string")
  public String getPrefixString() {
    return prefixString;
  }

  public void setPrefixString(String prefixString) {
    this.prefixString = prefixString;
  }

  public XmlItem prefixString(String prefixString) {
    this.prefixString = prefixString;
    return this;
  }

 /**
   * Get prefixNumber
   * @return prefixNumber
  **/
  @JsonProperty("prefix_number")
  public BigDecimal getPrefixNumber() {
    return prefixNumber;
  }

  public void setPrefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = prefixNumber;
  }

  public XmlItem prefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = prefixNumber;
    return this;
  }

 /**
   * Get prefixInteger
   * @return prefixInteger
  **/
  @JsonProperty("prefix_integer")
  public Integer getPrefixInteger() {
    return prefixInteger;
  }

  public void setPrefixInteger(Integer prefixInteger) {
    this.prefixInteger = prefixInteger;
  }

  public XmlItem prefixInteger(Integer prefixInteger) {
    this.prefixInteger = prefixInteger;
    return this;
  }

 /**
   * Get prefixBoolean
   * @return prefixBoolean
  **/
  @JsonProperty("prefix_boolean")
  public Boolean getPrefixBoolean() {
    return prefixBoolean;
  }

  public void setPrefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
  }

  public XmlItem prefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
    return this;
  }

 /**
   * Get prefixArray
   * @return prefixArray
  **/
  @JsonProperty("prefix_array")
  public List<Integer> getPrefixArray() {
    return prefixArray;
  }

  public void setPrefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
  }

  public XmlItem prefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
    return this;
  }

  public XmlItem addPrefixArrayItem(Integer prefixArrayItem) {
    this.prefixArray.add(prefixArrayItem);
    return this;
  }

 /**
   * Get prefixWrappedArray
   * @return prefixWrappedArray
  **/
  @JsonProperty("prefix_wrapped_array")
  public List<Integer> getPrefixWrappedArray() {
    return prefixWrappedArray;
  }

  public void setPrefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
  }

  public XmlItem prefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
    return this;
  }

  public XmlItem addPrefixWrappedArrayItem(Integer prefixWrappedArrayItem) {
    this.prefixWrappedArray.add(prefixWrappedArrayItem);
    return this;
  }

 /**
   * Get namespaceString
   * @return namespaceString
  **/
  @JsonProperty("namespace_string")
  public String getNamespaceString() {
    return namespaceString;
  }

  public void setNamespaceString(String namespaceString) {
    this.namespaceString = namespaceString;
  }

  public XmlItem namespaceString(String namespaceString) {
    this.namespaceString = namespaceString;
    return this;
  }

 /**
   * Get namespaceNumber
   * @return namespaceNumber
  **/
  @JsonProperty("namespace_number")
  public BigDecimal getNamespaceNumber() {
    return namespaceNumber;
  }

  public void setNamespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
  }

  public XmlItem namespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
    return this;
  }

 /**
   * Get namespaceInteger
   * @return namespaceInteger
  **/
  @JsonProperty("namespace_integer")
  public Integer getNamespaceInteger() {
    return namespaceInteger;
  }

  public void setNamespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
  }

  public XmlItem namespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
    return this;
  }

 /**
   * Get namespaceBoolean
   * @return namespaceBoolean
  **/
  @JsonProperty("namespace_boolean")
  public Boolean getNamespaceBoolean() {
    return namespaceBoolean;
  }

  public void setNamespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
  }

  public XmlItem namespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
    return this;
  }

 /**
   * Get namespaceArray
   * @return namespaceArray
  **/
  @JsonProperty("namespace_array")
  public List<Integer> getNamespaceArray() {
    return namespaceArray;
  }

  public void setNamespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
  }

  public XmlItem namespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
    return this;
  }

  public XmlItem addNamespaceArrayItem(Integer namespaceArrayItem) {
    this.namespaceArray.add(namespaceArrayItem);
    return this;
  }

 /**
   * Get namespaceWrappedArray
   * @return namespaceWrappedArray
  **/
  @JsonProperty("namespace_wrapped_array")
  public List<Integer> getNamespaceWrappedArray() {
    return namespaceWrappedArray;
  }

  public void setNamespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
  }

  public XmlItem namespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
    return this;
  }

  public XmlItem addNamespaceWrappedArrayItem(Integer namespaceWrappedArrayItem) {
    this.namespaceWrappedArray.add(namespaceWrappedArrayItem);
    return this;
  }

 /**
   * Get prefixNsString
   * @return prefixNsString
  **/
  @JsonProperty("prefix_ns_string")
  public String getPrefixNsString() {
    return prefixNsString;
  }

  public void setPrefixNsString(String prefixNsString) {
    this.prefixNsString = prefixNsString;
  }

  public XmlItem prefixNsString(String prefixNsString) {
    this.prefixNsString = prefixNsString;
    return this;
  }

 /**
   * Get prefixNsNumber
   * @return prefixNsNumber
  **/
  @JsonProperty("prefix_ns_number")
  public BigDecimal getPrefixNsNumber() {
    return prefixNsNumber;
  }

  public void setPrefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
  }

  public XmlItem prefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
    return this;
  }

 /**
   * Get prefixNsInteger
   * @return prefixNsInteger
  **/
  @JsonProperty("prefix_ns_integer")
  public Integer getPrefixNsInteger() {
    return prefixNsInteger;
  }

  public void setPrefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
  }

  public XmlItem prefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
    return this;
  }

 /**
   * Get prefixNsBoolean
   * @return prefixNsBoolean
  **/
  @JsonProperty("prefix_ns_boolean")
  public Boolean getPrefixNsBoolean() {
    return prefixNsBoolean;
  }

  public void setPrefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
  }

  public XmlItem prefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
    return this;
  }

 /**
   * Get prefixNsArray
   * @return prefixNsArray
  **/
  @JsonProperty("prefix_ns_array")
  public List<Integer> getPrefixNsArray() {
    return prefixNsArray;
  }

  public void setPrefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
  }

  public XmlItem prefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
    return this;
  }

  public XmlItem addPrefixNsArrayItem(Integer prefixNsArrayItem) {
    this.prefixNsArray.add(prefixNsArrayItem);
    return this;
  }

 /**
   * Get prefixNsWrappedArray
   * @return prefixNsWrappedArray
  **/
  @JsonProperty("prefix_ns_wrapped_array")
  public List<Integer> getPrefixNsWrappedArray() {
    return prefixNsWrappedArray;
  }

  public void setPrefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
    this.prefixNsWrappedArray = prefixNsWrappedArray;
  }

  public XmlItem prefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
    this.prefixNsWrappedArray = prefixNsWrappedArray;
    return this;
  }

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

