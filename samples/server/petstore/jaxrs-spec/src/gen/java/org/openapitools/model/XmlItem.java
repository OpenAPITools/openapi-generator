package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;



public class XmlItem  implements Serializable {
  
  private @Valid String attributeString;
  private @Valid BigDecimal attributeNumber;
  private @Valid Integer attributeInteger;
  private @Valid Boolean attributeBoolean;
  private @Valid List<Integer> wrappedArray = new ArrayList<Integer>();
  private @Valid String nameString;
  private @Valid BigDecimal nameNumber;
  private @Valid Integer nameInteger;
  private @Valid Boolean nameBoolean;
  private @Valid List<Integer> nameArray = new ArrayList<Integer>();
  private @Valid List<Integer> nameWrappedArray = new ArrayList<Integer>();
  private @Valid String prefixString;
  private @Valid BigDecimal prefixNumber;
  private @Valid Integer prefixInteger;
  private @Valid Boolean prefixBoolean;
  private @Valid List<Integer> prefixArray = new ArrayList<Integer>();
  private @Valid List<Integer> prefixWrappedArray = new ArrayList<Integer>();
  private @Valid String namespaceString;
  private @Valid BigDecimal namespaceNumber;
  private @Valid Integer namespaceInteger;
  private @Valid Boolean namespaceBoolean;
  private @Valid List<Integer> namespaceArray = new ArrayList<Integer>();
  private @Valid List<Integer> namespaceWrappedArray = new ArrayList<Integer>();
  private @Valid String prefixNamespaceString;
  private @Valid BigDecimal prefixNamespaceNumber;
  private @Valid Integer prefixNamespaceInteger;
  private @Valid Boolean prefixNamespaceBoolean;
  private @Valid List<Integer> prefixNamespaceArray = new ArrayList<Integer>();
  private @Valid List<Integer> prefixNamespaceWrappedArray = new ArrayList<Integer>();

  /**
   **/
  public XmlItem attributeString(String attributeString) {
    this.attributeString = attributeString;
    return this;
  }

  
  @ApiModelProperty(example = "a_string", value = "")
  @JsonProperty("attribute_string")
  public String getAttributeString() {
    return attributeString;
  }
  public void setAttributeString(String attributeString) {
    this.attributeString = attributeString;
  }

  /**
   **/
  public XmlItem attributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = attributeNumber;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("attribute_number")
  public BigDecimal getAttributeNumber() {
    return attributeNumber;
  }
  public void setAttributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = attributeNumber;
  }

  /**
   **/
  public XmlItem attributeInteger(Integer attributeInteger) {
    this.attributeInteger = attributeInteger;
    return this;
  }

  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("attribute_integer")
  public Integer getAttributeInteger() {
    return attributeInteger;
  }
  public void setAttributeInteger(Integer attributeInteger) {
    this.attributeInteger = attributeInteger;
  }

  /**
   **/
  public XmlItem attributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
    return this;
  }

  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("attribute_boolean")
  public Boolean getAttributeBoolean() {
    return attributeBoolean;
  }
  public void setAttributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
  }

  /**
   **/
  public XmlItem wrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("wrapped_array")
  public List<Integer> getWrappedArray() {
    return wrappedArray;
  }
  public void setWrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
  }

  /**
   **/
  public XmlItem nameString(String nameString) {
    this.nameString = nameString;
    return this;
  }

  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("name_string")
  public String getNameString() {
    return nameString;
  }
  public void setNameString(String nameString) {
    this.nameString = nameString;
  }

  /**
   **/
  public XmlItem nameNumber(BigDecimal nameNumber) {
    this.nameNumber = nameNumber;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("name_number")
  public BigDecimal getNameNumber() {
    return nameNumber;
  }
  public void setNameNumber(BigDecimal nameNumber) {
    this.nameNumber = nameNumber;
  }

  /**
   **/
  public XmlItem nameInteger(Integer nameInteger) {
    this.nameInteger = nameInteger;
    return this;
  }

  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("name_integer")
  public Integer getNameInteger() {
    return nameInteger;
  }
  public void setNameInteger(Integer nameInteger) {
    this.nameInteger = nameInteger;
  }

  /**
   **/
  public XmlItem nameBoolean(Boolean nameBoolean) {
    this.nameBoolean = nameBoolean;
    return this;
  }

  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("name_boolean")
  public Boolean getNameBoolean() {
    return nameBoolean;
  }
  public void setNameBoolean(Boolean nameBoolean) {
    this.nameBoolean = nameBoolean;
  }

  /**
   **/
  public XmlItem nameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("name_array")
  public List<Integer> getNameArray() {
    return nameArray;
  }
  public void setNameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
  }

  /**
   **/
  public XmlItem nameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("name_wrapped_array")
  public List<Integer> getNameWrappedArray() {
    return nameWrappedArray;
  }
  public void setNameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
  }

  /**
   **/
  public XmlItem prefixString(String prefixString) {
    this.prefixString = prefixString;
    return this;
  }

  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("prefix_string")
  public String getPrefixString() {
    return prefixString;
  }
  public void setPrefixString(String prefixString) {
    this.prefixString = prefixString;
  }

  /**
   **/
  public XmlItem prefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = prefixNumber;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("prefix_number")
  public BigDecimal getPrefixNumber() {
    return prefixNumber;
  }
  public void setPrefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = prefixNumber;
  }

  /**
   **/
  public XmlItem prefixInteger(Integer prefixInteger) {
    this.prefixInteger = prefixInteger;
    return this;
  }

  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("prefix_integer")
  public Integer getPrefixInteger() {
    return prefixInteger;
  }
  public void setPrefixInteger(Integer prefixInteger) {
    this.prefixInteger = prefixInteger;
  }

  /**
   **/
  public XmlItem prefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
    return this;
  }

  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("prefix_boolean")
  public Boolean getPrefixBoolean() {
    return prefixBoolean;
  }
  public void setPrefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
  }

  /**
   **/
  public XmlItem prefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_array")
  public List<Integer> getPrefixArray() {
    return prefixArray;
  }
  public void setPrefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
  }

  /**
   **/
  public XmlItem prefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_wrapped_array")
  public List<Integer> getPrefixWrappedArray() {
    return prefixWrappedArray;
  }
  public void setPrefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
  }

  /**
   **/
  public XmlItem namespaceString(String namespaceString) {
    this.namespaceString = namespaceString;
    return this;
  }

  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("namespace_string")
  public String getNamespaceString() {
    return namespaceString;
  }
  public void setNamespaceString(String namespaceString) {
    this.namespaceString = namespaceString;
  }

  /**
   **/
  public XmlItem namespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("namespace_number")
  public BigDecimal getNamespaceNumber() {
    return namespaceNumber;
  }
  public void setNamespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
  }

  /**
   **/
  public XmlItem namespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
    return this;
  }

  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("namespace_integer")
  public Integer getNamespaceInteger() {
    return namespaceInteger;
  }
  public void setNamespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
  }

  /**
   **/
  public XmlItem namespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
    return this;
  }

  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("namespace_boolean")
  public Boolean getNamespaceBoolean() {
    return namespaceBoolean;
  }
  public void setNamespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
  }

  /**
   **/
  public XmlItem namespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("namespace_array")
  public List<Integer> getNamespaceArray() {
    return namespaceArray;
  }
  public void setNamespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
  }

  /**
   **/
  public XmlItem namespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("namespace_wrapped_array")
  public List<Integer> getNamespaceWrappedArray() {
    return namespaceWrappedArray;
  }
  public void setNamespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
  }

  /**
   **/
  public XmlItem prefixNamespaceString(String prefixNamespaceString) {
    this.prefixNamespaceString = prefixNamespaceString;
    return this;
  }

  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("prefix_namespace_string")
  public String getPrefixNamespaceString() {
    return prefixNamespaceString;
  }
  public void setPrefixNamespaceString(String prefixNamespaceString) {
    this.prefixNamespaceString = prefixNamespaceString;
  }

  /**
   **/
  public XmlItem prefixNamespaceNumber(BigDecimal prefixNamespaceNumber) {
    this.prefixNamespaceNumber = prefixNamespaceNumber;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("prefix_namespace_number")
  public BigDecimal getPrefixNamespaceNumber() {
    return prefixNamespaceNumber;
  }
  public void setPrefixNamespaceNumber(BigDecimal prefixNamespaceNumber) {
    this.prefixNamespaceNumber = prefixNamespaceNumber;
  }

  /**
   **/
  public XmlItem prefixNamespaceInteger(Integer prefixNamespaceInteger) {
    this.prefixNamespaceInteger = prefixNamespaceInteger;
    return this;
  }

  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("prefix_namespace_integer")
  public Integer getPrefixNamespaceInteger() {
    return prefixNamespaceInteger;
  }
  public void setPrefixNamespaceInteger(Integer prefixNamespaceInteger) {
    this.prefixNamespaceInteger = prefixNamespaceInteger;
  }

  /**
   **/
  public XmlItem prefixNamespaceBoolean(Boolean prefixNamespaceBoolean) {
    this.prefixNamespaceBoolean = prefixNamespaceBoolean;
    return this;
  }

  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("prefix_namespace_boolean")
  public Boolean getPrefixNamespaceBoolean() {
    return prefixNamespaceBoolean;
  }
  public void setPrefixNamespaceBoolean(Boolean prefixNamespaceBoolean) {
    this.prefixNamespaceBoolean = prefixNamespaceBoolean;
  }

  /**
   **/
  public XmlItem prefixNamespaceArray(List<Integer> prefixNamespaceArray) {
    this.prefixNamespaceArray = prefixNamespaceArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_namespace_array")
  public List<Integer> getPrefixNamespaceArray() {
    return prefixNamespaceArray;
  }
  public void setPrefixNamespaceArray(List<Integer> prefixNamespaceArray) {
    this.prefixNamespaceArray = prefixNamespaceArray;
  }

  /**
   **/
  public XmlItem prefixNamespaceWrappedArray(List<Integer> prefixNamespaceWrappedArray) {
    this.prefixNamespaceWrappedArray = prefixNamespaceWrappedArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_namespace_wrapped_array")
  public List<Integer> getPrefixNamespaceWrappedArray() {
    return prefixNamespaceWrappedArray;
  }
  public void setPrefixNamespaceWrappedArray(List<Integer> prefixNamespaceWrappedArray) {
    this.prefixNamespaceWrappedArray = prefixNamespaceWrappedArray;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    XmlItem xmlItem = (XmlItem) o;
    return Objects.equals(attributeString, xmlItem.attributeString) &&
        Objects.equals(attributeNumber, xmlItem.attributeNumber) &&
        Objects.equals(attributeInteger, xmlItem.attributeInteger) &&
        Objects.equals(attributeBoolean, xmlItem.attributeBoolean) &&
        Objects.equals(wrappedArray, xmlItem.wrappedArray) &&
        Objects.equals(nameString, xmlItem.nameString) &&
        Objects.equals(nameNumber, xmlItem.nameNumber) &&
        Objects.equals(nameInteger, xmlItem.nameInteger) &&
        Objects.equals(nameBoolean, xmlItem.nameBoolean) &&
        Objects.equals(nameArray, xmlItem.nameArray) &&
        Objects.equals(nameWrappedArray, xmlItem.nameWrappedArray) &&
        Objects.equals(prefixString, xmlItem.prefixString) &&
        Objects.equals(prefixNumber, xmlItem.prefixNumber) &&
        Objects.equals(prefixInteger, xmlItem.prefixInteger) &&
        Objects.equals(prefixBoolean, xmlItem.prefixBoolean) &&
        Objects.equals(prefixArray, xmlItem.prefixArray) &&
        Objects.equals(prefixWrappedArray, xmlItem.prefixWrappedArray) &&
        Objects.equals(namespaceString, xmlItem.namespaceString) &&
        Objects.equals(namespaceNumber, xmlItem.namespaceNumber) &&
        Objects.equals(namespaceInteger, xmlItem.namespaceInteger) &&
        Objects.equals(namespaceBoolean, xmlItem.namespaceBoolean) &&
        Objects.equals(namespaceArray, xmlItem.namespaceArray) &&
        Objects.equals(namespaceWrappedArray, xmlItem.namespaceWrappedArray) &&
        Objects.equals(prefixNamespaceString, xmlItem.prefixNamespaceString) &&
        Objects.equals(prefixNamespaceNumber, xmlItem.prefixNamespaceNumber) &&
        Objects.equals(prefixNamespaceInteger, xmlItem.prefixNamespaceInteger) &&
        Objects.equals(prefixNamespaceBoolean, xmlItem.prefixNamespaceBoolean) &&
        Objects.equals(prefixNamespaceArray, xmlItem.prefixNamespaceArray) &&
        Objects.equals(prefixNamespaceWrappedArray, xmlItem.prefixNamespaceWrappedArray);
  }

  @Override
  public int hashCode() {
    return Objects.hash(attributeString, attributeNumber, attributeInteger, attributeBoolean, wrappedArray, nameString, nameNumber, nameInteger, nameBoolean, nameArray, nameWrappedArray, prefixString, prefixNumber, prefixInteger, prefixBoolean, prefixArray, prefixWrappedArray, namespaceString, namespaceNumber, namespaceInteger, namespaceBoolean, namespaceArray, namespaceWrappedArray, prefixNamespaceString, prefixNamespaceNumber, prefixNamespaceInteger, prefixNamespaceBoolean, prefixNamespaceArray, prefixNamespaceWrappedArray);
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
    sb.append("    prefixNamespaceString: ").append(toIndentedString(prefixNamespaceString)).append("\n");
    sb.append("    prefixNamespaceNumber: ").append(toIndentedString(prefixNamespaceNumber)).append("\n");
    sb.append("    prefixNamespaceInteger: ").append(toIndentedString(prefixNamespaceInteger)).append("\n");
    sb.append("    prefixNamespaceBoolean: ").append(toIndentedString(prefixNamespaceBoolean)).append("\n");
    sb.append("    prefixNamespaceArray: ").append(toIndentedString(prefixNamespaceArray)).append("\n");
    sb.append("    prefixNamespaceWrappedArray: ").append(toIndentedString(prefixNamespaceWrappedArray)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

