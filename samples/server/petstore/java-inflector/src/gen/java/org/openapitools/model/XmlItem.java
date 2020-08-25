package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;





@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaInflectorServerCodegen")
public class XmlItem   {
  @JsonProperty("attribute_string")
  private String attributeString;

  @JsonProperty("attribute_number")
  private BigDecimal attributeNumber;

  @JsonProperty("attribute_integer")
  private Integer attributeInteger;

  @JsonProperty("attribute_boolean")
  private Boolean attributeBoolean;

  @JsonProperty("wrapped_array")
  private List<Integer> wrappedArray = null;

  @JsonProperty("name_string")
  private String nameString;

  @JsonProperty("name_number")
  private BigDecimal nameNumber;

  @JsonProperty("name_integer")
  private Integer nameInteger;

  @JsonProperty("name_boolean")
  private Boolean nameBoolean;

  @JsonProperty("name_array")
  private List<Integer> nameArray = null;

  @JsonProperty("name_wrapped_array")
  private List<Integer> nameWrappedArray = null;

  @JsonProperty("prefix_string")
  private String prefixString;

  @JsonProperty("prefix_number")
  private BigDecimal prefixNumber;

  @JsonProperty("prefix_integer")
  private Integer prefixInteger;

  @JsonProperty("prefix_boolean")
  private Boolean prefixBoolean;

  @JsonProperty("prefix_array")
  private List<Integer> prefixArray = null;

  @JsonProperty("prefix_wrapped_array")
  private List<Integer> prefixWrappedArray = null;

  @JsonProperty("namespace_string")
  private String namespaceString;

  @JsonProperty("namespace_number")
  private BigDecimal namespaceNumber;

  @JsonProperty("namespace_integer")
  private Integer namespaceInteger;

  @JsonProperty("namespace_boolean")
  private Boolean namespaceBoolean;

  @JsonProperty("namespace_array")
  private List<Integer> namespaceArray = null;

  @JsonProperty("namespace_wrapped_array")
  private List<Integer> namespaceWrappedArray = null;

  @JsonProperty("prefix_ns_string")
  private String prefixNsString;

  @JsonProperty("prefix_ns_number")
  private BigDecimal prefixNsNumber;

  @JsonProperty("prefix_ns_integer")
  private Integer prefixNsInteger;

  @JsonProperty("prefix_ns_boolean")
  private Boolean prefixNsBoolean;

  @JsonProperty("prefix_ns_array")
  private List<Integer> prefixNsArray = null;

  @JsonProperty("prefix_ns_wrapped_array")
  private List<Integer> prefixNsWrappedArray = null;

  /**
   **/
  public XmlItem attributeString(String attributeString) {
    this.attributeString = attributeString;
    return this;
  }

  
  @ApiModelProperty(example = "string", value = "")
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
  public XmlItem prefixNsString(String prefixNsString) {
    this.prefixNsString = prefixNsString;
    return this;
  }

  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("prefix_ns_string")
  public String getPrefixNsString() {
    return prefixNsString;
  }
  public void setPrefixNsString(String prefixNsString) {
    this.prefixNsString = prefixNsString;
  }

  /**
   **/
  public XmlItem prefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("prefix_ns_number")
  public BigDecimal getPrefixNsNumber() {
    return prefixNsNumber;
  }
  public void setPrefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
  }

  /**
   **/
  public XmlItem prefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
    return this;
  }

  
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("prefix_ns_integer")
  public Integer getPrefixNsInteger() {
    return prefixNsInteger;
  }
  public void setPrefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
  }

  /**
   **/
  public XmlItem prefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
    return this;
  }

  
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("prefix_ns_boolean")
  public Boolean getPrefixNsBoolean() {
    return prefixNsBoolean;
  }
  public void setPrefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
  }

  /**
   **/
  public XmlItem prefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_ns_array")
  public List<Integer> getPrefixNsArray() {
    return prefixNsArray;
  }
  public void setPrefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
  }

  /**
   **/
  public XmlItem prefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
    this.prefixNsWrappedArray = prefixNsWrappedArray;
    return this;
  }

  
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
        Objects.equals(prefixNsString, xmlItem.prefixNsString) &&
        Objects.equals(prefixNsNumber, xmlItem.prefixNsNumber) &&
        Objects.equals(prefixNsInteger, xmlItem.prefixNsInteger) &&
        Objects.equals(prefixNsBoolean, xmlItem.prefixNsBoolean) &&
        Objects.equals(prefixNsArray, xmlItem.prefixNsArray) &&
        Objects.equals(prefixNsWrappedArray, xmlItem.prefixNsWrappedArray);
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

