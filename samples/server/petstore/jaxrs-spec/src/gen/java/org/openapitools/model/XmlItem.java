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
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("XmlItem")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public class XmlItem  implements Serializable {
  
  private @Valid String attributeString;
  private @Valid BigDecimal attributeNumber;
  private @Valid Integer attributeInteger;
  private @Valid Boolean attributeBoolean;
  private @Valid List<Integer> wrappedArray = new ArrayList<>();
  private @Valid String nameString;
  private @Valid BigDecimal nameNumber;
  private @Valid Integer nameInteger;
  private @Valid Boolean nameBoolean;
  private @Valid List<Integer> nameArray = new ArrayList<>();
  private @Valid List<Integer> nameWrappedArray = new ArrayList<>();
  private @Valid String prefixString;
  private @Valid BigDecimal prefixNumber;
  private @Valid Integer prefixInteger;
  private @Valid Boolean prefixBoolean;
  private @Valid List<Integer> prefixArray = new ArrayList<>();
  private @Valid List<Integer> prefixWrappedArray = new ArrayList<>();
  private @Valid String namespaceString;
  private @Valid BigDecimal namespaceNumber;
  private @Valid Integer namespaceInteger;
  private @Valid Boolean namespaceBoolean;
  private @Valid List<Integer> namespaceArray = new ArrayList<>();
  private @Valid List<Integer> namespaceWrappedArray = new ArrayList<>();
  private @Valid String prefixNsString;
  private @Valid BigDecimal prefixNsNumber;
  private @Valid Integer prefixNsInteger;
  private @Valid Boolean prefixNsBoolean;
  private @Valid List<Integer> prefixNsArray = new ArrayList<>();
  private @Valid List<Integer> prefixNsWrappedArray = new ArrayList<>();

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

  @JsonProperty("attribute_string")
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

  @JsonProperty("attribute_number")
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

  @JsonProperty("attribute_integer")
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

  @JsonProperty("attribute_boolean")
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

  @JsonProperty("wrapped_array")
  public void setWrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
  }

  public XmlItem addWrappedArrayItem(Integer wrappedArrayItem) {
    if (this.wrappedArray == null) {
      this.wrappedArray = new ArrayList<>();
    }

    this.wrappedArray.add(wrappedArrayItem);
    return this;
  }

  public XmlItem removeWrappedArrayItem(Integer wrappedArrayItem) {
    if (wrappedArrayItem != null && this.wrappedArray != null) {
      this.wrappedArray.remove(wrappedArrayItem);
    }

    return this;
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

  @JsonProperty("name_string")
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

  @JsonProperty("name_number")
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

  @JsonProperty("name_integer")
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

  @JsonProperty("name_boolean")
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

  @JsonProperty("name_array")
  public void setNameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
  }

  public XmlItem addNameArrayItem(Integer nameArrayItem) {
    if (this.nameArray == null) {
      this.nameArray = new ArrayList<>();
    }

    this.nameArray.add(nameArrayItem);
    return this;
  }

  public XmlItem removeNameArrayItem(Integer nameArrayItem) {
    if (nameArrayItem != null && this.nameArray != null) {
      this.nameArray.remove(nameArrayItem);
    }

    return this;
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

  @JsonProperty("name_wrapped_array")
  public void setNameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
  }

  public XmlItem addNameWrappedArrayItem(Integer nameWrappedArrayItem) {
    if (this.nameWrappedArray == null) {
      this.nameWrappedArray = new ArrayList<>();
    }

    this.nameWrappedArray.add(nameWrappedArrayItem);
    return this;
  }

  public XmlItem removeNameWrappedArrayItem(Integer nameWrappedArrayItem) {
    if (nameWrappedArrayItem != null && this.nameWrappedArray != null) {
      this.nameWrappedArray.remove(nameWrappedArrayItem);
    }

    return this;
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

  @JsonProperty("prefix_string")
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

  @JsonProperty("prefix_number")
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

  @JsonProperty("prefix_integer")
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

  @JsonProperty("prefix_boolean")
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

  @JsonProperty("prefix_array")
  public void setPrefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
  }

  public XmlItem addPrefixArrayItem(Integer prefixArrayItem) {
    if (this.prefixArray == null) {
      this.prefixArray = new ArrayList<>();
    }

    this.prefixArray.add(prefixArrayItem);
    return this;
  }

  public XmlItem removePrefixArrayItem(Integer prefixArrayItem) {
    if (prefixArrayItem != null && this.prefixArray != null) {
      this.prefixArray.remove(prefixArrayItem);
    }

    return this;
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

  @JsonProperty("prefix_wrapped_array")
  public void setPrefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
  }

  public XmlItem addPrefixWrappedArrayItem(Integer prefixWrappedArrayItem) {
    if (this.prefixWrappedArray == null) {
      this.prefixWrappedArray = new ArrayList<>();
    }

    this.prefixWrappedArray.add(prefixWrappedArrayItem);
    return this;
  }

  public XmlItem removePrefixWrappedArrayItem(Integer prefixWrappedArrayItem) {
    if (prefixWrappedArrayItem != null && this.prefixWrappedArray != null) {
      this.prefixWrappedArray.remove(prefixWrappedArrayItem);
    }

    return this;
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

  @JsonProperty("namespace_string")
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

  @JsonProperty("namespace_number")
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

  @JsonProperty("namespace_integer")
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

  @JsonProperty("namespace_boolean")
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

  @JsonProperty("namespace_array")
  public void setNamespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
  }

  public XmlItem addNamespaceArrayItem(Integer namespaceArrayItem) {
    if (this.namespaceArray == null) {
      this.namespaceArray = new ArrayList<>();
    }

    this.namespaceArray.add(namespaceArrayItem);
    return this;
  }

  public XmlItem removeNamespaceArrayItem(Integer namespaceArrayItem) {
    if (namespaceArrayItem != null && this.namespaceArray != null) {
      this.namespaceArray.remove(namespaceArrayItem);
    }

    return this;
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

  @JsonProperty("namespace_wrapped_array")
  public void setNamespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
  }

  public XmlItem addNamespaceWrappedArrayItem(Integer namespaceWrappedArrayItem) {
    if (this.namespaceWrappedArray == null) {
      this.namespaceWrappedArray = new ArrayList<>();
    }

    this.namespaceWrappedArray.add(namespaceWrappedArrayItem);
    return this;
  }

  public XmlItem removeNamespaceWrappedArrayItem(Integer namespaceWrappedArrayItem) {
    if (namespaceWrappedArrayItem != null && this.namespaceWrappedArray != null) {
      this.namespaceWrappedArray.remove(namespaceWrappedArrayItem);
    }

    return this;
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

  @JsonProperty("prefix_ns_string")
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

  @JsonProperty("prefix_ns_number")
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

  @JsonProperty("prefix_ns_integer")
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

  @JsonProperty("prefix_ns_boolean")
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

  @JsonProperty("prefix_ns_array")
  public void setPrefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
  }

  public XmlItem addPrefixNsArrayItem(Integer prefixNsArrayItem) {
    if (this.prefixNsArray == null) {
      this.prefixNsArray = new ArrayList<>();
    }

    this.prefixNsArray.add(prefixNsArrayItem);
    return this;
  }

  public XmlItem removePrefixNsArrayItem(Integer prefixNsArrayItem) {
    if (prefixNsArrayItem != null && this.prefixNsArray != null) {
      this.prefixNsArray.remove(prefixNsArrayItem);
    }

    return this;
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

  @JsonProperty("prefix_ns_wrapped_array")
  public void setPrefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
    this.prefixNsWrappedArray = prefixNsWrappedArray;
  }

  public XmlItem addPrefixNsWrappedArrayItem(Integer prefixNsWrappedArrayItem) {
    if (this.prefixNsWrappedArray == null) {
      this.prefixNsWrappedArray = new ArrayList<>();
    }

    this.prefixNsWrappedArray.add(prefixNsWrappedArrayItem);
    return this;
  }

  public XmlItem removePrefixNsWrappedArrayItem(Integer prefixNsWrappedArrayItem) {
    if (prefixNsWrappedArrayItem != null && this.prefixNsWrappedArray != null) {
      this.prefixNsWrappedArray.remove(prefixNsWrappedArrayItem);
    }

    return this;
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

