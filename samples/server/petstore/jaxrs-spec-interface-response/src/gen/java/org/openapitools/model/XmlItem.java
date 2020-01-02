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
  private @Valid String prefixNsString;
  private @Valid BigDecimal prefixNsNumber;
  private @Valid Integer prefixNsInteger;
  private @Valid Boolean prefixNsBoolean;
  private @Valid List<Integer> prefixNsArray = new ArrayList<Integer>();
  private @Valid List<Integer> prefixNsWrappedArray = new ArrayList<Integer>();

  public XmlItem(String attributeString, BigDecimal attributeNumber, Integer attributeInteger, Boolean attributeBoolean, List<Integer> wrappedArray, String nameString, BigDecimal nameNumber, Integer nameInteger, Boolean nameBoolean, List<Integer> nameArray, List<Integer> nameWrappedArray, String prefixString, BigDecimal prefixNumber, Integer prefixInteger, Boolean prefixBoolean, List<Integer> prefixArray, List<Integer> prefixWrappedArray, String namespaceString, BigDecimal namespaceNumber, Integer namespaceInteger, Boolean namespaceBoolean, List<Integer> namespaceArray, List<Integer> namespaceWrappedArray, String prefixNsString, BigDecimal prefixNsNumber, Integer prefixNsInteger, Boolean prefixNsBoolean, List<Integer> prefixNsArray, List<Integer> prefixNsWrappedArray) {
    this.attributeString = attributeString;
    this.attributeNumber = attributeNumber;
    this.attributeInteger = attributeInteger;
    this.attributeBoolean = attributeBoolean;
    this.wrappedArray = wrappedArray;
    this.nameString = nameString;
    this.nameNumber = nameNumber;
    this.nameInteger = nameInteger;
    this.nameBoolean = nameBoolean;
    this.nameArray = nameArray;
    this.nameWrappedArray = nameWrappedArray;
    this.prefixString = prefixString;
    this.prefixNumber = prefixNumber;
    this.prefixInteger = prefixInteger;
    this.prefixBoolean = prefixBoolean;
    this.prefixArray = prefixArray;
    this.prefixWrappedArray = prefixWrappedArray;
    this.namespaceString = namespaceString;
    this.namespaceNumber = namespaceNumber;
    this.namespaceInteger = namespaceInteger;
    this.namespaceBoolean = namespaceBoolean;
    this.namespaceArray = namespaceArray;
    this.namespaceWrappedArray = namespaceWrappedArray;
    this.prefixNsString = prefixNsString;
    this.prefixNsNumber = prefixNsNumber;
    this.prefixNsInteger = prefixNsInteger;
    this.prefixNsBoolean = prefixNsBoolean;
    this.prefixNsArray = prefixNsArray;
    this.prefixNsWrappedArray = prefixNsWrappedArray;
  }

  
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("attribute_string")
  public String getAttributeString() {
    return attributeString;
  }

  public void setAttributeString(String attributeString) {
    this.attributeString = attributeString;
  }
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("attribute_number")
  public BigDecimal getAttributeNumber() {
    return attributeNumber;
  }

  public void setAttributeNumber(BigDecimal attributeNumber) {
    this.attributeNumber = attributeNumber;
  }
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("attribute_integer")
  public Integer getAttributeInteger() {
    return attributeInteger;
  }

  public void setAttributeInteger(Integer attributeInteger) {
    this.attributeInteger = attributeInteger;
  }
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("attribute_boolean")
  public Boolean getAttributeBoolean() {
    return attributeBoolean;
  }

  public void setAttributeBoolean(Boolean attributeBoolean) {
    this.attributeBoolean = attributeBoolean;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("wrapped_array")
  public List<Integer> getWrappedArray() {
    return wrappedArray;
  }

  public void setWrappedArray(List<Integer> wrappedArray) {
    this.wrappedArray = wrappedArray;
  }
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("name_string")
  public String getNameString() {
    return nameString;
  }

  public void setNameString(String nameString) {
    this.nameString = nameString;
  }
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("name_number")
  public BigDecimal getNameNumber() {
    return nameNumber;
  }

  public void setNameNumber(BigDecimal nameNumber) {
    this.nameNumber = nameNumber;
  }
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("name_integer")
  public Integer getNameInteger() {
    return nameInteger;
  }

  public void setNameInteger(Integer nameInteger) {
    this.nameInteger = nameInteger;
  }
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("name_boolean")
  public Boolean getNameBoolean() {
    return nameBoolean;
  }

  public void setNameBoolean(Boolean nameBoolean) {
    this.nameBoolean = nameBoolean;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("name_array")
  public List<Integer> getNameArray() {
    return nameArray;
  }

  public void setNameArray(List<Integer> nameArray) {
    this.nameArray = nameArray;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("name_wrapped_array")
  public List<Integer> getNameWrappedArray() {
    return nameWrappedArray;
  }

  public void setNameWrappedArray(List<Integer> nameWrappedArray) {
    this.nameWrappedArray = nameWrappedArray;
  }
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("prefix_string")
  public String getPrefixString() {
    return prefixString;
  }

  public void setPrefixString(String prefixString) {
    this.prefixString = prefixString;
  }
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("prefix_number")
  public BigDecimal getPrefixNumber() {
    return prefixNumber;
  }

  public void setPrefixNumber(BigDecimal prefixNumber) {
    this.prefixNumber = prefixNumber;
  }
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("prefix_integer")
  public Integer getPrefixInteger() {
    return prefixInteger;
  }

  public void setPrefixInteger(Integer prefixInteger) {
    this.prefixInteger = prefixInteger;
  }
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("prefix_boolean")
  public Boolean getPrefixBoolean() {
    return prefixBoolean;
  }

  public void setPrefixBoolean(Boolean prefixBoolean) {
    this.prefixBoolean = prefixBoolean;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_array")
  public List<Integer> getPrefixArray() {
    return prefixArray;
  }

  public void setPrefixArray(List<Integer> prefixArray) {
    this.prefixArray = prefixArray;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_wrapped_array")
  public List<Integer> getPrefixWrappedArray() {
    return prefixWrappedArray;
  }

  public void setPrefixWrappedArray(List<Integer> prefixWrappedArray) {
    this.prefixWrappedArray = prefixWrappedArray;
  }
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("namespace_string")
  public String getNamespaceString() {
    return namespaceString;
  }

  public void setNamespaceString(String namespaceString) {
    this.namespaceString = namespaceString;
  }
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("namespace_number")
  public BigDecimal getNamespaceNumber() {
    return namespaceNumber;
  }

  public void setNamespaceNumber(BigDecimal namespaceNumber) {
    this.namespaceNumber = namespaceNumber;
  }
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("namespace_integer")
  public Integer getNamespaceInteger() {
    return namespaceInteger;
  }

  public void setNamespaceInteger(Integer namespaceInteger) {
    this.namespaceInteger = namespaceInteger;
  }
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("namespace_boolean")
  public Boolean getNamespaceBoolean() {
    return namespaceBoolean;
  }

  public void setNamespaceBoolean(Boolean namespaceBoolean) {
    this.namespaceBoolean = namespaceBoolean;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("namespace_array")
  public List<Integer> getNamespaceArray() {
    return namespaceArray;
  }

  public void setNamespaceArray(List<Integer> namespaceArray) {
    this.namespaceArray = namespaceArray;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("namespace_wrapped_array")
  public List<Integer> getNamespaceWrappedArray() {
    return namespaceWrappedArray;
  }

  public void setNamespaceWrappedArray(List<Integer> namespaceWrappedArray) {
    this.namespaceWrappedArray = namespaceWrappedArray;
  }
  @ApiModelProperty(example = "string", value = "")
  @JsonProperty("prefix_ns_string")
  public String getPrefixNsString() {
    return prefixNsString;
  }

  public void setPrefixNsString(String prefixNsString) {
    this.prefixNsString = prefixNsString;
  }
  @ApiModelProperty(example = "1.234", value = "")
  @JsonProperty("prefix_ns_number")
  public BigDecimal getPrefixNsNumber() {
    return prefixNsNumber;
  }

  public void setPrefixNsNumber(BigDecimal prefixNsNumber) {
    this.prefixNsNumber = prefixNsNumber;
  }
  @ApiModelProperty(example = "-2", value = "")
  @JsonProperty("prefix_ns_integer")
  public Integer getPrefixNsInteger() {
    return prefixNsInteger;
  }

  public void setPrefixNsInteger(Integer prefixNsInteger) {
    this.prefixNsInteger = prefixNsInteger;
  }
  @ApiModelProperty(example = "true", value = "")
  @JsonProperty("prefix_ns_boolean")
  public Boolean getPrefixNsBoolean() {
    return prefixNsBoolean;
  }

  public void setPrefixNsBoolean(Boolean prefixNsBoolean) {
    this.prefixNsBoolean = prefixNsBoolean;
  }
  @ApiModelProperty(value = "")
  @JsonProperty("prefix_ns_array")
  public List<Integer> getPrefixNsArray() {
    return prefixNsArray;
  }

  public void setPrefixNsArray(List<Integer> prefixNsArray) {
    this.prefixNsArray = prefixNsArray;
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
  public boolean equals(java.lang.Object o) {
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }

  public static Builder builder() {
    return new Builder();
  }

  public static class Builder {
    private String attributeString;
    private BigDecimal attributeNumber;
    private Integer attributeInteger;
    private Boolean attributeBoolean;
    private List<Integer> wrappedArray = new ArrayList<Integer>();
    private String nameString;
    private BigDecimal nameNumber;
    private Integer nameInteger;
    private Boolean nameBoolean;
    private List<Integer> nameArray = new ArrayList<Integer>();
    private List<Integer> nameWrappedArray = new ArrayList<Integer>();
    private String prefixString;
    private BigDecimal prefixNumber;
    private Integer prefixInteger;
    private Boolean prefixBoolean;
    private List<Integer> prefixArray = new ArrayList<Integer>();
    private List<Integer> prefixWrappedArray = new ArrayList<Integer>();
    private String namespaceString;
    private BigDecimal namespaceNumber;
    private Integer namespaceInteger;
    private Boolean namespaceBoolean;
    private List<Integer> namespaceArray = new ArrayList<Integer>();
    private List<Integer> namespaceWrappedArray = new ArrayList<Integer>();
    private String prefixNsString;
    private BigDecimal prefixNsNumber;
    private Integer prefixNsInteger;
    private Boolean prefixNsBoolean;
    private List<Integer> prefixNsArray = new ArrayList<Integer>();
    private List<Integer> prefixNsWrappedArray = new ArrayList<Integer>();

    /**
      **/
    public Builder attributeString(String attributeString) {
      this.attributeString = attributeString;
      return this;
    }
    /**
      **/
    public Builder attributeNumber(BigDecimal attributeNumber) {
      this.attributeNumber = attributeNumber;
      return this;
    }
    /**
      **/
    public Builder attributeInteger(Integer attributeInteger) {
      this.attributeInteger = attributeInteger;
      return this;
    }
    /**
      **/
    public Builder attributeBoolean(Boolean attributeBoolean) {
      this.attributeBoolean = attributeBoolean;
      return this;
    }
    /**
      **/
    public Builder wrappedArray(List<Integer> wrappedArray) {
      this.wrappedArray = wrappedArray;
      return this;
    }
    /**
      **/
    public Builder nameString(String nameString) {
      this.nameString = nameString;
      return this;
    }
    /**
      **/
    public Builder nameNumber(BigDecimal nameNumber) {
      this.nameNumber = nameNumber;
      return this;
    }
    /**
      **/
    public Builder nameInteger(Integer nameInteger) {
      this.nameInteger = nameInteger;
      return this;
    }
    /**
      **/
    public Builder nameBoolean(Boolean nameBoolean) {
      this.nameBoolean = nameBoolean;
      return this;
    }
    /**
      **/
    public Builder nameArray(List<Integer> nameArray) {
      this.nameArray = nameArray;
      return this;
    }
    /**
      **/
    public Builder nameWrappedArray(List<Integer> nameWrappedArray) {
      this.nameWrappedArray = nameWrappedArray;
      return this;
    }
    /**
      **/
    public Builder prefixString(String prefixString) {
      this.prefixString = prefixString;
      return this;
    }
    /**
      **/
    public Builder prefixNumber(BigDecimal prefixNumber) {
      this.prefixNumber = prefixNumber;
      return this;
    }
    /**
      **/
    public Builder prefixInteger(Integer prefixInteger) {
      this.prefixInteger = prefixInteger;
      return this;
    }
    /**
      **/
    public Builder prefixBoolean(Boolean prefixBoolean) {
      this.prefixBoolean = prefixBoolean;
      return this;
    }
    /**
      **/
    public Builder prefixArray(List<Integer> prefixArray) {
      this.prefixArray = prefixArray;
      return this;
    }
    /**
      **/
    public Builder prefixWrappedArray(List<Integer> prefixWrappedArray) {
      this.prefixWrappedArray = prefixWrappedArray;
      return this;
    }
    /**
      **/
    public Builder namespaceString(String namespaceString) {
      this.namespaceString = namespaceString;
      return this;
    }
    /**
      **/
    public Builder namespaceNumber(BigDecimal namespaceNumber) {
      this.namespaceNumber = namespaceNumber;
      return this;
    }
    /**
      **/
    public Builder namespaceInteger(Integer namespaceInteger) {
      this.namespaceInteger = namespaceInteger;
      return this;
    }
    /**
      **/
    public Builder namespaceBoolean(Boolean namespaceBoolean) {
      this.namespaceBoolean = namespaceBoolean;
      return this;
    }
    /**
      **/
    public Builder namespaceArray(List<Integer> namespaceArray) {
      this.namespaceArray = namespaceArray;
      return this;
    }
    /**
      **/
    public Builder namespaceWrappedArray(List<Integer> namespaceWrappedArray) {
      this.namespaceWrappedArray = namespaceWrappedArray;
      return this;
    }
    /**
      **/
    public Builder prefixNsString(String prefixNsString) {
      this.prefixNsString = prefixNsString;
      return this;
    }
    /**
      **/
    public Builder prefixNsNumber(BigDecimal prefixNsNumber) {
      this.prefixNsNumber = prefixNsNumber;
      return this;
    }
    /**
      **/
    public Builder prefixNsInteger(Integer prefixNsInteger) {
      this.prefixNsInteger = prefixNsInteger;
      return this;
    }
    /**
      **/
    public Builder prefixNsBoolean(Boolean prefixNsBoolean) {
      this.prefixNsBoolean = prefixNsBoolean;
      return this;
    }
    /**
      **/
    public Builder prefixNsArray(List<Integer> prefixNsArray) {
      this.prefixNsArray = prefixNsArray;
      return this;
    }
    /**
      **/
    public Builder prefixNsWrappedArray(List<Integer> prefixNsWrappedArray) {
      this.prefixNsWrappedArray = prefixNsWrappedArray;
      return this;
    }

    public XmlItem build() {
      return new XmlItem(attributeString, attributeNumber, attributeInteger, attributeBoolean, wrappedArray, nameString, nameNumber, nameInteger, nameBoolean, nameArray, nameWrappedArray, prefixString, prefixNumber, prefixInteger, prefixBoolean, prefixArray, prefixWrappedArray, namespaceString, namespaceNumber, namespaceInteger, namespaceBoolean, namespaceArray, namespaceWrappedArray, prefixNsString, prefixNsNumber, prefixNsInteger, prefixNsBoolean, prefixNsArray, prefixNsWrappedArray);
    }
  }
}

