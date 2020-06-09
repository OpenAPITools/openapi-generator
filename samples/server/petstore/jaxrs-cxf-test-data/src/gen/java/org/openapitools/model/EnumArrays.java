package org.openapitools.model;

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


public class EnumArrays  {
  
@XmlType(name="JustSymbolEnum")
@XmlEnum(String.class)
public enum JustSymbolEnum {

    @XmlEnumValue(">=") @JsonProperty(">=") GREATER_THAN_OR_EQUAL_TO(String.valueOf(">=")), 
    @XmlEnumValue("$") @JsonProperty("$") DOLLAR(String.valueOf("$"));

    private String value;

    JustSymbolEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static JustSymbolEnum fromValue(String value) {
        for (JustSymbolEnum b : JustSymbolEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  @ApiModelProperty(value = "")
  private JustSymbolEnum justSymbol;

@XmlType(name="ArrayEnumEnum")
@XmlEnum(String.class)
public enum ArrayEnumEnum {

    @XmlEnumValue("fish") @JsonProperty("fish") FISH(String.valueOf("fish")), 
    @XmlEnumValue("crab") @JsonProperty("crab") CRAB(String.valueOf("crab"));

    private String value;

    ArrayEnumEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static ArrayEnumEnum fromValue(String value) {
        for (ArrayEnumEnum b : ArrayEnumEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  @ApiModelProperty(value = "")
  private List<ArrayEnumEnum> arrayEnum = null;
 /**
  * Get justSymbol
  * @return justSymbol
  */
  @JsonProperty("just_symbol")
  public String getJustSymbol() {
    return justSymbol == null ? null : justSymbol.value();
  }

  /**
   * Sets the <code>justSymbol</code> property.
   */
  public void setJustSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
  }

  /**
   * Sets the <code>justSymbol</code> property.
   */
  public EnumArrays justSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
    return this;
  }

 /**
  * Get arrayEnum
  * @return arrayEnum
  */
  @JsonProperty("array_enum")
  public List<ArrayEnumEnum> getArrayEnum() {
    return arrayEnum;
  }

  /**
   * Sets the <code>arrayEnum</code> property.
   */
  public void setArrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
  }

  /**
   * Sets the <code>arrayEnum</code> property.
   */
  public EnumArrays arrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
    return this;
  }

  /**
   * Adds a new item to the <code>arrayEnum</code> list.
   */
  public EnumArrays addArrayEnumItem(ArrayEnumEnum arrayEnumItem) {
    this.arrayEnum.add(arrayEnumItem);
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumArrays {\n");
    
    sb.append("    justSymbol: ").append(toIndentedString(justSymbol)).append("\n");
    sb.append("    arrayEnum: ").append(toIndentedString(arrayEnum)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

