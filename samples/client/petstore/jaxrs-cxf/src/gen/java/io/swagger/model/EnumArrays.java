package io.swagger.model;

import java.util.ArrayList;
import java.util.List;

import io.swagger.annotations.ApiModelProperty;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

public class EnumArrays  {
  

@XmlType(name="JustSymbolEnum")
@XmlEnum(String.class)
public enum JustSymbolEnum {

@XmlEnumValue(">=") GREATER_THAN_OR_EQUAL_TO(String.valueOf(">=")), @XmlEnumValue("$") DOLLAR(String.valueOf("$"));


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

    public static JustSymbolEnum fromValue(String v) {
        for (JustSymbolEnum b : JustSymbolEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  @ApiModelProperty(example = "null", value = "")
  private JustSymbolEnum justSymbol = null;

@XmlType(name="ArrayEnumEnum")
@XmlEnum(String.class)
public enum ArrayEnumEnum {

@XmlEnumValue("fish") FISH(String.valueOf("fish")), @XmlEnumValue("crab") CRAB(String.valueOf("crab"));


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

    public static ArrayEnumEnum fromValue(String v) {
        for (ArrayEnumEnum b : ArrayEnumEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  @ApiModelProperty(example = "null", value = "")
  private List<ArrayEnumEnum> arrayEnum = new ArrayList<ArrayEnumEnum>();

 /**
   * Get justSymbol
   * @return justSymbol
  **/
  public JustSymbolEnum getJustSymbol() {
    return justSymbol;
  }

  public void setJustSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
  }

  public EnumArrays justSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
    return this;
  }

 /**
   * Get arrayEnum
   * @return arrayEnum
  **/
  public List<ArrayEnumEnum> getArrayEnum() {
    return arrayEnum;
  }

  public void setArrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
  }

  public EnumArrays arrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
    return this;
  }

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
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

