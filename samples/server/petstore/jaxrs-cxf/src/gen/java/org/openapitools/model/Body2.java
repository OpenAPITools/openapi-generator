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
import com.fasterxml.jackson.annotation.JsonProperty;

public class Body2  {
  
@XmlType(name="EnumFormStringArrayEnum")
@XmlEnum(String.class)
public enum EnumFormStringArrayEnum {

@XmlEnumValue(">") GREATER_THAN(String.valueOf(">")), @XmlEnumValue("$") DOLLAR(String.valueOf("$"));


    private String value;

    EnumFormStringArrayEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static EnumFormStringArrayEnum fromValue(String v) {
        for (EnumFormStringArrayEnum b : EnumFormStringArrayEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  @ApiModelProperty(value = "Form parameter enum test (string array)")
 /**
   * Form parameter enum test (string array)
  **/
  private List<EnumFormStringArrayEnum> enumFormStringArray = null;

@XmlType(name="EnumFormStringEnum")
@XmlEnum(String.class)
public enum EnumFormStringEnum {

@XmlEnumValue("_abc") _ABC(String.valueOf("_abc")), @XmlEnumValue("-efg") _EFG(String.valueOf("-efg")), @XmlEnumValue("(xyz)") _XYZ_(String.valueOf("(xyz)"));


    private String value;

    EnumFormStringEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static EnumFormStringEnum fromValue(String v) {
        for (EnumFormStringEnum b : EnumFormStringEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  @ApiModelProperty(value = "Form parameter enum test (string)")
 /**
   * Form parameter enum test (string)
  **/
  private EnumFormStringEnum enumFormString = EnumFormStringEnum._EFG;
 /**
   * Form parameter enum test (string array)
   * @return enumFormStringArray
  **/
  @JsonProperty("enum_form_string_array")
  public List<EnumFormStringArrayEnum> getEnumFormStringArray() {
    return enumFormStringArray;
  }

  public void setEnumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
  }

  public Body2 enumFormStringArray(List<EnumFormStringArrayEnum> enumFormStringArray) {
    this.enumFormStringArray = enumFormStringArray;
    return this;
  }

  public Body2 addEnumFormStringArrayItem(EnumFormStringArrayEnum enumFormStringArrayItem) {
    this.enumFormStringArray.add(enumFormStringArrayItem);
    return this;
  }

 /**
   * Form parameter enum test (string)
   * @return enumFormString
  **/
  @JsonProperty("enum_form_string")
  public String getEnumFormString() {
    if (enumFormString == null) {
      return null;
    }
    return enumFormString.value();
  }

  public void setEnumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
  }

  public Body2 enumFormString(EnumFormStringEnum enumFormString) {
    this.enumFormString = enumFormString;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body2 {\n");
    
    sb.append("    enumFormStringArray: ").append(toIndentedString(enumFormStringArray)).append("\n");
    sb.append("    enumFormString: ").append(toIndentedString(enumFormString)).append("\n");
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

