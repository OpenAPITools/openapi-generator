package io.swagger.model;

import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;


import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;


public class EnumArrays  implements Serializable {
  

public enum JustSymbolEnum {

    GREATER_THAN_OR_EQUAL_TO(String.valueOf(">=")), DOLLAR(String.valueOf("$"));


    private String value;

    JustSymbolEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static JustSymbolEnum fromValue(String v) {
        for (JustSymbolEnum b : JustSymbolEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  private @Valid JustSymbolEnum justSymbol = null;

public enum ArrayEnumEnum {

    FISH(String.valueOf("fish")), CRAB(String.valueOf("crab"));


    private String value;

    ArrayEnumEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static ArrayEnumEnum fromValue(String v) {
        for (ArrayEnumEnum b : ArrayEnumEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        return null;
    }
}

  private @Valid List<ArrayEnumEnum> arrayEnum = new ArrayList<ArrayEnumEnum>();

  /**
   **/
  public EnumArrays justSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("just_symbol")
  public JustSymbolEnum getJustSymbol() {
    return justSymbol;
  }
  public void setJustSymbol(JustSymbolEnum justSymbol) {
    this.justSymbol = justSymbol;
  }

  /**
   **/
  public EnumArrays arrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("array_enum")
  public List<ArrayEnumEnum> getArrayEnum() {
    return arrayEnum;
  }
  public void setArrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EnumArrays enumArrays = (EnumArrays) o;
    return Objects.equals(justSymbol, enumArrays.justSymbol) &&
        Objects.equals(arrayEnum, enumArrays.arrayEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(justSymbol, arrayEnum);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

