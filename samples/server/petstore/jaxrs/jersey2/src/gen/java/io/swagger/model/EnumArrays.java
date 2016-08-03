package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;




/**
 * EnumArrays
 */

public class EnumArrays   {
  /**
   * Gets or Sets justEnum
   */
  public enum JustEnumEnum {
    BIRD("bird"),
    
    EAGLE("eagle");

    private String value;

    JustEnumEnum(String value) {
      this.value = value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  private JustEnumEnum justEnum = null;

  /**
   * Gets or Sets arrayEnum
   */
  public enum ArrayEnumEnum {
    FISH("fish"),
    
    CRAB("crab");

    private String value;

    ArrayEnumEnum(String value) {
      this.value = value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }

  private List<ArrayEnumEnum> arrayEnum = new ArrayList<ArrayEnumEnum>();

  public EnumArrays justEnum(JustEnumEnum justEnum) {
    this.justEnum = justEnum;
    return this;
  }

   /**
   * Get justEnum
   * @return justEnum
  **/
  @ApiModelProperty(value = "")
  public JustEnumEnum getJustEnum() {
    return justEnum;
  }

  public void setJustEnum(JustEnumEnum justEnum) {
    this.justEnum = justEnum;
  }

  public EnumArrays arrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
    return this;
  }

  public EnumArrays addArrayEnumItem(ArrayEnumEnum arrayEnumItem) {
    this.arrayEnum.add(arrayEnumItem);
    return this;
  }

   /**
   * Get arrayEnum
   * @return arrayEnum
  **/
  @ApiModelProperty(value = "")
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
    return Objects.equals(this.justEnum, enumArrays.justEnum) &&
        Objects.equals(this.arrayEnum, enumArrays.arrayEnum);
  }

  @Override
  public int hashCode() {
    return Objects.hash(justEnum, arrayEnum);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class EnumArrays {\n");
    
    sb.append("    justEnum: ").append(toIndentedString(justEnum)).append("\n");
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

