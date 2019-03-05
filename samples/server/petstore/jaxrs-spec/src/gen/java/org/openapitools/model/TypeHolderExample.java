package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.joda.time.LocalDate;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

/**
 * a model to test required properties with an example and length one enum
 **/
@ApiModel(description = "a model to test required properties with an example and length one enum")
public class TypeHolderExample  implements Serializable {
  

public enum StringItemEnum {

    WHAT(String.valueOf("what"));


    private String value;

    StringItemEnum (String v) {
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
    public static StringItemEnum fromValue(String v) {
        for (StringItemEnum b : StringItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  private @Valid StringItemEnum stringItem;

public enum NumberItemEnum {

    NUMBER_1_DOT_2339999675750732(Float.valueOf(1.2339999675750732f));


    private Float value;

    NumberItemEnum (Float v) {
        value = v;
    }

    public Float value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static NumberItemEnum fromValue(String v) {
        for (NumberItemEnum b : NumberItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  private @Valid NumberItemEnum numberItem;

public enum IntegerItemEnum {

    NUMBER_MINUS_2(Integer.valueOf(-2));


    private Integer value;

    IntegerItemEnum (Integer v) {
        value = v;
    }

    public Integer value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static IntegerItemEnum fromValue(String v) {
        for (IntegerItemEnum b : IntegerItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  private @Valid IntegerItemEnum integerItem;
  private @Valid Boolean boolItem;

public enum DateItemEnum {

    THU_JUL_20_17_00_00_PDT_2017(LocalDate.valueOf("Thu Jul 20 17:00:00 PDT 2017"));


    private LocalDate value;

    DateItemEnum (LocalDate v) {
        value = v;
    }

    public LocalDate value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static DateItemEnum fromValue(String v) {
        for (DateItemEnum b : DateItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  private @Valid DateItemEnum dateItem;

public enum DatetimeItemEnum {

    FRI_JUL_21_10_32_28_PDT_2017(Date.valueOf("Fri Jul 21 10:32:28 PDT 2017"));


    private Date value;

    DatetimeItemEnum (Date v) {
        value = v;
    }

    public Date value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    @JsonCreator
    public static DatetimeItemEnum fromValue(String v) {
        for (DatetimeItemEnum b : DatetimeItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  private @Valid DatetimeItemEnum datetimeItem;
  private @Valid List<Integer> arrayItem = new ArrayList<Integer>();

  /**
   **/
  public TypeHolderExample stringItem(StringItemEnum stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  
  @ApiModelProperty(example = "what", required = true, value = "")
  @JsonProperty("string_item")
  @NotNull
  public StringItemEnum getStringItem() {
    return stringItem;
  }
  public void setStringItem(StringItemEnum stringItem) {
    this.stringItem = stringItem;
  }

  /**
   **/
  public TypeHolderExample numberItem(NumberItemEnum numberItem) {
    this.numberItem = numberItem;
    return this;
  }

  
  @ApiModelProperty(example = "1.234", required = true, value = "")
  @JsonProperty("number_item")
  @NotNull
  public NumberItemEnum getNumberItem() {
    return numberItem;
  }
  public void setNumberItem(NumberItemEnum numberItem) {
    this.numberItem = numberItem;
  }

  /**
   **/
  public TypeHolderExample integerItem(IntegerItemEnum integerItem) {
    this.integerItem = integerItem;
    return this;
  }

  
  @ApiModelProperty(example = "-2", required = true, value = "")
  @JsonProperty("integer_item")
  @NotNull
  public IntegerItemEnum getIntegerItem() {
    return integerItem;
  }
  public void setIntegerItem(IntegerItemEnum integerItem) {
    this.integerItem = integerItem;
  }

  /**
   **/
  public TypeHolderExample boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

  
  @ApiModelProperty(example = "true", required = true, value = "")
  @JsonProperty("bool_item")
  @NotNull
  public Boolean getBoolItem() {
    return boolItem;
  }
  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  /**
   **/
  public TypeHolderExample dateItem(DateItemEnum dateItem) {
    this.dateItem = dateItem;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("date_item")
  @NotNull
  public DateItemEnum getDateItem() {
    return dateItem;
  }
  public void setDateItem(DateItemEnum dateItem) {
    this.dateItem = dateItem;
  }

  /**
   **/
  public TypeHolderExample datetimeItem(DatetimeItemEnum datetimeItem) {
    this.datetimeItem = datetimeItem;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("datetime_item")
  @NotNull
  public DatetimeItemEnum getDatetimeItem() {
    return datetimeItem;
  }
  public void setDatetimeItem(DatetimeItemEnum datetimeItem) {
    this.datetimeItem = datetimeItem;
  }

  /**
   **/
  public TypeHolderExample arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  
  @ApiModelProperty(example = "[[0, 1, 2, 3]]", required = true, value = "")
  @JsonProperty("array_item")
  @NotNull
  public List<Integer> getArrayItem() {
    return arrayItem;
  }
  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TypeHolderExample typeHolderExample = (TypeHolderExample) o;
    return Objects.equals(stringItem, typeHolderExample.stringItem) &&
        Objects.equals(numberItem, typeHolderExample.numberItem) &&
        Objects.equals(integerItem, typeHolderExample.integerItem) &&
        Objects.equals(boolItem, typeHolderExample.boolItem) &&
        Objects.equals(dateItem, typeHolderExample.dateItem) &&
        Objects.equals(datetimeItem, typeHolderExample.datetimeItem) &&
        Objects.equals(arrayItem, typeHolderExample.arrayItem);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringItem, numberItem, integerItem, boolItem, dateItem, datetimeItem, arrayItem);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TypeHolderExample {\n");
    
    sb.append("    stringItem: ").append(toIndentedString(stringItem)).append("\n");
    sb.append("    numberItem: ").append(toIndentedString(numberItem)).append("\n");
    sb.append("    integerItem: ").append(toIndentedString(integerItem)).append("\n");
    sb.append("    boolItem: ").append(toIndentedString(boolItem)).append("\n");
    sb.append("    dateItem: ").append(toIndentedString(dateItem)).append("\n");
    sb.append("    datetimeItem: ").append(toIndentedString(datetimeItem)).append("\n");
    sb.append("    arrayItem: ").append(toIndentedString(arrayItem)).append("\n");
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

