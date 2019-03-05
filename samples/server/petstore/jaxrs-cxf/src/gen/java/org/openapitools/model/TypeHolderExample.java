package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.joda.time.LocalDate;
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

/**
  * a model to test required properties with an example and length one enum
 **/
@ApiModel(description="a model to test required properties with an example and length one enum")
public class TypeHolderExample  {
  
@XmlType(name="StringItemEnum")
@XmlEnum(String.class)
public enum StringItemEnum {

@XmlEnumValue("what") WHAT(String.valueOf("what"));


    private String value;

    StringItemEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static StringItemEnum fromValue(String v) {
        for (StringItemEnum b : StringItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  @ApiModelProperty(example = "what", required = true, value = "")
  private StringItemEnum stringItem;

@XmlType(name="NumberItemEnum")
@XmlEnum(Float.class)
public enum NumberItemEnum {

@XmlEnumValue("1.2339999675750732f") NUMBER_1_DOT_2339999675750732(Float.valueOf(1.2339999675750732f));


    private Float value;

    NumberItemEnum (Float v) {
        value = v;
    }

    public Float value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static NumberItemEnum fromValue(String v) {
        for (NumberItemEnum b : NumberItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  @ApiModelProperty(example = "1.234", required = true, value = "")
  private NumberItemEnum numberItem;

@XmlType(name="IntegerItemEnum")
@XmlEnum(Integer.class)
public enum IntegerItemEnum {

@XmlEnumValue("-2") NUMBER_MINUS_2(Integer.valueOf(-2));


    private Integer value;

    IntegerItemEnum (Integer v) {
        value = v;
    }

    public Integer value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static IntegerItemEnum fromValue(String v) {
        for (IntegerItemEnum b : IntegerItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  @ApiModelProperty(example = "-2", required = true, value = "")
  private IntegerItemEnum integerItem;

  @ApiModelProperty(example = "true", required = true, value = "")
  private Boolean boolItem;

@XmlType(name="DateItemEnum")
@XmlEnum(LocalDate.class)
public enum DateItemEnum {

@XmlEnumValue("Thu Jul 20 17:00:00 PDT 2017") THU_JUL_20_17_00_00_PDT_2017(LocalDate.valueOf("Thu Jul 20 17:00:00 PDT 2017"));


    private LocalDate value;

    DateItemEnum (LocalDate v) {
        value = v;
    }

    public LocalDate value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static DateItemEnum fromValue(String v) {
        for (DateItemEnum b : DateItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  @ApiModelProperty(required = true, value = "")
  private DateItemEnum dateItem;

@XmlType(name="DatetimeItemEnum")
@XmlEnum(Date.class)
public enum DatetimeItemEnum {

@XmlEnumValue("Fri Jul 21 10:32:28 PDT 2017") FRI_JUL_21_10_32_28_PDT_2017(Date.valueOf("Fri Jul 21 10:32:28 PDT 2017"));


    private Date value;

    DatetimeItemEnum (Date v) {
        value = v;
    }

    public Date value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    public static DatetimeItemEnum fromValue(String v) {
        for (DatetimeItemEnum b : DatetimeItemEnum.values()) {
            if (String.valueOf(b.value).equals(v)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + v + "'");
    }
}

  @ApiModelProperty(required = true, value = "")
  private DatetimeItemEnum datetimeItem;

  @ApiModelProperty(example = "[[0, 1, 2, 3]]", required = true, value = "")
  private List<Integer> arrayItem = new ArrayList<Integer>();
 /**
   * Get stringItem
   * @return stringItem
  **/
  @JsonProperty("string_item")
  @NotNull
  public String getStringItem() {
    if (stringItem == null) {
      return null;
    }
    return stringItem.value();
  }

  public void setStringItem(StringItemEnum stringItem) {
    this.stringItem = stringItem;
  }

  public TypeHolderExample stringItem(StringItemEnum stringItem) {
    this.stringItem = stringItem;
    return this;
  }

 /**
   * Get numberItem
   * @return numberItem
  **/
  @JsonProperty("number_item")
  @NotNull
  public Float getNumberItem() {
    if (numberItem == null) {
      return null;
    }
    return numberItem.value();
  }

  public void setNumberItem(NumberItemEnum numberItem) {
    this.numberItem = numberItem;
  }

  public TypeHolderExample numberItem(NumberItemEnum numberItem) {
    this.numberItem = numberItem;
    return this;
  }

 /**
   * Get integerItem
   * @return integerItem
  **/
  @JsonProperty("integer_item")
  @NotNull
  public Integer getIntegerItem() {
    if (integerItem == null) {
      return null;
    }
    return integerItem.value();
  }

  public void setIntegerItem(IntegerItemEnum integerItem) {
    this.integerItem = integerItem;
  }

  public TypeHolderExample integerItem(IntegerItemEnum integerItem) {
    this.integerItem = integerItem;
    return this;
  }

 /**
   * Get boolItem
   * @return boolItem
  **/
  @JsonProperty("bool_item")
  @NotNull
  public Boolean getBoolItem() {
    return boolItem;
  }

  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  public TypeHolderExample boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

 /**
   * Get dateItem
   * @return dateItem
  **/
  @JsonProperty("date_item")
  @NotNull
  public LocalDate getDateItem() {
    if (dateItem == null) {
      return null;
    }
    return dateItem.value();
  }

  public void setDateItem(DateItemEnum dateItem) {
    this.dateItem = dateItem;
  }

  public TypeHolderExample dateItem(DateItemEnum dateItem) {
    this.dateItem = dateItem;
    return this;
  }

 /**
   * Get datetimeItem
   * @return datetimeItem
  **/
  @JsonProperty("datetime_item")
  @NotNull
  public Date getDatetimeItem() {
    if (datetimeItem == null) {
      return null;
    }
    return datetimeItem.value();
  }

  public void setDatetimeItem(DatetimeItemEnum datetimeItem) {
    this.datetimeItem = datetimeItem;
  }

  public TypeHolderExample datetimeItem(DatetimeItemEnum datetimeItem) {
    this.datetimeItem = datetimeItem;
    return this;
  }

 /**
   * Get arrayItem
   * @return arrayItem
  **/
  @JsonProperty("array_item")
  @NotNull
  public List<Integer> getArrayItem() {
    return arrayItem;
  }

  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }

  public TypeHolderExample arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderExample addArrayItemItem(Integer arrayItemItem) {
    this.arrayItem.add(arrayItemItem);
    return this;
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
  private static String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

