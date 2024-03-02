package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * TypeHolderDefaultDto
 */

@JsonTypeName("TypeHolderDefault")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class TypeHolderDefaultDto {

  private String stringItem = "what";

  private BigDecimal numberItem = new BigDecimal("1.234");

  private Integer integerItem = -2;

  private Boolean boolItem = true;

  
  private List<Integer> arrayItem = new ArrayList<>(Arrays.asList(0, 1, 2, 3));

  public TypeHolderDefaultDto stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  /**
   * Get stringItem
   * @return stringItem
  */
  @NotNull
  @JsonProperty("string_item")
  public String getStringItem() {
    return stringItem;
  }

  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  public TypeHolderDefaultDto numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

  /**
   * Get numberItem
   * @return numberItem
  */
  @NotNull
  @JsonProperty("number_item")
  public BigDecimal getNumberItem() {
    return numberItem;
  }

  public void setNumberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
  }

  public TypeHolderDefaultDto integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

  /**
   * Get integerItem
   * @return integerItem
  */
  @NotNull
  @JsonProperty("integer_item")
  public Integer getIntegerItem() {
    return integerItem;
  }

  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  public TypeHolderDefaultDto boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

  /**
   * Get boolItem
   * @return boolItem
  */
  @NotNull
  @JsonProperty("bool_item")
  public Boolean getBoolItem() {
    return boolItem;
  }

  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  public TypeHolderDefaultDto arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderDefaultDto addArrayItemItem(Integer arrayItemItem) {
    if (this.arrayItem == null) {
      this.arrayItem = new ArrayList<>(Arrays.asList(0, 1, 2, 3));
    }
    this.arrayItem.add(arrayItemItem);
    return this;
  }

  /**
   * Get arrayItem
   * @return arrayItem
  */
  @NotNull
  @JsonProperty("array_item")
  public List<Integer> getArrayItem() {
    return arrayItem;
  }

  public void setArrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TypeHolderDefaultDto typeHolderDefault = (TypeHolderDefaultDto) o;
    return Objects.equals(this.stringItem, typeHolderDefault.stringItem) &&
        Objects.equals(this.numberItem, typeHolderDefault.numberItem) &&
        Objects.equals(this.integerItem, typeHolderDefault.integerItem) &&
        Objects.equals(this.boolItem, typeHolderDefault.boolItem) &&
        Objects.equals(this.arrayItem, typeHolderDefault.arrayItem);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringItem, numberItem, integerItem, boolItem, arrayItem);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TypeHolderDefaultDto {\n");
    sb.append("    stringItem: ").append(toIndentedString(stringItem)).append("\n");
    sb.append("    numberItem: ").append(toIndentedString(numberItem)).append("\n");
    sb.append("    integerItem: ").append(toIndentedString(integerItem)).append("\n");
    sb.append("    boolItem: ").append(toIndentedString(boolItem)).append("\n");
    sb.append("    arrayItem: ").append(toIndentedString(arrayItem)).append("\n");
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
  
  public static class Builder {

    private TypeHolderDefaultDto instance;

    public Builder() {
      this(new TypeHolderDefaultDto());
    }

    protected Builder(TypeHolderDefaultDto instance) {
      this.instance = instance;
    }

    public TypeHolderDefaultDto.Builder stringItem(String stringItem) {
      this.instance.stringItem(stringItem);
      return this;
    }
    public TypeHolderDefaultDto.Builder numberItem(BigDecimal numberItem) {
      this.instance.numberItem(numberItem);
      return this;
    }
    public TypeHolderDefaultDto.Builder integerItem(Integer integerItem) {
      this.instance.integerItem(integerItem);
      return this;
    }
    public TypeHolderDefaultDto.Builder boolItem(Boolean boolItem) {
      this.instance.boolItem(boolItem);
      return this;
    }
    public TypeHolderDefaultDto.Builder arrayItem(List<Integer> arrayItem) {
      this.instance.arrayItem(arrayItem);
      return this;
    }
    /**
    * returns a built TypeHolderDefaultDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public TypeHolderDefaultDto build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
  }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field.
  */
  public static TypeHolderDefaultDto.Builder builder() {
    return new TypeHolderDefaultDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public TypeHolderDefaultDto.Builder toBuilder() {
    TypeHolderDefaultDto.Builder builder = new TypeHolderDefaultDto.Builder();
    builder.instance.setStringItem(stringItem);
    builder.instance.setNumberItem(numberItem);
    builder.instance.setIntegerItem(integerItem);
    builder.instance.setBoolItem(boolItem);
    builder.instance.setArrayItem(arrayItem);
    return builder;
  }

}

