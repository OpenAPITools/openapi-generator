package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * TypeHolderDefault
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class TypeHolderDefault {

  private String stringItem = "what";

  private BigDecimal numberItem = new BigDecimal("1.234");

  private Integer integerItem = -2;

  private Boolean boolItem = true;

  @Valid
  private List<Integer> arrayItem = new ArrayList<>(Arrays.asList(0, 1, 2, 3));

  public TypeHolderDefault() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public TypeHolderDefault(String stringItem, BigDecimal numberItem, Integer integerItem, Boolean boolItem, List<Integer> arrayItem) {
    this.stringItem = stringItem;
    this.numberItem = numberItem;
    this.integerItem = integerItem;
    this.boolItem = boolItem;
    this.arrayItem = arrayItem;
  }

  public TypeHolderDefault stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  /**
   * Get stringItem
   * @return stringItem
   */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("string_item")
  public String getStringItem() {
    return stringItem;
  }

  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  public TypeHolderDefault numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

  /**
   * Get numberItem
   * @return numberItem
   */
  @NotNull @Valid 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("number_item")
  public BigDecimal getNumberItem() {
    return numberItem;
  }

  public void setNumberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
  }

  public TypeHolderDefault integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

  /**
   * Get integerItem
   * @return integerItem
   */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("integer_item")
  public Integer getIntegerItem() {
    return integerItem;
  }

  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  public TypeHolderDefault boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

  /**
   * Get boolItem
   * @return boolItem
   */
  @NotNull 
  @ApiModelProperty(required = true, value = "")
  @JsonProperty("bool_item")
  public Boolean getBoolItem() {
    return boolItem;
  }

  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  public TypeHolderDefault arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderDefault addArrayItemItem(Integer arrayItemItem) {
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
  @ApiModelProperty(required = true, value = "")
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
    TypeHolderDefault typeHolderDefault = (TypeHolderDefault) o;
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
    sb.append("class TypeHolderDefault {\n");
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

    private TypeHolderDefault instance;

    public Builder() {
      this(new TypeHolderDefault());
    }

    protected Builder(TypeHolderDefault instance) {
      this.instance = instance;
    }

    protected Builder copyOf(TypeHolderDefault value) { 
      this.instance.setStringItem(value.stringItem);
      this.instance.setNumberItem(value.numberItem);
      this.instance.setIntegerItem(value.integerItem);
      this.instance.setBoolItem(value.boolItem);
      this.instance.setArrayItem(value.arrayItem);
      return this;
    }

    public TypeHolderDefault.Builder stringItem(String stringItem) {
      this.instance.stringItem(stringItem);
      return this;
    }
    
    public TypeHolderDefault.Builder numberItem(BigDecimal numberItem) {
      this.instance.numberItem(numberItem);
      return this;
    }
    
    public TypeHolderDefault.Builder integerItem(Integer integerItem) {
      this.instance.integerItem(integerItem);
      return this;
    }
    
    public TypeHolderDefault.Builder boolItem(Boolean boolItem) {
      this.instance.boolItem(boolItem);
      return this;
    }
    
    public TypeHolderDefault.Builder arrayItem(List<Integer> arrayItem) {
      this.instance.arrayItem(arrayItem);
      return this;
    }
    
    /**
    * returns a built TypeHolderDefault instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public TypeHolderDefault build() {
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
  * Create a builder with no initialized field (except for the default values).
  */
  public static TypeHolderDefault.Builder builder() {
    return new TypeHolderDefault.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public TypeHolderDefault.Builder toBuilder() {
    TypeHolderDefault.Builder builder = new TypeHolderDefault.Builder();
    return builder.copyOf(this);
  }

}

