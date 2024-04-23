package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * TypeHolderExampleDto
 */

@JsonTypeName("TypeHolderExample")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.6.0-SNAPSHOT")
public class TypeHolderExampleDto {

  private String stringItem;

  private BigDecimal numberItem;

  private Float floatItem;

  private Integer integerItem;

  private Boolean boolItem;

  @Valid
  private List<Integer> arrayItem = new ArrayList<>();

  public TypeHolderExampleDto() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public TypeHolderExampleDto(String stringItem, BigDecimal numberItem, Float floatItem, Integer integerItem, Boolean boolItem, List<Integer> arrayItem) {
    this.stringItem = stringItem;
    this.numberItem = numberItem;
    this.floatItem = floatItem;
    this.integerItem = integerItem;
    this.boolItem = boolItem;
    this.arrayItem = arrayItem;
  }

  public TypeHolderExampleDto stringItem(String stringItem) {
    this.stringItem = stringItem;
    return this;
  }

  /**
   * Get stringItem
   * @return stringItem
  */
  @NotNull 
  @ApiModelProperty(example = "what", required = true, value = "")
  @JsonProperty("string_item")
  public String getStringItem() {
    return stringItem;
  }

  public void setStringItem(String stringItem) {
    this.stringItem = stringItem;
  }

  public TypeHolderExampleDto numberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
    return this;
  }

  /**
   * Get numberItem
   * @return numberItem
  */
  @NotNull @Valid 
  @ApiModelProperty(example = "1.234", required = true, value = "")
  @JsonProperty("number_item")
  public BigDecimal getNumberItem() {
    return numberItem;
  }

  public void setNumberItem(BigDecimal numberItem) {
    this.numberItem = numberItem;
  }

  public TypeHolderExampleDto floatItem(Float floatItem) {
    this.floatItem = floatItem;
    return this;
  }

  /**
   * Get floatItem
   * @return floatItem
  */
  @NotNull 
  @ApiModelProperty(example = "1.234", required = true, value = "")
  @JsonProperty("float_item")
  public Float getFloatItem() {
    return floatItem;
  }

  public void setFloatItem(Float floatItem) {
    this.floatItem = floatItem;
  }

  public TypeHolderExampleDto integerItem(Integer integerItem) {
    this.integerItem = integerItem;
    return this;
  }

  /**
   * Get integerItem
   * @return integerItem
  */
  @NotNull 
  @ApiModelProperty(example = "-2", required = true, value = "")
  @JsonProperty("integer_item")
  public Integer getIntegerItem() {
    return integerItem;
  }

  public void setIntegerItem(Integer integerItem) {
    this.integerItem = integerItem;
  }

  public TypeHolderExampleDto boolItem(Boolean boolItem) {
    this.boolItem = boolItem;
    return this;
  }

  /**
   * Get boolItem
   * @return boolItem
  */
  @NotNull 
  @ApiModelProperty(example = "true", required = true, value = "")
  @JsonProperty("bool_item")
  public Boolean getBoolItem() {
    return boolItem;
  }

  public void setBoolItem(Boolean boolItem) {
    this.boolItem = boolItem;
  }

  public TypeHolderExampleDto arrayItem(List<Integer> arrayItem) {
    this.arrayItem = arrayItem;
    return this;
  }

  public TypeHolderExampleDto addArrayItemItem(Integer arrayItemItem) {
    if (this.arrayItem == null) {
      this.arrayItem = new ArrayList<>();
    }
    this.arrayItem.add(arrayItemItem);
    return this;
  }

  /**
   * Get arrayItem
   * @return arrayItem
  */
  @NotNull 
  @ApiModelProperty(example = "[0,1,2,3]", required = true, value = "")
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
    TypeHolderExampleDto typeHolderExample = (TypeHolderExampleDto) o;
    return Objects.equals(this.stringItem, typeHolderExample.stringItem) &&
        Objects.equals(this.numberItem, typeHolderExample.numberItem) &&
        Objects.equals(this.floatItem, typeHolderExample.floatItem) &&
        Objects.equals(this.integerItem, typeHolderExample.integerItem) &&
        Objects.equals(this.boolItem, typeHolderExample.boolItem) &&
        Objects.equals(this.arrayItem, typeHolderExample.arrayItem);
  }

  @Override
  public int hashCode() {
    return Objects.hash(stringItem, numberItem, floatItem, integerItem, boolItem, arrayItem);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TypeHolderExampleDto {\n");
    sb.append("    stringItem: ").append(toIndentedString(stringItem)).append("\n");
    sb.append("    numberItem: ").append(toIndentedString(numberItem)).append("\n");
    sb.append("    floatItem: ").append(toIndentedString(floatItem)).append("\n");
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

    private TypeHolderExampleDto instance;

    public Builder() {
      this(new TypeHolderExampleDto());
    }

    protected Builder(TypeHolderExampleDto instance) {
      this.instance = instance;
    }

    protected Builder copyOf(TypeHolderExampleDto value) { 
      this.instance.setStringItem(value.stringItem);
      this.instance.setNumberItem(value.numberItem);
      this.instance.setFloatItem(value.floatItem);
      this.instance.setIntegerItem(value.integerItem);
      this.instance.setBoolItem(value.boolItem);
      this.instance.setArrayItem(value.arrayItem);
      return this;
    }

    public TypeHolderExampleDto.Builder stringItem(String stringItem) {
      this.instance.stringItem(stringItem);
      return this;
    }
    
    public TypeHolderExampleDto.Builder numberItem(BigDecimal numberItem) {
      this.instance.numberItem(numberItem);
      return this;
    }
    
    public TypeHolderExampleDto.Builder floatItem(Float floatItem) {
      this.instance.floatItem(floatItem);
      return this;
    }
    
    public TypeHolderExampleDto.Builder integerItem(Integer integerItem) {
      this.instance.integerItem(integerItem);
      return this;
    }
    
    public TypeHolderExampleDto.Builder boolItem(Boolean boolItem) {
      this.instance.boolItem(boolItem);
      return this;
    }
    
    public TypeHolderExampleDto.Builder arrayItem(List<Integer> arrayItem) {
      this.instance.arrayItem(arrayItem);
      return this;
    }
    
    /**
    * returns a built TypeHolderExampleDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public TypeHolderExampleDto build() {
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
  public static TypeHolderExampleDto.Builder builder() {
    return new TypeHolderExampleDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public TypeHolderExampleDto.Builder toBuilder() {
    TypeHolderExampleDto.Builder builder = new TypeHolderExampleDto.Builder();
    return builder.copyOf(this);
  }

}

