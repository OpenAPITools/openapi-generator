package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("EnumArrays")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
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

    /**
     * Convert a String into String, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
	public static JustSymbolEnum fromString(String s) {
        for (JustSymbolEnum b : JustSymbolEnum.values()) {
            // using Objects.toString() to be safe if value type non-object type
            // because types like 'int' etc. will be auto-boxed
            if (java.util.Objects.toString(b.value).equals(s)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected string value '" + s + "'");
	}
	
    @JsonCreator
    public static JustSymbolEnum fromValue(String value) {
        for (JustSymbolEnum b : JustSymbolEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  private @Valid JustSymbolEnum justSymbol;
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

    /**
     * Convert a String into String, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
	public static ArrayEnumEnum fromString(String s) {
        for (ArrayEnumEnum b : ArrayEnumEnum.values()) {
            // using Objects.toString() to be safe if value type non-object type
            // because types like 'int' etc. will be auto-boxed
            if (java.util.Objects.toString(b.value).equals(s)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected string value '" + s + "'");
	}
	
    @JsonCreator
    public static ArrayEnumEnum fromValue(String value) {
        for (ArrayEnumEnum b : ArrayEnumEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  private @Valid List<ArrayEnumEnum> arrayEnum = null;

  protected EnumArrays(EnumArraysBuilder<?, ?> b) {
    this.justSymbol = b.justSymbol;
    this.arrayEnum = b.arrayEnum;
  }

  public EnumArrays() {
  }

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

  @JsonProperty("just_symbol")
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

  @JsonProperty("array_enum")
  public void setArrayEnum(List<ArrayEnumEnum> arrayEnum) {
    this.arrayEnum = arrayEnum;
  }

  public EnumArrays addArrayEnumItem(ArrayEnumEnum arrayEnumItem) {
    if (this.arrayEnum == null) {
      this.arrayEnum = new ArrayList<>();
    }

    this.arrayEnum.add(arrayEnumItem);
    return this;
  }

  public EnumArrays removeArrayEnumItem(ArrayEnumEnum arrayEnumItem) {
    if (arrayEnumItem != null && this.arrayEnum != null) {
      this.arrayEnum.remove(arrayEnumItem);
    }

    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    EnumArrays enumArrays = (EnumArrays) o;
    return Objects.equals(this.justSymbol, enumArrays.justSymbol) &&
        Objects.equals(this.arrayEnum, enumArrays.arrayEnum);
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }


  public static EnumArraysBuilder<?, ?> builder() {
    return new EnumArraysBuilderImpl();
  }

  private static final class EnumArraysBuilderImpl extends EnumArraysBuilder<EnumArrays, EnumArraysBuilderImpl> {

    @Override
    protected EnumArraysBuilderImpl self() {
      return this;
    }

    @Override
    public EnumArrays build() {
      return new EnumArrays(this);
    }
  }

  public static abstract class EnumArraysBuilder<C extends EnumArrays, B extends EnumArraysBuilder<C, B>>  {
    private JustSymbolEnum justSymbol;
    private List<ArrayEnumEnum> arrayEnum = new ArrayList<>();
    protected abstract B self();

    public abstract C build();

    public B justSymbol(JustSymbolEnum justSymbol) {
      this.justSymbol = justSymbol;
      return self();
    }
    public B arrayEnum(List<ArrayEnumEnum> arrayEnum) {
      this.arrayEnum = arrayEnum;
      return self();
    }
  }
}

