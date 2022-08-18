package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("NumberOnly")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class NumberOnly  implements Serializable {
  private @Valid BigDecimal justNumber;

  protected NumberOnly(NumberOnlyBuilder<?, ?> b) {
    this.justNumber = b.justNumber;
  }

  public NumberOnly() {
  }

  /**
   **/
  public NumberOnly justNumber(BigDecimal justNumber) {
    this.justNumber = justNumber;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("JustNumber")
  public BigDecimal getJustNumber() {
    return justNumber;
  }

  @JsonProperty("JustNumber")
  public void setJustNumber(BigDecimal justNumber) {
    this.justNumber = justNumber;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NumberOnly numberOnly = (NumberOnly) o;
    return Objects.equals(this.justNumber, numberOnly.justNumber);
  }

  @Override
  public int hashCode() {
    return Objects.hash(justNumber);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class NumberOnly {\n");
    
    sb.append("    justNumber: ").append(toIndentedString(justNumber)).append("\n");
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


  public static NumberOnlyBuilder<?, ?> builder() {
    return new NumberOnlyBuilderImpl();
  }

  private static final class NumberOnlyBuilderImpl extends NumberOnlyBuilder<NumberOnly, NumberOnlyBuilderImpl> {

    @Override
    protected NumberOnlyBuilderImpl self() {
      return this;
    }

    @Override
    public NumberOnly build() {
      return new NumberOnly(this);
    }
  }

  public static abstract class NumberOnlyBuilder<C extends NumberOnly, B extends NumberOnlyBuilder<C, B>>  {
    private BigDecimal justNumber;
    protected abstract B self();

    public abstract C build();

    public B justNumber(BigDecimal justNumber) {
      this.justNumber = justNumber;
      return self();
    }
  }
}

