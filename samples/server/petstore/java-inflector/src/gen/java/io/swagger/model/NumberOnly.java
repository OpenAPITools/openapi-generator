package io.swagger.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.math.BigDecimal;






public class NumberOnly   {
  @JsonProperty("JustNumber")
  private BigDecimal justNumber = null;

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
  public void setJustNumber(BigDecimal justNumber) {
    this.justNumber = justNumber;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    NumberOnly numberOnly = (NumberOnly) o;
    return Objects.equals(justNumber, numberOnly.justNumber);
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
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

