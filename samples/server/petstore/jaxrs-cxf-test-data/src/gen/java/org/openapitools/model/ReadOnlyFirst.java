package org.openapitools.model;

import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.ApiModelProperty;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class ReadOnlyFirst  {
  
  @ApiModelProperty(value = "")
  private String bar;

  @ApiModelProperty(value = "")
  private String baz;
 /**
  * Get bar
  * @return bar
  */
  @JsonProperty("bar")
  public String getBar() {
    return bar;
  }

  /**
   * Sets the <code>bar</code> property.
   * <br><em>N.B. <code>bar</code> is <b>read only</b>; client code should not call this method</em>.
   */
 public void setBar(String bar) {
    this.bar = bar;
  }

  /**
   * Sets the <code>bar</code> property.
   * <br><em>N.B. <code>bar</code> is <b>read only</b>; client code should not call this method</em>.
   */
  public ReadOnlyFirst bar(String bar) {
    this.bar = bar;
    return this;
  }

 /**
  * Get baz
  * @return baz
  */
  @JsonProperty("baz")
  public String getBaz() {
    return baz;
  }

  /**
   * Sets the <code>baz</code> property.
   */
 public void setBaz(String baz) {
    this.baz = baz;
  }

  /**
   * Sets the <code>baz</code> property.
   */
  public ReadOnlyFirst baz(String baz) {
    this.baz = baz;
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
    ReadOnlyFirst readOnlyFirst = (ReadOnlyFirst) o;
    return Objects.equals(bar, readOnlyFirst.bar) &&
        Objects.equals(baz, readOnlyFirst.baz);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bar, baz);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ReadOnlyFirst {\n");
    
    sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
    sb.append("    baz: ").append(toIndentedString(baz)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private static String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

