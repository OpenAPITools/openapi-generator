package org.openapitools.model;

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
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;


public class HasOnlyReadOnly  {
  
  @ApiModelProperty(value = "")
  private String bar;

  @ApiModelProperty(value = "")
  private String foo;
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
  public HasOnlyReadOnly bar(String bar) {
    this.bar = bar;
    return this;
  }

 /**
  * Get foo
  * @return foo
  */
  @JsonProperty("foo")
  public String getFoo() {
    return foo;
  }

  /**
   * Sets the <code>foo</code> property.
   * <br><em>N.B. <code>foo</code> is <b>read only</b>; client code should not call this method</em>.
   */
  public void setFoo(String foo) {
    this.foo = foo;
  }

  /**
   * Sets the <code>foo</code> property.
   * <br><em>N.B. <code>foo</code> is <b>read only</b>; client code should not call this method</em>.
   */
  public HasOnlyReadOnly foo(String foo) {
    this.foo = foo;
    return this;
  }


  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class HasOnlyReadOnly {\n");
    
    sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
    sb.append("    foo: ").append(toIndentedString(foo)).append("\n");
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

