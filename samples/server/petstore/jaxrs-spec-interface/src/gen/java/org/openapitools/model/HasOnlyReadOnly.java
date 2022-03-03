package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("hasOnlyReadOnly")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public class HasOnlyReadOnly  implements Serializable {
  
  private @Valid String bar;
  private @Valid String foo;

  /**
   **/
  public HasOnlyReadOnly bar(String bar) {
    this.bar = bar;
    return this;
  }

  

  
  @ApiModelProperty(value = "")
  @JsonProperty("bar")
  public String getBar() {
    return bar;
  }

  @JsonProperty("bar")
  public void setBar(String bar) {
    this.bar = bar;
  }

/**
   **/
  public HasOnlyReadOnly foo(String foo) {
    this.foo = foo;
    return this;
  }

  

  
  @ApiModelProperty(value = "")
  @JsonProperty("foo")
  public String getFoo() {
    return foo;
  }

  @JsonProperty("foo")
  public void setFoo(String foo) {
    this.foo = foo;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    HasOnlyReadOnly hasOnlyReadOnly = (HasOnlyReadOnly) o;
    return Objects.equals(this.bar, hasOnlyReadOnly.bar) &&
        Objects.equals(this.foo, hasOnlyReadOnly.foo);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bar, foo);
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
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }


}

