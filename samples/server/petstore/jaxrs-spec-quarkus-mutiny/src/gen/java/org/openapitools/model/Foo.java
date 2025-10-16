package org.openapitools.model;

import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;




@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("Foo")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class Foo  implements Serializable {
  private String bar = "bar";

  protected Foo(FooBuilder<?, ?> b) {
    this.bar = b.bar;
  }

  public Foo() {
  }

  /**
   **/
  public Foo bar(String bar) {
    this.bar = bar;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("bar")
  public String getBar() {
    return bar;
  }

  @JsonProperty("bar")
  public void setBar(String bar) {
    this.bar = bar;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Foo foo = (Foo) o;
    return Objects.equals(this.bar, foo.bar);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bar);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Foo {\n");
    
    sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
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


  public static FooBuilder<?, ?> builder() {
    return new FooBuilderImpl();
  }

  private static final class FooBuilderImpl extends FooBuilder<Foo, FooBuilderImpl> {

    @Override
    protected FooBuilderImpl self() {
      return this;
    }

    @Override
    public Foo build() {
      return new Foo(this);
    }
  }

  public static abstract class FooBuilder<C extends Foo, B extends FooBuilder<C, B>>  {
    private String bar = "bar";
    protected abstract B self();

    public abstract C build();

    public B bar(String bar) {
      this.bar = bar;
      return self();
    }
  }
}

