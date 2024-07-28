package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.model.Foo;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("_foo_get_default_response")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.8.0-SNAPSHOT")
public class FooGetDefaultResponse  implements Serializable {
  private Foo _string;

  protected FooGetDefaultResponse(FooGetDefaultResponseBuilder<?, ?> b) {
    this._string = b._string;
  }

  public FooGetDefaultResponse() {
  }

  /**
   **/
  public FooGetDefaultResponse _string(Foo _string) {
    this._string = _string;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("string")
  @Valid public Foo getString() {
    return _string;
  }

  @JsonProperty("string")
  public void setString(Foo _string) {
    this._string = _string;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FooGetDefaultResponse fooGetDefaultResponse = (FooGetDefaultResponse) o;
    return Objects.equals(this._string, fooGetDefaultResponse._string);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_string);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FooGetDefaultResponse {\n");
    
    sb.append("    _string: ").append(toIndentedString(_string)).append("\n");
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


  public static FooGetDefaultResponseBuilder<?, ?> builder() {
    return new FooGetDefaultResponseBuilderImpl();
  }

  private static class FooGetDefaultResponseBuilderImpl extends FooGetDefaultResponseBuilder<FooGetDefaultResponse, FooGetDefaultResponseBuilderImpl> {

    @Override
    protected FooGetDefaultResponseBuilderImpl self() {
      return this;
    }

    @Override
    public FooGetDefaultResponse build() {
      return new FooGetDefaultResponse(this);
    }
  }

  public static abstract class FooGetDefaultResponseBuilder<C extends FooGetDefaultResponse, B extends FooGetDefaultResponseBuilder<C, B>>  {
    private Foo _string;
    protected abstract B self();

    public abstract C build();

    public B _string(Foo _string) {
      this._string = _string;
      return self();
    }
  }
}

