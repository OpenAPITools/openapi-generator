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
@JsonTypeName("DeprecatedObject")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class DeprecatedObject  implements Serializable {
  private String name;

  protected DeprecatedObject(DeprecatedObjectBuilder<?, ?> b) {
    this.name = b.name;
  }

  public DeprecatedObject() {
  }

  /**
   **/
  public DeprecatedObject name(String name) {
    this.name = name;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("name")
  public String getName() {
    return name;
  }

  @JsonProperty("name")
  public void setName(String name) {
    this.name = name;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    DeprecatedObject deprecatedObject = (DeprecatedObject) o;
    return Objects.equals(this.name, deprecatedObject.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class DeprecatedObject {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
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


  public static DeprecatedObjectBuilder<?, ?> builder() {
    return new DeprecatedObjectBuilderImpl();
  }

  private static final class DeprecatedObjectBuilderImpl extends DeprecatedObjectBuilder<DeprecatedObject, DeprecatedObjectBuilderImpl> {

    @Override
    protected DeprecatedObjectBuilderImpl self() {
      return this;
    }

    @Override
    public DeprecatedObject build() {
      return new DeprecatedObject(this);
    }
  }

  public static abstract class DeprecatedObjectBuilder<C extends DeprecatedObject, B extends DeprecatedObjectBuilder<C, B>>  {
    private String name;
    protected abstract B self();

    public abstract C build();

    public B name(String name) {
      this.name = name;
      return self();
    }
  }
}

