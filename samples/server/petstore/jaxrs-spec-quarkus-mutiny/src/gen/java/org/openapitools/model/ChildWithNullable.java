package org.openapitools.model;

import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.model.ParentWithNullable;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("ChildWithNullable")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class ChildWithNullable extends ParentWithNullable implements Serializable {
  private String otherProperty;

  protected ChildWithNullable(ChildWithNullableBuilder<?, ?> b) {
    super(b);
    this.otherProperty = b.otherProperty;
  }

  public ChildWithNullable() {
  }

  /**
   **/
  public ChildWithNullable otherProperty(String otherProperty) {
    this.otherProperty = otherProperty;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("otherProperty")
  public String getOtherProperty() {
    return otherProperty;
  }

  @JsonProperty("otherProperty")
  public void setOtherProperty(String otherProperty) {
    this.otherProperty = otherProperty;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ChildWithNullable childWithNullable = (ChildWithNullable) o;
    return Objects.equals(this.otherProperty, childWithNullable.otherProperty) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(otherProperty, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ChildWithNullable {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    otherProperty: ").append(toIndentedString(otherProperty)).append("\n");
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


  public static ChildWithNullableBuilder<?, ?> builder() {
    return new ChildWithNullableBuilderImpl();
  }

  private static final class ChildWithNullableBuilderImpl extends ChildWithNullableBuilder<ChildWithNullable, ChildWithNullableBuilderImpl> {

    @Override
    protected ChildWithNullableBuilderImpl self() {
      return this;
    }

    @Override
    public ChildWithNullable build() {
      return new ChildWithNullable(this);
    }
  }

  public static abstract class ChildWithNullableBuilder<C extends ChildWithNullable, B extends ChildWithNullableBuilder<C, B>> extends ParentWithNullableBuilder<C, B> {
    private String otherProperty;

    public B otherProperty(String otherProperty) {
      this.otherProperty = otherProperty;
      return self();
    }
  }
}

