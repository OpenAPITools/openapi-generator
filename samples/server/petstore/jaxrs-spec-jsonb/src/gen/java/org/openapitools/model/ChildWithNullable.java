package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.ParentWithNullable;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;
import jakarta.json.bind.annotation.JsonbTypeDeserializer;
import jakarta.json.bind.annotation.JsonbTypeSerializer;
import jakarta.json.bind.serializer.DeserializationContext;
import jakarta.json.bind.serializer.JsonbDeserializer;
import jakarta.json.bind.serializer.JsonbSerializer;
import jakarta.json.bind.serializer.SerializationContext;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonParser;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.27.0-SNAPSHOT")
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

  
  @ApiModelProperty(value = "")
  @JsonbProperty("otherProperty")
  public String getOtherProperty() {
    return otherProperty;
  }

  @JsonbProperty("otherProperty")
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
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
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
