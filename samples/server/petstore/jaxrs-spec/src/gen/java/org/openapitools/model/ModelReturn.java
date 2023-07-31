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

/**
 * Model for testing reserved words
 **/
@ApiModel(description = "Model for testing reserved words")
@JsonTypeName("Return")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class ModelReturn  implements Serializable {
  private @Valid Integer _return;

  protected ModelReturn(ModelReturnBuilder<?, ?> b) {
    this._return = b._return;
  }

  public ModelReturn() {
  }

  /**
   **/
  public ModelReturn _return(Integer _return) {
    this._return = _return;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("return")
  public Integer getReturn() {
    return _return;
  }

  @JsonProperty("return")
  public void setReturn(Integer _return) {
    this._return = _return;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ModelReturn _return = (ModelReturn) o;
    return Objects.equals(this._return, _return._return);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_return);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelReturn {\n");
    
    sb.append("    _return: ").append(toIndentedString(_return)).append("\n");
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


  public static ModelReturnBuilder<?, ?> builder() {
    return new ModelReturnBuilderImpl();
  }

  private static final class ModelReturnBuilderImpl extends ModelReturnBuilder<ModelReturn, ModelReturnBuilderImpl> {

    @Override
    protected ModelReturnBuilderImpl self() {
      return this;
    }

    @Override
    public ModelReturn build() {
      return new ModelReturn(this);
    }
  }

  public static abstract class ModelReturnBuilder<C extends ModelReturn, B extends ModelReturnBuilder<C, B>>  {
    private Integer _return;
    protected abstract B self();

    public abstract C build();

    public B _return(Integer _return) {
      this._return = _return;
      return self();
    }
  }
}

