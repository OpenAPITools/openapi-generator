package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("List")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class ModelList  implements Serializable {
  private @Valid String _123list;

  protected ModelList(ModelListBuilder<?, ?> b) {
    this._123list = b._123list;
  }

  public ModelList() {
  }

  /**
   **/
  public ModelList _123list(String _123list) {
    this._123list = _123list;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("123-list")
  public String get123list() {
    return _123list;
  }

  @JsonProperty("123-list")
  public void set123list(String _123list) {
    this._123list = _123list;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ModelList _list = (ModelList) o;
    return Objects.equals(this._123list, _list._123list);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_123list);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelList {\n");
    
    sb.append("    _123list: ").append(toIndentedString(_123list)).append("\n");
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


  public static ModelListBuilder<?, ?> builder() {
    return new ModelListBuilderImpl();
  }

  private static final class ModelListBuilderImpl extends ModelListBuilder<ModelList, ModelListBuilderImpl> {

    @Override
    protected ModelListBuilderImpl self() {
      return this;
    }

    @Override
    public ModelList build() {
      return new ModelList(this);
    }
  }

  public static abstract class ModelListBuilder<C extends ModelList, B extends ModelListBuilder<C, B>>  {
    private String _123list;
    protected abstract B self();

    public abstract C build();

    public B _123list(String _123list) {
      this._123list = _123list;
      return self();
    }
  }
}

