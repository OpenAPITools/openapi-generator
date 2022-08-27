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



@JsonTypeName("List")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class ModelList  implements Serializable {
  private @Valid String _123List;

  protected ModelList(ModelListBuilder<?, ?> b) {
    this._123List = b._123List;
  }

  public ModelList() {
  }

  /**
   **/
  public ModelList _123List(String _123List) {
    this._123List = _123List;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("123-list")
  public String get123List() {
    return _123List;
  }

  @JsonProperty("123-list")
  public void set123List(String _123List) {
    this._123List = _123List;
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
    return Objects.equals(this._123List, _list._123List);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_123List);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ModelList {\n");
    
    sb.append("    _123List: ").append(toIndentedString(_123List)).append("\n");
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
    private String _123List;
    protected abstract B self();

    public abstract C build();

    public B _123List(String _123List) {
      this._123List = _123List;
      return self();
    }
  }
}

