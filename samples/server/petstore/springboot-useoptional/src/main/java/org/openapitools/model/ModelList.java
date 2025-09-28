package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * ModelList
 */

@JsonTypeName("List")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class ModelList {

  private Optional<String> _123list = Optional.empty();

  public ModelList _123list(String _123list) {
    this._123list = Optional.ofNullable(_123list);
    return this;
  }

  /**
   * Get _123list
   * @return _123list
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("123-list")
  public Optional<String> get123list() {
    return _123list;
  }

  public void set123list(Optional<String> _123list) {
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
  
  public static class Builder {

    private ModelList instance;

    public Builder() {
      this(new ModelList());
    }

    protected Builder(ModelList instance) {
      this.instance = instance;
    }

    protected Builder copyOf(ModelList value) { 
      this.instance.set123list(value._123list);
      return this;
    }

    public ModelList.Builder _123list(String _123list) {
      this.instance._123list(_123list);
      return this;
    }
    
    /**
    * returns a built ModelList instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public ModelList build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        this.instance = null;
      }
    }

    @Override
    public String toString() {
      return getClass() + "=(" + instance + ")";
    }
  }

  /**
  * Create a builder with no initialized field (except for the default values).
  */
  public static ModelList.Builder builder() {
    return new ModelList.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public ModelList.Builder toBuilder() {
    ModelList.Builder builder = new ModelList.Builder();
    return builder.copyOf(this);
  }

}

