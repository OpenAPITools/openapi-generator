package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * Model for testing reserved words
 */
@ApiModel(description = "Model for testing reserved words")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class ModelReturn   {
  @JsonProperty("return")
  private Integer _return;

  public ModelReturn _return(Integer _return) {
    this._return = _return;
    return this;
  }

  /**
   * Get _return
   * @return _return
  */
  @ApiModelProperty(value = "")


  public Integer getReturn() {
    return _return;
  }

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


}

