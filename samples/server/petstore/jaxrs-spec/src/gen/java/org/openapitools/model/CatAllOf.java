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



@JsonTypeName("Cat_allOf")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class CatAllOf  implements Serializable {
  private @Valid Boolean declawed;

  protected CatAllOf(CatAllOfBuilder<?, ?> b) {
    this.declawed = b.declawed;
  }

  public CatAllOf() {
  }

  /**
   **/
  public CatAllOf declawed(Boolean declawed) {
    this.declawed = declawed;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("declawed")
  public Boolean getDeclawed() {
    return declawed;
  }

  @JsonProperty("declawed")
  public void setDeclawed(Boolean declawed) {
    this.declawed = declawed;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    CatAllOf catAllOf = (CatAllOf) o;
    return Objects.equals(this.declawed, catAllOf.declawed);
  }

  @Override
  public int hashCode() {
    return Objects.hash(declawed);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class CatAllOf {\n");
    
    sb.append("    declawed: ").append(toIndentedString(declawed)).append("\n");
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


  public static CatAllOfBuilder<?, ?> builder() {
    return new CatAllOfBuilderImpl();
  }

  private static final class CatAllOfBuilderImpl extends CatAllOfBuilder<CatAllOf, CatAllOfBuilderImpl> {

    @Override
    protected CatAllOfBuilderImpl self() {
      return this;
    }

    @Override
    public CatAllOf build() {
      return new CatAllOf(this);
    }
  }

  public static abstract class CatAllOfBuilder<C extends CatAllOf, B extends CatAllOfBuilder<C, B>>  {
    private Boolean declawed;
    protected abstract B self();

    public abstract C build();

    public B declawed(Boolean declawed) {
      this.declawed = declawed;
      return self();
    }
  }
}

