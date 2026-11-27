package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.Cat;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.28.0-SNAPSHOT")
public final class PersianCat extends Cat implements Serializable {
  private Boolean longHair;

  protected PersianCat(PersianCatBuilder<?, ?> b) {
    super(b);
    this.longHair = b.longHair;
  }

  public PersianCat() {
    this.setPetType("PERSIAN");
  }

  @JsonbCreator
  public PersianCat(
    @JsonbProperty("petType") String petType
  ) {
    super(
      petType
    );
  }

  /**
   **/
  public PersianCat longHair(Boolean longHair) {
    this.longHair = longHair;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("longHair")
  public Boolean getLongHair() {
    return longHair;
  }

  @JsonbProperty("longHair")
  public void setLongHair(Boolean longHair) {
    this.longHair = longHair;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PersianCat persianCat = (PersianCat) o;
    return Objects.equals(this.longHair, persianCat.longHair) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(longHair, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class PersianCat {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    longHair: ").append(toIndentedString(longHair)).append("\n");
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


  public static PersianCatBuilder<?, ?> builder() {
    return new PersianCatBuilderImpl();
  }

  private static final class PersianCatBuilderImpl extends PersianCatBuilder<PersianCat, PersianCatBuilderImpl> {

    @Override
    protected PersianCatBuilderImpl self() {
      return this;
    }

    @Override
    public PersianCat build() {
      return new PersianCat(this);
    }
  }

  public static abstract class PersianCatBuilder<C extends PersianCat, B extends PersianCatBuilder<C, B>> extends CatBuilder<C, B> {
    private Boolean longHair;

    {
      // the discriminator values of PersianCat, which the caller may override
      this.petType("PERSIAN");
    }

    public B longHair(Boolean longHair) {
      this.longHair = longHair;
      return self();
    }
  }
}
