package org.openapitools.model;

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



@JsonTypeName("ReadOnlyFirst")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class ReadOnlyFirst  implements Serializable {
  private @Valid String bar;
  private @Valid String baz;

  protected ReadOnlyFirst(ReadOnlyFirstBuilder<?, ?> b) {
    this.bar = b.bar;
    this.baz = b.baz;
  }

  public ReadOnlyFirst() {
  }

  /**
   **/
  public ReadOnlyFirst bar(String bar) {
    this.bar = bar;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("bar")
  public String getBar() {
    return bar;
  }

  @JsonProperty("bar")
  public void setBar(String bar) {
    this.bar = bar;
  }

  /**
   **/
  public ReadOnlyFirst baz(String baz) {
    this.baz = baz;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("baz")
  public String getBaz() {
    return baz;
  }

  @JsonProperty("baz")
  public void setBaz(String baz) {
    this.baz = baz;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ReadOnlyFirst readOnlyFirst = (ReadOnlyFirst) o;
    return Objects.equals(this.bar, readOnlyFirst.bar) &&
        Objects.equals(this.baz, readOnlyFirst.baz);
  }

  @Override
  public int hashCode() {
    return Objects.hash(bar, baz);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ReadOnlyFirst {\n");
    
    sb.append("    bar: ").append(toIndentedString(bar)).append("\n");
    sb.append("    baz: ").append(toIndentedString(baz)).append("\n");
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


  public static ReadOnlyFirstBuilder<?, ?> builder() {
    return new ReadOnlyFirstBuilderImpl();
  }

  private static final class ReadOnlyFirstBuilderImpl extends ReadOnlyFirstBuilder<ReadOnlyFirst, ReadOnlyFirstBuilderImpl> {

    @Override
    protected ReadOnlyFirstBuilderImpl self() {
      return this;
    }

    @Override
    public ReadOnlyFirst build() {
      return new ReadOnlyFirst(this);
    }
  }

  public static abstract class ReadOnlyFirstBuilder<C extends ReadOnlyFirst, B extends ReadOnlyFirstBuilder<C, B>>  {
    private String bar;
    private String baz;
    protected abstract B self();

    public abstract C build();

    public B bar(String bar) {
      this.bar = bar;
      return self();
    }
    public B baz(String baz) {
      this.baz = baz;
      return self();
    }
  }
}

