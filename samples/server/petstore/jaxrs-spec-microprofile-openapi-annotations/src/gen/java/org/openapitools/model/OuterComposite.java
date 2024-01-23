package org.openapitools.model;

import java.math.BigDecimal;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("OuterComposite")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class OuterComposite  implements Serializable {
  private @Valid BigDecimal myNumber;
  private @Valid String myString;
  private @Valid Boolean myBoolean;

  protected OuterComposite(OuterCompositeBuilder<?, ?> b) {
    this.myNumber = b.myNumber;
    this.myString = b.myString;
    this.myBoolean = b.myBoolean;
  }

  public OuterComposite() {
  }

  /**
   **/
  public OuterComposite myNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("my_number")
  public BigDecimal getMyNumber() {
    return myNumber;
  }

  @JsonProperty("my_number")
  public void setMyNumber(BigDecimal myNumber) {
    this.myNumber = myNumber;
  }

  /**
   **/
  public OuterComposite myString(String myString) {
    this.myString = myString;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("my_string")
  public String getMyString() {
    return myString;
  }

  @JsonProperty("my_string")
  public void setMyString(String myString) {
    this.myString = myString;
  }

  /**
   **/
  public OuterComposite myBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("my_boolean")
  public Boolean getMyBoolean() {
    return myBoolean;
  }

  @JsonProperty("my_boolean")
  public void setMyBoolean(Boolean myBoolean) {
    this.myBoolean = myBoolean;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    OuterComposite outerComposite = (OuterComposite) o;
    return Objects.equals(this.myNumber, outerComposite.myNumber) &&
        Objects.equals(this.myString, outerComposite.myString) &&
        Objects.equals(this.myBoolean, outerComposite.myBoolean);
  }

  @Override
  public int hashCode() {
    return Objects.hash(myNumber, myString, myBoolean);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class OuterComposite {\n");
    
    sb.append("    myNumber: ").append(toIndentedString(myNumber)).append("\n");
    sb.append("    myString: ").append(toIndentedString(myString)).append("\n");
    sb.append("    myBoolean: ").append(toIndentedString(myBoolean)).append("\n");
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


  public static OuterCompositeBuilder<?, ?> builder() {
    return new OuterCompositeBuilderImpl();
  }

  private static final class OuterCompositeBuilderImpl extends OuterCompositeBuilder<OuterComposite, OuterCompositeBuilderImpl> {

    @Override
    protected OuterCompositeBuilderImpl self() {
      return this;
    }

    @Override
    public OuterComposite build() {
      return new OuterComposite(this);
    }
  }

  public static abstract class OuterCompositeBuilder<C extends OuterComposite, B extends OuterCompositeBuilder<C, B>>  {
    private BigDecimal myNumber;
    private String myString;
    private Boolean myBoolean;
    protected abstract B self();

    public abstract C build();

    public B myNumber(BigDecimal myNumber) {
      this.myNumber = myNumber;
      return self();
    }
    public B myString(String myString) {
      this.myString = myString;
      return self();
    }
    public B myBoolean(Boolean myBoolean) {
      this.myBoolean = myBoolean;
      return self();
    }
  }
}

