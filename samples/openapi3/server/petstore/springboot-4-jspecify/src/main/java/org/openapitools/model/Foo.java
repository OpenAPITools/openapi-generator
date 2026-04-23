package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.jspecify.annotations.Nullable;
import org.springframework.format.annotation.DateTimeFormat;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import tools.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import tools.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import tools.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import io.swagger.v3.oas.annotations.media.Schema;

import jakarta.xml.bind.annotation.*;

import java.util.*;
import jakarta.annotation.Generated;

/**
 * Foo
 */

@JacksonXmlRootElement(localName = "Foo")
@XmlRootElement(name = "Foo")
@XmlAccessorType(XmlAccessType.FIELD)
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.22.0-SNAPSHOT")
public class Foo {

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private @Nullable OffsetDateTime dt;

  private org.springframework.core.io.@Nullable Resource binary;

  @Valid
  private List<OffsetDateTime> listOfDt = new ArrayList<>();

  @Valid
  private List<OffsetDateTime> listMinIntems = new ArrayList<>();

  @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
  private OffsetDateTime requiredDt;

  private @Nullable BigDecimal number;

  public Foo() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public Foo(OffsetDateTime requiredDt) {
    this.requiredDt = requiredDt;
  }

  /**
   * Constructor with all args parameters
   */
  public Foo(OffsetDateTime dt, org.springframework.core.io.Resource binary, List<OffsetDateTime> listOfDt, List<OffsetDateTime> listMinIntems, OffsetDateTime requiredDt, BigDecimal number) {
      this.dt = dt;
      this.binary = binary;
      this.listOfDt = listOfDt;
      this.listMinIntems = listMinIntems;
      this.requiredDt = requiredDt;
      this.number = number;
  }

  public Foo dt(OffsetDateTime dt) {
    this.dt = dt;
    return this;
  }

  /**
   * Get dt
   * @return dt
   */
  @Valid 
  @Schema(name = "dt", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("dt")
  @JacksonXmlProperty(localName = "dt")
  @XmlElement(name = "dt")
  public @Nullable OffsetDateTime getDt() {
    return dt;
  }

  @JsonProperty("dt")
  @JacksonXmlProperty(localName = "dt")
  public void setDt(@Nullable OffsetDateTime dt) {
    this.dt = dt;
  }

  public Foo binary(org.springframework.core.io.Resource binary) {
    this.binary = binary;
    return this;
  }

  /**
   * Get binary
   * @return binary
   */
  @Valid 
  @Schema(name = "binary", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("binary")
  @JacksonXmlProperty(localName = "binary")
  @XmlElement(name = "binary")
  public org.springframework.core.io.@Nullable Resource getBinary() {
    return binary;
  }

  @JsonProperty("binary")
  @JacksonXmlProperty(localName = "binary")
  public void setBinary(org.springframework.core.io.@Nullable Resource binary) {
    this.binary = binary;
  }

  public Foo listOfDt(List<OffsetDateTime> listOfDt) {
    this.listOfDt = listOfDt;
    return this;
  }

  public Foo addListOfDtItem(OffsetDateTime listOfDtItem) {
    if (this.listOfDt == null) {
      this.listOfDt = new ArrayList<>();
    }
    this.listOfDt.add(listOfDtItem);
    return this;
  }

  /**
   * Get listOfDt
   * @return listOfDt
   */
  @Valid 
  @Schema(name = "listOfDt", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("listOfDt")
  @JacksonXmlProperty(localName = "listOfDt")
  @JacksonXmlElementWrapper(useWrapping = false)
  @XmlElement(name = "listOfDt")
  public List<OffsetDateTime> getListOfDt() {
    return listOfDt;
  }

  @JsonProperty("listOfDt")
  @JacksonXmlProperty(localName = "listOfDt")
  @JacksonXmlElementWrapper(useWrapping = false)
  public void setListOfDt(List<OffsetDateTime> listOfDt) {
    this.listOfDt = listOfDt;
  }

  public Foo listMinIntems(List<OffsetDateTime> listMinIntems) {
    this.listMinIntems = listMinIntems;
    return this;
  }

  public Foo addListMinIntemsItem(OffsetDateTime listMinIntemsItem) {
    if (this.listMinIntems == null) {
      this.listMinIntems = new ArrayList<>();
    }
    this.listMinIntems.add(listMinIntemsItem);
    return this;
  }

  /**
   * Get listMinIntems
   * @return listMinIntems
   */
  @Valid @Size(min = 1) 
  @Schema(name = "listMinIntems", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("listMinIntems")
  @JacksonXmlProperty(localName = "listMinIntems")
  @JacksonXmlElementWrapper(useWrapping = false)
  @XmlElement(name = "listMinIntems")
  public List<OffsetDateTime> getListMinIntems() {
    return listMinIntems;
  }

  @JsonProperty("listMinIntems")
  @JacksonXmlProperty(localName = "listMinIntems")
  @JacksonXmlElementWrapper(useWrapping = false)
  public void setListMinIntems(List<OffsetDateTime> listMinIntems) {
    this.listMinIntems = listMinIntems;
  }

  public Foo requiredDt(OffsetDateTime requiredDt) {
    this.requiredDt = requiredDt;
    return this;
  }

  /**
   * Get requiredDt
   * @return requiredDt
   */
  @NotNull @Valid 
  @Schema(name = "requiredDt", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("requiredDt")
  @JacksonXmlProperty(localName = "requiredDt")
  @XmlElement(name = "requiredDt")
  public OffsetDateTime getRequiredDt() {
    return requiredDt;
  }

  @JsonProperty("requiredDt")
  @JacksonXmlProperty(localName = "requiredDt")
  public void setRequiredDt(OffsetDateTime requiredDt) {
    this.requiredDt = requiredDt;
  }

  public Foo number(BigDecimal number) {
    this.number = number;
    return this;
  }

  /**
   * Get number
   * @return number
   */
  @Valid 
  @Schema(name = "number", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("number")
  @JacksonXmlProperty(localName = "number")
  @XmlElement(name = "number")
  public @Nullable BigDecimal getNumber() {
    return number;
  }

  @JsonProperty("number")
  @JacksonXmlProperty(localName = "number")
  public void setNumber(@Nullable BigDecimal number) {
    this.number = number;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Foo foo = (Foo) o;
    return Objects.equals(this.dt, foo.dt) &&
        Objects.equals(this.binary, foo.binary) &&
        Objects.equals(this.listOfDt, foo.listOfDt) &&
        Objects.equals(this.listMinIntems, foo.listMinIntems) &&
        Objects.equals(this.requiredDt, foo.requiredDt) &&
        Objects.equals(this.number, foo.number);
  }

  @Override
  public int hashCode() {
    return Objects.hash(dt, binary, listOfDt, listMinIntems, requiredDt, number);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Foo {\n");
    sb.append("    dt: ").append(toIndentedString(dt)).append("\n");
    sb.append("    binary: ").append(toIndentedString(binary)).append("\n");
    sb.append("    listOfDt: ").append(toIndentedString(listOfDt)).append("\n");
    sb.append("    listMinIntems: ").append(toIndentedString(listMinIntems)).append("\n");
    sb.append("    requiredDt: ").append(toIndentedString(requiredDt)).append("\n");
    sb.append("    number: ").append(toIndentedString(number)).append("\n");
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
  
  public static class Builder {

    private Foo instance;

    public Builder() {
      this(new Foo());
    }

    protected Builder(Foo instance) {
      this.instance = instance;
    }

    protected Builder copyOf(Foo value) { 
      this.instance.setDt(value.dt);
      this.instance.setBinary(value.binary);
      this.instance.setListOfDt(value.listOfDt);
      this.instance.setListMinIntems(value.listMinIntems);
      this.instance.setRequiredDt(value.requiredDt);
      this.instance.setNumber(value.number);
      return this;
    }

    public Foo.Builder dt(OffsetDateTime dt) {
      this.instance.dt(dt);
      return this;
    }
    
    public Foo.Builder binary(org.springframework.core.io.Resource binary) {
      this.instance.binary(binary);
      return this;
    }
    
    public Foo.Builder listOfDt(List<OffsetDateTime> listOfDt) {
      this.instance.listOfDt(listOfDt);
      return this;
    }
    
    public Foo.Builder listMinIntems(List<OffsetDateTime> listMinIntems) {
      this.instance.listMinIntems(listMinIntems);
      return this;
    }
    
    public Foo.Builder requiredDt(OffsetDateTime requiredDt) {
      this.instance.requiredDt(requiredDt);
      return this;
    }
    
    public Foo.Builder number(BigDecimal number) {
      this.instance.number(number);
      return this;
    }
    
    /**
    * returns a built Foo instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public Foo build() {
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
  public static Foo.Builder builder() {
    return new Foo.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public Foo.Builder toBuilder() {
    Foo.Builder builder = new Foo.Builder();
    return builder.copyOf(this);
  }

}

