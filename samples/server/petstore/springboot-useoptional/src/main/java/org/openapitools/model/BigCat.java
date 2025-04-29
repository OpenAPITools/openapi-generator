package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonValue;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.openapitools.model.Cat;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import javax.validation.Valid;
import javax.validation.constraints.*;


import java.util.*;
import javax.annotation.Generated;

/**
 * BigCat
 */


@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class BigCat extends Cat {

  /**
   * Gets or Sets kind
   */
  public enum KindEnum {
    LIONS("lions"),
    
    TIGERS("tigers"),
    
    LEOPARDS("leopards"),
    
    JAGUARS("jaguars");

    private final String value;

    KindEnum(String value) {
      this.value = value;
    }

    @JsonValue
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }

    @JsonCreator
    public static KindEnum fromValue(String value) {
      for (KindEnum b : KindEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  private Optional<KindEnum> kind = Optional.empty();

  public BigCat() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public BigCat(String className) {
    super(className);
  }

  public BigCat kind(KindEnum kind) {
    this.kind = Optional.ofNullable(kind);
    return this;
  }

  /**
   * Get kind
   * @return kind
   */
  
  @ApiModelProperty(value = "")
  @JsonProperty("kind")
  public Optional<KindEnum> getKind() {
    return kind;
  }

  public void setKind(Optional<KindEnum> kind) {
    this.kind = kind;
  }


  public BigCat declawed(Boolean declawed) {
    super.declawed(declawed);
    return this;
  }

  public BigCat className(String className) {
    super.className(className);
    return this;
  }

  public BigCat color(String color) {
    super.color(color);
    return this;
  }
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    BigCat bigCat = (BigCat) o;
    return Objects.equals(this.kind, bigCat.kind) &&
        super.equals(o);
  }

  @Override
  public int hashCode() {
    return Objects.hash(kind, super.hashCode());
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class BigCat {\n");
    sb.append("    ").append(toIndentedString(super.toString())).append("\n");
    sb.append("    kind: ").append(toIndentedString(kind)).append("\n");
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
  
  public static class Builder extends Cat.Builder {

    private BigCat instance;

    public Builder() {
      this(new BigCat());
    }

    protected Builder(BigCat instance) {
      super(instance); // the parent builder shares the same instance
      this.instance = instance;
    }

    protected Builder copyOf(BigCat value) { 
      super.copyOf(value);
      this.instance.setKind(value.kind);
      return this;
    }

    public BigCat.Builder kind(KindEnum kind) {
      this.instance.kind(kind);
      return this;
    }
    
    @Override
    public BigCat.Builder declawed(Boolean declawed) {
      this.instance.declawed(declawed);
      return this;
    }
    
    @Override
    public BigCat.Builder className(String className) {
      this.instance.className(className);
      return this;
    }
    
    @Override
    public BigCat.Builder color(String color) {
      this.instance.color(color);
      return this;
    }
    
    /**
    * returns a built BigCat instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public BigCat build() {
      try {
        return this.instance;
      } finally {
        // ensure that this.instance is not reused
        super.build();
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
  public static BigCat.Builder builder() {
    return new BigCat.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public BigCat.Builder toBuilder() {
    BigCat.Builder builder = new BigCat.Builder();
    return builder.copyOf(this);
  }

}

