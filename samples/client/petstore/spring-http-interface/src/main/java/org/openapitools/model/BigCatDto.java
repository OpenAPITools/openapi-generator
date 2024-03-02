package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.model.CatDto;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.constraints.NotNull;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * BigCatDto
 */


@JsonTypeName("BigCat")
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public class BigCatDto extends CatDto {

  /**
   * Gets or Sets kind
   */
  public enum KindEnum {
    LIONS("lions"),
    
    TIGERS("tigers"),
    
    LEOPARDS("leopards"),
    
    JAGUARS("jaguars");

    private String value;

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

  private KindEnum kind;

  public BigCatDto kind(KindEnum kind) {
    this.kind = kind;
    return this;
  }

  /**
   * Get kind
   * @return kind
  */
  
  @JsonProperty("kind")
  public KindEnum getKind() {
    return kind;
  }

  public void setKind(KindEnum kind) {
    this.kind = kind;
  }


  public BigCatDto declawed(Boolean declawed) {
    super.declawed(declawed);
    return this;
  }

  public BigCatDto className(String className) {
    super.className(className);
    return this;
  }

  public BigCatDto color(String color) {
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
    BigCatDto bigCat = (BigCatDto) o;
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
    sb.append("class BigCatDto {\n");
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
  
  public static class Builder extends CatDto.Builder {

    private BigCatDto instance;

    public Builder() {
      this(new BigCatDto());
    }

    protected Builder(BigCatDto instance) {
      super(instance);
      this.instance = instance;
    }

    public BigCatDto.Builder kind(KindEnum kind) {
      this.instance.kind(kind);
      return this;
    }

    @Override
    public BigCatDto.Builder declawed(Boolean declawed) {
      this.instance.declawed(declawed);
      return this;
    }

    @Override
    public BigCatDto.Builder className(String className) {
      this.instance.className(className);
      return this;
    }

    @Override
    public BigCatDto.Builder color(String color) {
      this.instance.color(color);
      return this;
    }
    /**
    * returns a built BigCatDto instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public BigCatDto build() {
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
  * Create a builder with no initialized field.
  */
  public static BigCatDto.Builder builder() {
    return new BigCatDto.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public BigCatDto.Builder toBuilder() {
    BigCatDto.Builder builder = new BigCatDto.Builder();
    builder.instance.setKind(kind);
    builder.instance.setDeclawed(getDeclawed());
    builder.instance.setClassName(getClassName());
    builder.instance.setColor(getColor());
    return builder;
  }

}

