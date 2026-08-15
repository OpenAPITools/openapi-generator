package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.jspecify.annotations.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
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
 * RequiredAndNullable
 */

@JacksonXmlRootElement(localName = "RequiredAndNullable")
@XmlRootElement(name = "RequiredAndNullable")
@XmlAccessorType(XmlAccessType.FIELD)
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.25.0-SNAPSHOT")
public class RequiredAndNullable {

  private JsonNullable<String> str = JsonNullable.<String>undefined();

  private JsonNullable<org.springframework.core.io.Resource> file = JsonNullable.<org.springframework.core.io.Resource>undefined();

  private JsonNullable<String> color = JsonNullable.<String>undefined();

  private String onlyRequired;

  public RequiredAndNullable() {
    super();
  }

  /**
   * Constructor with only required parameters and all parameters
   */
  public RequiredAndNullable(@Nullable String str, org.springframework.core.io.@Nullable Resource file, @Nullable String color, String onlyRequired) {
    this.str = JsonNullable.of(str);
    this.file = JsonNullable.of(file);
    this.color = JsonNullable.of(color);
    this.onlyRequired = onlyRequired;
  }

  public RequiredAndNullable str(@Nullable String str) {
    this.str = JsonNullable.of(str);
    return this;
  }

  /**
   * Get str
   * @return str
   */
  @NotNull 
  @Schema(name = "str", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("str")
  @JacksonXmlProperty(localName = "str")
  @XmlElement(name = "str")
  public JsonNullable<String> getStr() {
    return str;
  }

  @JsonProperty("str")
  @JacksonXmlProperty(localName = "str")
  public void setStr(JsonNullable<String> str) {
    this.str = str;
  }

  public RequiredAndNullable file(org.springframework.core.io.@Nullable Resource file) {
    this.file = JsonNullable.of(file);
    return this;
  }

  /**
   * Get file
   * @return file
   */
  @NotNull @Valid 
  @Schema(name = "file", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("file")
  @JacksonXmlProperty(localName = "file")
  @XmlElement(name = "file")
  public JsonNullable<org.springframework.core.io.Resource> getFile() {
    return file;
  }

  @JsonProperty("file")
  @JacksonXmlProperty(localName = "file")
  public void setFile(JsonNullable<org.springframework.core.io.Resource> file) {
    this.file = file;
  }

  public RequiredAndNullable color(@Nullable String color) {
    this.color = JsonNullable.of(color);
    return this;
  }

  /**
   * Get color
   * @return color
   */
  @NotNull 
  @Schema(name = "color", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("color")
  @JacksonXmlProperty(localName = "color")
  @XmlElement(name = "color")
  public JsonNullable<String> getColor() {
    return color;
  }

  @JsonProperty("color")
  @JacksonXmlProperty(localName = "color")
  public void setColor(JsonNullable<String> color) {
    this.color = color;
  }

  public RequiredAndNullable onlyRequired(String onlyRequired) {
    this.onlyRequired = onlyRequired;
    return this;
  }

  /**
   * Get onlyRequired
   * @return onlyRequired
   */
  @NotNull 
  @Schema(name = "onlyRequired", requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("onlyRequired")
  @JacksonXmlProperty(localName = "onlyRequired")
  @XmlElement(name = "onlyRequired")
  public String getOnlyRequired() {
    return onlyRequired;
  }

  @JsonProperty("onlyRequired")
  @JacksonXmlProperty(localName = "onlyRequired")
  public void setOnlyRequired(String onlyRequired) {
    this.onlyRequired = onlyRequired;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    RequiredAndNullable requiredAndNullable = (RequiredAndNullable) o;
    return Objects.equals(this.str, requiredAndNullable.str) &&
        Objects.equals(this.file, requiredAndNullable.file) &&
        Objects.equals(this.color, requiredAndNullable.color) &&
        Objects.equals(this.onlyRequired, requiredAndNullable.onlyRequired);
  }

  @Override
  public int hashCode() {
    return Objects.hash(str, file, color, onlyRequired);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class RequiredAndNullable {\n");
    sb.append("    str: ").append(toIndentedString(str)).append("\n");
    sb.append("    file: ").append(toIndentedString(file)).append("\n");
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
    sb.append("    onlyRequired: ").append(toIndentedString(onlyRequired)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(@Nullable Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder {

    private RequiredAndNullable instance;

    public Builder() {
      this(new RequiredAndNullable());
    }

    protected Builder(RequiredAndNullable instance) {
      this.instance = instance;
    }

    protected Builder copyOf(RequiredAndNullable value) { 
      this.instance.setStr(value.str);
      this.instance.setFile(value.file);
      this.instance.setColor(value.color);
      this.instance.setOnlyRequired(value.onlyRequired);
      return this;
    }

    public RequiredAndNullable.Builder str(@Nullable String str) {
      this.instance.str(str);
      return this;
    }
    
    public RequiredAndNullable.Builder str(JsonNullable<String> str) {
      this.instance.str = str;
      return this;
    }
    
    public RequiredAndNullable.Builder file(org.springframework.core.io.@Nullable Resource file) {
      this.instance.file(file);
      return this;
    }
    
    public RequiredAndNullable.Builder file(JsonNullable<org.springframework.core.io.Resource> file) {
      this.instance.file = file;
      return this;
    }
    
    public RequiredAndNullable.Builder color(@Nullable String color) {
      this.instance.color(color);
      return this;
    }
    
    public RequiredAndNullable.Builder color(JsonNullable<String> color) {
      this.instance.color = color;
      return this;
    }
    
    public RequiredAndNullable.Builder onlyRequired(String onlyRequired) {
      this.instance.onlyRequired(onlyRequired);
      return this;
    }
    
    /**
    * returns a built RequiredAndNullable instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public RequiredAndNullable build() {
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
  public static RequiredAndNullable.Builder builder() {
    return new RequiredAndNullable.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public RequiredAndNullable.Builder toBuilder() {
    RequiredAndNullable.Builder builder = new RequiredAndNullable.Builder();
    return builder.copyOf(this);
  }

}

