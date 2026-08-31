package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.jspecify.annotations.Nullable;
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
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class RequiredAndNullable {

  @JsonInclude(JsonInclude.Include.ALWAYS)
  private @Nullable String str = null;

  @JsonInclude(JsonInclude.Include.ALWAYS)
  private org.springframework.core.io.@Nullable Resource file = null;

  @JsonInclude(JsonInclude.Include.ALWAYS)
  private @Nullable String color = null;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String onlyRequired;

  @JsonInclude(JsonInclude.Include.ALWAYS)
  private @Nullable List<String> _list;

  public RequiredAndNullable() {
    super();
  }

  /**
   * Constructor with only required parameters and all parameters
   */
  public RequiredAndNullable(@Nullable String str, org.springframework.core.io.@Nullable Resource file, @Nullable String color, String onlyRequired, @Nullable List<String> _list) {
    this.str = str;
    this.file = file;
    this.color = color;
    this.onlyRequired = onlyRequired;
    this._list = _list;
  }

  public RequiredAndNullable str(@Nullable String str) {
    this.str = str;
    return this;
  }

  /**
   * Get str
   * @return str
   */
  
  @Schema(name = "str", requiredMode = Schema.RequiredMode.REQUIRED, nullable = true)
  @JsonProperty("str")
  @JacksonXmlProperty(localName = "str")
  @XmlElement(name = "str")
  public @Nullable String getStr() {
    return str;
  }

  @JsonProperty("str")
  @JacksonXmlProperty(localName = "str")
  public void setStr(@Nullable String str) {
    this.str = str;
  }

  public RequiredAndNullable file(org.springframework.core.io.@Nullable Resource file) {
    this.file = file;
    return this;
  }

  /**
   * Get file
   * @return file
   */
  @Valid 
  @Schema(name = "file", requiredMode = Schema.RequiredMode.REQUIRED, nullable = true)
  @JsonProperty("file")
  @JacksonXmlProperty(localName = "file")
  @XmlElement(name = "file")
  public org.springframework.core.io.@Nullable Resource getFile() {
    return file;
  }

  @JsonProperty("file")
  @JacksonXmlProperty(localName = "file")
  public void setFile(org.springframework.core.io.@Nullable Resource file) {
    this.file = file;
  }

  public RequiredAndNullable color(@Nullable String color) {
    this.color = color;
    return this;
  }

  /**
   * Get color
   * @return color
   */
  
  @Schema(name = "color", requiredMode = Schema.RequiredMode.REQUIRED, nullable = true)
  @JsonProperty("color")
  @JacksonXmlProperty(localName = "color")
  @XmlElement(name = "color")
  public @Nullable String getColor() {
    return color;
  }

  @JsonProperty("color")
  @JacksonXmlProperty(localName = "color")
  public void setColor(@Nullable String color) {
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

  public RequiredAndNullable _list(@Nullable List<String> _list) {
    this._list = _list;
    return this;
  }

  public RequiredAndNullable addListItem(String _listItem) {
    if (this._list == null) {
      this._list = new ArrayList<>();
    }
    this._list.add(_listItem);
    return this;
  }

  /**
   * Get _list
   * @return _list
   */
  
  @Schema(name = "list", requiredMode = Schema.RequiredMode.REQUIRED, nullable = true)
  @JsonProperty("list")
  @JacksonXmlProperty(localName = "list")
  @JacksonXmlElementWrapper(useWrapping = false)
  @XmlElement(name = "list")
  public @Nullable List<String> getList() {
    return _list;
  }

  @JsonProperty("list")
  @JacksonXmlProperty(localName = "list")
  @JacksonXmlElementWrapper(useWrapping = false)
  public void setList(@Nullable List<String> _list) {
    this._list = _list;
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
        Objects.equals(this.onlyRequired, requiredAndNullable.onlyRequired) &&
        Objects.equals(this._list, requiredAndNullable._list);
  }

  @Override
  public int hashCode() {
    return Objects.hash(str, file, color, onlyRequired, _list);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class RequiredAndNullable {\n");
    sb.append("    str: ").append(toIndentedString(str)).append("\n");
    sb.append("    file: ").append(toIndentedString(file)).append("\n");
    sb.append("    color: ").append(toIndentedString(color)).append("\n");
    sb.append("    onlyRequired: ").append(toIndentedString(onlyRequired)).append("\n");
    sb.append("    _list: ").append(toIndentedString(_list)).append("\n");
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
      this.instance.setList(value._list);
      return this;
    }

    public RequiredAndNullable.Builder str(@Nullable String str) {
      this.instance.str(str);
      return this;
    }
    
    public RequiredAndNullable.Builder file(org.springframework.core.io.@Nullable Resource file) {
      this.instance.file(file);
      return this;
    }
    
    public RequiredAndNullable.Builder color(@Nullable String color) {
      this.instance.color(color);
      return this;
    }
    
    public RequiredAndNullable.Builder onlyRequired(String onlyRequired) {
      this.instance.onlyRequired(onlyRequired);
      return this;
    }
    
    public RequiredAndNullable.Builder _list(@Nullable List<String> _list) {
      this.instance._list(_list);
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

