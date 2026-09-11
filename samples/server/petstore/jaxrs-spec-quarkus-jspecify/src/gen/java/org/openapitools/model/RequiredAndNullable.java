package org.openapitools.model;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.jspecify.annotations.Nullable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("RequiredAndNullable")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class RequiredAndNullable   {
  private @Nullable String str;
  private @Nullable File _file;
  private @Nullable String color = "red";
  private String onlyRequired;
  private @Valid @Nullable List<String> _list;

  public RequiredAndNullable() {
  }

  @JsonCreator
  public RequiredAndNullable(
    @JsonProperty(required = true, value = "str") String str,
    @JsonProperty(required = true, value = "file") File _file,
    @JsonProperty(required = true, value = "color") String color,
    @JsonProperty(required = true, value = "onlyRequired") String onlyRequired,
    @JsonProperty(required = true, value = "list") List<String> _list
  ) {
    this.str = str;
    this._file = _file;
    this.color = color;
    this.onlyRequired = onlyRequired;
    this._list = _list;
  }

  /**
   **/
  public RequiredAndNullable str(@Nullable String str) {
    this.str = str;
    return this;
  }

  
  @JsonProperty(required = true, value = "str")
  @NotNull public @Nullable String getStr() {
    return str;
  }

  @JsonProperty(required = true, value = "str")
  public void setStr(@Nullable String str) {
    this.str = str;
  }

  /**
   **/
  public RequiredAndNullable _file(@Nullable File _file) {
    this._file = _file;
    return this;
  }

  
  @JsonProperty(required = true, value = "file")
  @NotNull public @Nullable File getFile() {
    return _file;
  }

  @JsonProperty(required = true, value = "file")
  public void setFile(@Nullable File _file) {
    this._file = _file;
  }

  /**
   **/
  public RequiredAndNullable color(@Nullable String color) {
    this.color = color;
    return this;
  }

  
  @JsonProperty(required = true, value = "color")
  @NotNull public @Nullable String getColor() {
    return color;
  }

  @JsonProperty(required = true, value = "color")
  public void setColor(@Nullable String color) {
    this.color = color;
  }

  /**
   **/
  public RequiredAndNullable onlyRequired(String onlyRequired) {
    this.onlyRequired = onlyRequired;
    return this;
  }

  
  @JsonProperty(required = true, value = "onlyRequired")
  @NotNull public String getOnlyRequired() {
    return onlyRequired;
  }

  @JsonProperty(required = true, value = "onlyRequired")
  public void setOnlyRequired(String onlyRequired) {
    this.onlyRequired = onlyRequired;
  }

  /**
   **/
  public RequiredAndNullable _list(@Nullable List<String> _list) {
    this._list = _list;
    return this;
  }

  
  @JsonProperty(required = true, value = "list")
  @NotNull public @Nullable List<String> getList() {
    return _list;
  }

  @JsonProperty(required = true, value = "list")
  public void setList(@Nullable List<String> _list) {
    this._list = _list;
  }

  public RequiredAndNullable addListItem(String _listItem) {
    if (this._list == null) {
      this._list = new ArrayList<>();
    }

    this._list.add(_listItem);
    return this;
  }

  public RequiredAndNullable removeListItem(String _listItem) {
    if (_listItem != null && this._list != null) {
      this._list.remove(_listItem);
    }

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
    RequiredAndNullable requiredAndNullable = (RequiredAndNullable) o;
    return Objects.equals(this.str, requiredAndNullable.str) &&
        Objects.equals(this._file, requiredAndNullable._file) &&
        Objects.equals(this.color, requiredAndNullable.color) &&
        Objects.equals(this.onlyRequired, requiredAndNullable.onlyRequired) &&
        Objects.equals(this._list, requiredAndNullable._list);
  }

  @Override
  public int hashCode() {
    return Objects.hash(str, _file, color, onlyRequired, _list);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class RequiredAndNullable {\n");
    
    sb.append("    str: ").append(toIndentedString(str)).append("\n");
    sb.append("    _file: ").append(toIndentedString(_file)).append("\n");
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
  private String toIndentedString(Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }


}
