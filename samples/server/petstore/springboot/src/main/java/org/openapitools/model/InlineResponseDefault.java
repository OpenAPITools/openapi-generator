package org.openapitools.model;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import io.swagger.v3.oas.annotations.media.Schema;
import org.openapitools.model.Foo;
import org.openapitools.jackson.nullable.JsonNullable;
import javax.validation.Valid;
import javax.validation.constraints.*;

/**
 * InlineResponseDefault
 */

public class InlineResponseDefault   {
  @JsonProperty("string")
  private Foo string;

  public InlineResponseDefault string(Foo string) {
    this.string = string;
    return this;
  }

  /**
   * Get string
   * @return string
  */
  @Schema(description = "")

  @Valid

  public Foo getString() {
    return string;
  }

  public void setString(Foo string) {
    this.string = string;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    InlineResponseDefault inlineResponseDefault = (InlineResponseDefault) o;
    return Objects.equals(this.string, inlineResponseDefault.string);
  }

  @Override
  public int hashCode() {
    return Objects.hash(string);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class InlineResponseDefault {\n");
    
    sb.append("    string: ").append(toIndentedString(string)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

