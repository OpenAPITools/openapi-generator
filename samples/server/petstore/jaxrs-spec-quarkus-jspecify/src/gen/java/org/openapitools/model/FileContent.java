package org.openapitools.model;

import org.jspecify.annotations.Nullable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("FileContent")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class FileContent   {
  private String name;
  private @Nullable Integer size;
  public enum VirusScanEnum {

    CLEAN(String.valueOf("clean")), DETECTED(String.valueOf("detected"));


    private String value;

    VirusScanEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    @JsonValue
    public String toString() {
        return String.valueOf(value);
    }

    /**
     * Convert a String into String, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
    public static VirusScanEnum fromString(String s) {
        for (VirusScanEnum b : VirusScanEnum.values()) {
            // using Objects.toString() to be safe if value type non-object type
            // because types like 'int' etc. will be auto-boxed
            if (java.util.Objects.toString(b.value).equals(s)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected string value '" + s + "'");
    }

    @JsonCreator
    public static VirusScanEnum fromValue(String value) {
        for (VirusScanEnum b : VirusScanEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
}

  private @Nullable VirusScanEnum virusScan;

  public FileContent() {
  }

  @JsonCreator
  public FileContent(
    @JsonProperty(required = true, value = "name") String name
  ) {
    this.name = name;
  }

  /**
   **/
  public FileContent name(String name) {
    this.name = name;
    return this;
  }

  
  @JsonProperty(required = true, value = "name")
  public String getName() {
    return name;
  }

  @JsonProperty(required = true, value = "name")
  public void setName(String name) {
    this.name = name;
  }

  /**
   **/
  public FileContent size(@Nullable Integer size) {
    this.size = size;
    return this;
  }

  
  @JsonProperty("size")
  public @Nullable Integer getSize() {
    return size;
  }

  @JsonProperty("size")
  public void setSize(@Nullable Integer size) {
    this.size = size;
  }

  /**
   **/
  public FileContent virusScan(@Nullable VirusScanEnum virusScan) {
    this.virusScan = virusScan;
    return this;
  }

  
  @JsonProperty("virusScan")
  public @Nullable VirusScanEnum getVirusScan() {
    return virusScan;
  }

  @JsonProperty("virusScan")
  public void setVirusScan(@Nullable VirusScanEnum virusScan) {
    this.virusScan = virusScan;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FileContent fileContent = (FileContent) o;
    return Objects.equals(this.name, fileContent.name) &&
        Objects.equals(this.size, fileContent.size) &&
        Objects.equals(this.virusScan, fileContent.virusScan);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, size, virusScan);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FileContent {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    size: ").append(toIndentedString(size)).append("\n");
    sb.append("    virusScan: ").append(toIndentedString(virusScan)).append("\n");
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
