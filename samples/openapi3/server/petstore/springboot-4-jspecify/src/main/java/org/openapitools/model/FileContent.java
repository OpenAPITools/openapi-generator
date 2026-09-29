package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonSetter;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.Nulls;
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
 * FileContent
 */

@JacksonXmlRootElement(localName = "FileContent")
@XmlRootElement(name = "FileContent")
@XmlAccessorType(XmlAccessType.FIELD)
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class FileContent {

  @JsonInclude(JsonInclude.Include.NON_NULL)
  private String name;

  @JsonInclude(JsonInclude.Include.NON_NULL)
  @JsonSetter(nulls = Nulls.SKIP)
  private @Nullable Integer size;

  /**
   * Gets or Sets virusScan
   */
  public enum VirusScanEnum {
    CLEAN("clean"),
    
    DETECTED("detected");

    private final String value;

    VirusScanEnum(String value) {
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
    public static VirusScanEnum fromValue(String value) {
      for (VirusScanEnum b : VirusScanEnum.values()) {
        if (b.value.equals(value)) {
          return b;
        }
      }
      throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }
  }

  @JsonInclude(JsonInclude.Include.NON_NULL)
  @JsonSetter(nulls = Nulls.SKIP)
  private @Nullable VirusScanEnum virusScan;

  public FileContent() {
    super();
  }

  /**
   * Constructor with only required parameters
   */
  public FileContent(String name) {
    this.name = name;
  }

  /**
   * Constructor with all args parameters
   */
  public FileContent(String name, @Nullable Integer size, @Nullable VirusScanEnum virusScan) {
      this.name = name;
      this.size = size;
      this.virusScan = virusScan;
  }

  public FileContent name(String name) {
    this.name = name;
    return this;
  }

  /**
   * Get name
   * @return name
   */
  
  @Schema(name = "name", accessMode = Schema.AccessMode.READ_ONLY, requiredMode = Schema.RequiredMode.REQUIRED)
  @JsonProperty("name")
  @JacksonXmlProperty(localName = "name")
  @XmlElement(name = "name")
  public String getName() {
    return name;
  }

  @JsonProperty("name")
  @JacksonXmlProperty(localName = "name")
  public void setName(String name) {
    this.name = name;
  }

  public FileContent size(@Nullable Integer size) {
    this.size = size;
    return this;
  }

  /**
   * Get size
   * @return size
   */
  
  @Schema(name = "size", accessMode = Schema.AccessMode.READ_ONLY, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("size")
  @JacksonXmlProperty(localName = "size")
  @XmlElement(name = "size")
  public @Nullable Integer getSize() {
    return size;
  }

  @JsonProperty("size")
  @JacksonXmlProperty(localName = "size")
  public void setSize(@Nullable Integer size) {
    this.size = size;
  }

  public FileContent virusScan(@Nullable VirusScanEnum virusScan) {
    this.virusScan = virusScan;
    return this;
  }

  /**
   * Get virusScan
   * @return virusScan
   */
  
  @Schema(name = "virusScan", accessMode = Schema.AccessMode.READ_ONLY, requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("virusScan")
  @JacksonXmlProperty(localName = "virusScan")
  @XmlElement(name = "virusScan")
  public @Nullable VirusScanEnum getVirusScan() {
    return virusScan;
  }

  @JsonProperty("virusScan")
  @JacksonXmlProperty(localName = "virusScan")
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
  private String toIndentedString(@Nullable Object o) {
    return o == null ? "null" : o.toString().replace("\n", "\n    ");
  }
  
  public static class Builder {

    private FileContent instance;

    public Builder() {
      this(new FileContent());
    }

    protected Builder(FileContent instance) {
      this.instance = instance;
    }

    protected Builder copyOf(FileContent value) { 
      this.instance.setName(value.name);
      this.instance.setSize(value.size);
      this.instance.setVirusScan(value.virusScan);
      return this;
    }

    public FileContent.Builder name(String name) {
      this.instance.name(name);
      return this;
    }
    
    public FileContent.Builder size(@Nullable Integer size) {
      this.instance.size(size);
      return this;
    }
    
    public FileContent.Builder virusScan(@Nullable VirusScanEnum virusScan) {
      this.instance.virusScan(virusScan);
      return this;
    }
    
    /**
    * returns a built FileContent instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public FileContent build() {
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
  public static FileContent.Builder builder() {
    return new FileContent.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public FileContent.Builder toBuilder() {
    FileContent.Builder builder = new FileContent.Builder();
    return builder.copyOf(this);
  }

}

