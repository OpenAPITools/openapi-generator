package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import java.util.UUID;
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
 * UploadPostDefaultResponse
 */

@JsonTypeName("_upload_post_default_response")
@JacksonXmlRootElement(localName = "UploadPostDefaultResponse")
@XmlRootElement(name = "UploadPostDefaultResponse")
@XmlAccessorType(XmlAccessType.FIELD)
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.23.0-SNAPSHOT")
public class UploadPostDefaultResponse {

  private @Nullable UUID id;

  public UploadPostDefaultResponse() {
    super();
  }

  /**
   * Constructor with all args parameters
   */
  public UploadPostDefaultResponse(UUID id) {
      this.id = id;
  }

  public UploadPostDefaultResponse id(UUID id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
   */
  @Valid 
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")
  @JacksonXmlProperty(localName = "id")
  @XmlElement(name = "id")
  public @Nullable UUID getId() {
    return id;
  }

  @JsonProperty("id")
  @JacksonXmlProperty(localName = "id")
  public void setId(@Nullable UUID id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UploadPostDefaultResponse uploadPostDefaultResponse = (UploadPostDefaultResponse) o;
    return Objects.equals(this.id, uploadPostDefaultResponse.id);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class UploadPostDefaultResponse {\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
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

    private UploadPostDefaultResponse instance;

    public Builder() {
      this(new UploadPostDefaultResponse());
    }

    protected Builder(UploadPostDefaultResponse instance) {
      this.instance = instance;
    }

    protected Builder copyOf(UploadPostDefaultResponse value) { 
      this.instance.setId(value.id);
      return this;
    }

    public UploadPostDefaultResponse.Builder id(UUID id) {
      this.instance.id(id);
      return this;
    }
    
    /**
    * returns a built UploadPostDefaultResponse instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public UploadPostDefaultResponse build() {
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
  public static UploadPostDefaultResponse.Builder builder() {
    return new UploadPostDefaultResponse.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public UploadPostDefaultResponse.Builder toBuilder() {
    UploadPostDefaultResponse.Builder builder = new UploadPostDefaultResponse.Builder();
    return builder.copyOf(this);
  }

}

