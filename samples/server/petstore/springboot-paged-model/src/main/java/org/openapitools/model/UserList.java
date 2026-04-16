package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.openapitools.model.User;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.io.Serializable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;


import java.util.*;
import jakarta.annotation.Generated;

/**
 * A plain list wrapper — has content array but no pagination metadata property
 */

@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.22.0-SNAPSHOT")
public class UserList implements Serializable {

  private static final long serialVersionUID = 1L;

  @Valid
  private List<@Valid User> content = new ArrayList<>();

  private @Nullable String description;

  public UserList content(List<@Valid User> content) {
    this.content = content;
    return this;
  }

  public UserList addContentItem(User contentItem) {
    if (this.content == null) {
      this.content = new ArrayList<>();
    }
    this.content.add(contentItem);
    return this;
  }

  /**
   * Get content
   * @return content
   */
  @Valid 
  @JsonProperty("content")
  public List<@Valid User> getContent() {
    return content;
  }

  @JsonProperty("content")
  public void setContent(List<@Valid User> content) {
    this.content = content;
  }

  public UserList description(@Nullable String description) {
    this.description = description;
    return this;
  }

  /**
   * Get description
   * @return description
   */
  
  @JsonProperty("description")
  public @Nullable String getDescription() {
    return description;
  }

  @JsonProperty("description")
  public void setDescription(@Nullable String description) {
    this.description = description;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UserList userList = (UserList) o;
    return Objects.equals(this.content, userList.content) &&
        Objects.equals(this.description, userList.description);
  }

  @Override
  public int hashCode() {
    return Objects.hash(content, description);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class UserList {\n");
    sb.append("    content: ").append(toIndentedString(content)).append("\n");
    sb.append("    description: ").append(toIndentedString(description)).append("\n");
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
}

