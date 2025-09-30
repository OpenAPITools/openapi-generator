package org.openapitools.model;

import java.net.URI;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import org.springframework.lang.Nullable;
import org.openapitools.jackson.nullable.JsonNullable;
import java.time.OffsetDateTime;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import io.swagger.v3.oas.annotations.media.Schema;

import jakarta.xml.bind.annotation.*;

import java.util.*;
import jakarta.annotation.Generated;

/**
 * A User who is purchasing from the pet store
 */

@Schema(name = "User", description = "A User who is purchasing from the pet store")
@JacksonXmlRootElement(localName = "User")
@XmlRootElement(name = "User")
@XmlAccessorType(XmlAccessType.FIELD)
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public class User {

  private @Nullable Long id;

  private @Nullable String username;

  private @Nullable String firstName;

  private @Nullable String lastName;

  private @Nullable String email;

  private @Nullable String password;

  private @Nullable String phone;

  private @Nullable Integer userStatus;

  public User() {
    super();
  }

  /**
   * Constructor with all args parameters
   */
  public User(@Nullable Long id, @Nullable String username, @Nullable String firstName, @Nullable String lastName, @Nullable String email, @Nullable String password, @Nullable String phone, @Nullable Integer userStatus) {
      this.id = id;
      this.username = username;
      this.firstName = firstName;
      this.lastName = lastName;
      this.email = email;
      this.password = password;
      this.phone = phone;
      this.userStatus = userStatus;
  }

  public User id(@Nullable Long id) {
    this.id = id;
    return this;
  }

  /**
   * Get id
   * @return id
   */
  
  @Schema(name = "id", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("id")
  @JacksonXmlProperty(localName = "id")
  @XmlElement(name = "id")
  public @Nullable Long getId() {
    return id;
  }

  public void setId(@Nullable Long id) {
    this.id = id;
  }

  public User username(@Nullable String username) {
    this.username = username;
    return this;
  }

  /**
   * Get username
   * @return username
   */
  
  @Schema(name = "username", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("username")
  @JacksonXmlProperty(localName = "username")
  @XmlElement(name = "username")
  public @Nullable String getUsername() {
    return username;
  }

  public void setUsername(@Nullable String username) {
    this.username = username;
  }

  public User firstName(@Nullable String firstName) {
    this.firstName = firstName;
    return this;
  }

  /**
   * Get firstName
   * @return firstName
   */
  
  @Schema(name = "firstName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("firstName")
  @JacksonXmlProperty(localName = "firstName")
  @XmlElement(name = "firstName")
  public @Nullable String getFirstName() {
    return firstName;
  }

  public void setFirstName(@Nullable String firstName) {
    this.firstName = firstName;
  }

  public User lastName(@Nullable String lastName) {
    this.lastName = lastName;
    return this;
  }

  /**
   * Get lastName
   * @return lastName
   */
  
  @Schema(name = "lastName", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("lastName")
  @JacksonXmlProperty(localName = "lastName")
  @XmlElement(name = "lastName")
  public @Nullable String getLastName() {
    return lastName;
  }

  public void setLastName(@Nullable String lastName) {
    this.lastName = lastName;
  }

  public User email(@Nullable String email) {
    this.email = email;
    return this;
  }

  /**
   * Get email
   * @return email
   */
  
  @Schema(name = "email", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("email")
  @JacksonXmlProperty(localName = "email")
  @XmlElement(name = "email")
  public @Nullable String getEmail() {
    return email;
  }

  public void setEmail(@Nullable String email) {
    this.email = email;
  }

  public User password(@Nullable String password) {
    this.password = password;
    return this;
  }

  /**
   * Get password
   * @return password
   */
  
  @Schema(name = "password", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("password")
  @JacksonXmlProperty(localName = "password")
  @XmlElement(name = "password")
  public @Nullable String getPassword() {
    return password;
  }

  public void setPassword(@Nullable String password) {
    this.password = password;
  }

  public User phone(@Nullable String phone) {
    this.phone = phone;
    return this;
  }

  /**
   * Get phone
   * @return phone
   */
  
  @Schema(name = "phone", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("phone")
  @JacksonXmlProperty(localName = "phone")
  @XmlElement(name = "phone")
  public @Nullable String getPhone() {
    return phone;
  }

  public void setPhone(@Nullable String phone) {
    this.phone = phone;
  }

  public User userStatus(@Nullable Integer userStatus) {
    this.userStatus = userStatus;
    return this;
  }

  /**
   * User Status
   * @return userStatus
   */
  
  @Schema(name = "userStatus", description = "User Status", requiredMode = Schema.RequiredMode.NOT_REQUIRED)
  @JsonProperty("userStatus")
  @JacksonXmlProperty(localName = "userStatus")
  @XmlElement(name = "userStatus")
  public @Nullable Integer getUserStatus() {
    return userStatus;
  }

  public void setUserStatus(@Nullable Integer userStatus) {
    this.userStatus = userStatus;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    User user = (User) o;
    return Objects.equals(this.id, user.id) &&
        Objects.equals(this.username, user.username) &&
        Objects.equals(this.firstName, user.firstName) &&
        Objects.equals(this.lastName, user.lastName) &&
        Objects.equals(this.email, user.email) &&
        Objects.equals(this.password, user.password) &&
        Objects.equals(this.phone, user.phone) &&
        Objects.equals(this.userStatus, user.userStatus);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, username, firstName, lastName, email, password, phone, userStatus);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class User {\n");
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    username: ").append(toIndentedString(username)).append("\n");
    sb.append("    firstName: ").append(toIndentedString(firstName)).append("\n");
    sb.append("    lastName: ").append(toIndentedString(lastName)).append("\n");
    sb.append("    email: ").append(toIndentedString(email)).append("\n");
    sb.append("    password: ").append(toIndentedString(password)).append("\n");
    sb.append("    phone: ").append(toIndentedString(phone)).append("\n");
    sb.append("    userStatus: ").append(toIndentedString(userStatus)).append("\n");
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
  
  public static class Builder {

    private User instance;

    public Builder() {
      this(new User());
    }

    protected Builder(User instance) {
      this.instance = instance;
    }

    protected Builder copyOf(User value) { 
      this.instance.setId(value.id);
      this.instance.setUsername(value.username);
      this.instance.setFirstName(value.firstName);
      this.instance.setLastName(value.lastName);
      this.instance.setEmail(value.email);
      this.instance.setPassword(value.password);
      this.instance.setPhone(value.phone);
      this.instance.setUserStatus(value.userStatus);
      return this;
    }

    public User.Builder id(Long id) {
      this.instance.id(id);
      return this;
    }
    
    public User.Builder username(String username) {
      this.instance.username(username);
      return this;
    }
    
    public User.Builder firstName(String firstName) {
      this.instance.firstName(firstName);
      return this;
    }
    
    public User.Builder lastName(String lastName) {
      this.instance.lastName(lastName);
      return this;
    }
    
    public User.Builder email(String email) {
      this.instance.email(email);
      return this;
    }
    
    public User.Builder password(String password) {
      this.instance.password(password);
      return this;
    }
    
    public User.Builder phone(String phone) {
      this.instance.phone(phone);
      return this;
    }
    
    public User.Builder userStatus(Integer userStatus) {
      this.instance.userStatus(userStatus);
      return this;
    }
    
    /**
    * returns a built User instance.
    *
    * The builder is not reusable (NullPointerException)
    */
    public User build() {
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
  public static User.Builder builder() {
    return new User.Builder();
  }

  /**
  * Create a builder with a shallow copy of this instance.
  */
  public User.Builder toBuilder() {
    User.Builder builder = new User.Builder();
    return builder.copyOf(this);
  }

}

