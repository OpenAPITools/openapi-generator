package org.openapitools.model;

import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("Category")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class Category  implements Serializable {
  private @Valid Long id;
  private @Valid String name = "default-name";

  protected Category(CategoryBuilder<?, ?> b) {
    this.id = b.id;
    this.name = b.name;
  }

  public Category() {
  }

  /**
   **/
  public Category id(Long id) {
    this.id = id;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("id")
  public Long getId() {
    return id;
  }

  @JsonProperty("id")
  public void setId(Long id) {
    this.id = id;
  }

  /**
   **/
  public Category name(String name) {
    this.name = name;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(required = true, description = "")
  @JsonProperty("name")
  @NotNull
  public String getName() {
    return name;
  }

  @JsonProperty("name")
  public void setName(String name) {
    this.name = name;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Category category = (Category) o;
    return Objects.equals(this.id, category.id) &&
        Objects.equals(this.name, category.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, name);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Category {\n");
    
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
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


  public static CategoryBuilder<?, ?> builder() {
    return new CategoryBuilderImpl();
  }

  private static final class CategoryBuilderImpl extends CategoryBuilder<Category, CategoryBuilderImpl> {

    @Override
    protected CategoryBuilderImpl self() {
      return this;
    }

    @Override
    public Category build() {
      return new Category(this);
    }
  }

  public static abstract class CategoryBuilder<C extends Category, B extends CategoryBuilder<C, B>>  {
    private Long id;
    private String name = "default-name";
    protected abstract B self();

    public abstract C build();

    public B id(Long id) {
      this.id = id;
      return self();
    }
    public B name(String name) {
      this.name = name;
      return self();
    }
  }
}

