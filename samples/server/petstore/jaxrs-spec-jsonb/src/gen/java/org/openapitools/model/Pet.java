package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.openapitools.model.Category;
import org.openapitools.model.Tag;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import jakarta.json.bind.annotation.JsonbCreator;
import jakarta.json.bind.annotation.JsonbProperty;
import jakarta.json.bind.annotation.JsonbTypeDeserializer;
import jakarta.json.bind.annotation.JsonbTypeSerializer;
import jakarta.json.bind.serializer.DeserializationContext;
import jakarta.json.bind.serializer.JsonbDeserializer;
import jakarta.json.bind.serializer.JsonbSerializer;
import jakarta.json.bind.serializer.SerializationContext;
import jakarta.json.stream.JsonGenerator;
import jakarta.json.stream.JsonParser;



@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.26.0-SNAPSHOT")
public class Pet  implements Serializable {
  private Long id;
  private Category category;
  private String name;
  private Set<String> photoUrls = new LinkedHashSet<>();
  private List<@Valid Tag> tags = new ArrayList<>();
  @JsonbTypeSerializer(StatusEnum.Serializer.class)
  @JsonbTypeDeserializer(StatusEnum.Deserializer.class)
  public enum StatusEnum {

    AVAILABLE(String.valueOf("available")), PENDING(String.valueOf("pending")), SOLD(String.valueOf("sold"));


    private String value;

    StatusEnum (String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    /**
     * Convert a String into String, as specified in the
     * <a href="https://download.oracle.com/otndocs/jcp/jaxrs-2_0-fr-eval-spec/index.html">See JAX RS 2.0 Specification, section 3.2, p. 12</a>
     */
    public static StatusEnum fromString(String s) {
        for (StatusEnum b : StatusEnum.values()) {
            // using Objects.toString() to be safe if value type non-object type
            // because types like 'int' etc. will be auto-boxed
            if (java.util.Objects.toString(b.value).equals(s)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected string value '" + s + "'");
    }

    public static StatusEnum fromValue(String value) {
        for (StatusEnum b : StatusEnum.values()) {
            if (b.value.equals(value)) {
                return b;
            }
        }
        throw new IllegalArgumentException("Unexpected value '" + value + "'");
    }

    public static final class Serializer implements JsonbSerializer<StatusEnum> {
        @Override
        public void serialize(StatusEnum obj, JsonGenerator generator, SerializationContext ctx) {
            ctx.serialize(obj.value, generator);
        }
    }

    public static final class Deserializer implements JsonbDeserializer<StatusEnum> {
        @Override
        public StatusEnum deserialize(JsonParser parser, DeserializationContext ctx, java.lang.reflect.Type rtType) {
            if (parser.getValue().getValueType() == jakarta.json.JsonValue.ValueType.NULL) {
                return null;
            }
            return fromValue(ctx.deserialize(String.class, parser));
        }
    }
}

  private StatusEnum status;

  protected Pet(PetBuilder<?, ?> b) {
    this.id = b.id;
    this.category = b.category;
    this.name = b.name;
    this.photoUrls = b.photoUrls;
    this.tags = b.tags;
    this.status = b.status;
  }

  public Pet() {
  }

  @JsonbCreator
  public Pet(
    @JsonbProperty("name") String name,
    @JsonbProperty("photoUrls") Set<String> photoUrls
  ) {
    this.name = name;
    this.photoUrls = photoUrls;
  }

  /**
   **/
  public Pet id(Long id) {
    this.id = id;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("id")
  public Long getId() {
    return id;
  }

  @JsonbProperty("id")
  public void setId(Long id) {
    this.id = id;
  }

  /**
   **/
  public Pet category(Category category) {
    this.category = category;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("category")
  @Valid public Category getCategory() {
    return category;
  }

  @JsonbProperty("category")
  public void setCategory(Category category) {
    this.category = category;
  }

  /**
   **/
  public Pet name(String name) {
    this.name = name;
    return this;
  }

  
  @ApiModelProperty(example = "doggie", required = true, value = "")
  @JsonbProperty("name")
  @NotNull public String getName() {
    return name;
  }

  @JsonbProperty("name")
  public void setName(String name) {
    this.name = name;
  }

  /**
   **/
  public Pet photoUrls(Set<String> photoUrls) {
    this.photoUrls = photoUrls;
    return this;
  }

  
  @ApiModelProperty(required = true, value = "")
  @JsonbProperty("photoUrls")
  @NotNull public Set<String> getPhotoUrls() {
    return photoUrls;
  }

  @JsonbProperty("photoUrls")
  public void setPhotoUrls(Set<String> photoUrls) {
    this.photoUrls = photoUrls;
  }

  public Pet addPhotoUrlsItem(String photoUrlsItem) {
    if (this.photoUrls == null) {
      this.photoUrls = new LinkedHashSet<>();
    }

    this.photoUrls.add(photoUrlsItem);
    return this;
  }

  public Pet removePhotoUrlsItem(String photoUrlsItem) {
    if (photoUrlsItem != null && this.photoUrls != null) {
      this.photoUrls.remove(photoUrlsItem);
    }

    return this;
  }
  /**
   **/
  public Pet tags(List<@Valid Tag> tags) {
    this.tags = tags;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonbProperty("tags")
  public List<@Valid Tag> getTags() {
    return tags;
  }

  @JsonbProperty("tags")
  public void setTags(List<@Valid Tag> tags) {
    this.tags = tags;
  }

  public Pet addTagsItem(Tag tagsItem) {
    if (this.tags == null) {
      this.tags = new ArrayList<>();
    }

    this.tags.add(tagsItem);
    return this;
  }

  public Pet removeTagsItem(Tag tagsItem) {
    if (tagsItem != null && this.tags != null) {
      this.tags.remove(tagsItem);
    }

    return this;
  }
  /**
   * pet status in the store
   **/
  public Pet status(StatusEnum status) {
    this.status = status;
    return this;
  }

  
  @ApiModelProperty(value = "pet status in the store")
  @JsonbProperty("status")
  public StatusEnum getStatus() {
    return status;
  }

  @JsonbProperty("status")
  public void setStatus(StatusEnum status) {
    this.status = status;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Pet pet = (Pet) o;
    return Objects.equals(this.id, pet.id) &&
        Objects.equals(this.category, pet.category) &&
        Objects.equals(this.name, pet.name) &&
        Objects.equals(this.photoUrls, pet.photoUrls) &&
        Objects.equals(this.tags, pet.tags) &&
        Objects.equals(this.status, pet.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, category, name, photoUrls, tags, status);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Pet {\n");
    
    sb.append("    id: ").append(toIndentedString(id)).append("\n");
    sb.append("    category: ").append(toIndentedString(category)).append("\n");
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    photoUrls: ").append(toIndentedString(photoUrls)).append("\n");
    sb.append("    tags: ").append(toIndentedString(tags)).append("\n");
    sb.append("    status: ").append(toIndentedString(status)).append("\n");
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


  public static PetBuilder<?, ?> builder() {
    return new PetBuilderImpl();
  }

  private static final class PetBuilderImpl extends PetBuilder<Pet, PetBuilderImpl> {

    @Override
    protected PetBuilderImpl self() {
      return this;
    }

    @Override
    public Pet build() {
      return new Pet(this);
    }
  }

  public static abstract class PetBuilder<C extends Pet, B extends PetBuilder<C, B>>  {
    private Long id;
    private Category category;
    private String name;
    private Set<String> photoUrls = new LinkedHashSet<>();
    private List<Tag> tags = new ArrayList<>();
    private StatusEnum status;
    protected abstract B self();

    public abstract C build();

    public B id(Long id) {
      this.id = id;
      return self();
    }
    public B category(Category category) {
      this.category = category;
      return self();
    }
    public B name(String name) {
      this.name = name;
      return self();
    }
    public B photoUrls(Set<String> photoUrls) {
      this.photoUrls = photoUrls;
      return self();
    }
    public B tags(List<Tag> tags) {
      this.tags = tags;
      return self();
    }
    public B status(StatusEnum status) {
      this.status = status;
      return self();
    }
  }
}
