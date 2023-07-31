package org.openapitools.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.openapitools.model.Animal;
import java.io.Serializable;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

import io.swagger.annotations.*;
import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@JsonTypeName("MixedPropertiesAndAdditionalPropertiesClass")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class MixedPropertiesAndAdditionalPropertiesClass  implements Serializable {
  private @Valid UUID uuid;
  private @Valid Date dateTime;
  private @Valid Map<String, Animal> map = new HashMap<>();

  protected MixedPropertiesAndAdditionalPropertiesClass(MixedPropertiesAndAdditionalPropertiesClassBuilder<?, ?> b) {
    this.uuid = b.uuid;
    this.dateTime = b.dateTime;
    this.map = b.map;
  }

  public MixedPropertiesAndAdditionalPropertiesClass() {
  }

  /**
   **/
  public MixedPropertiesAndAdditionalPropertiesClass uuid(UUID uuid) {
    this.uuid = uuid;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("uuid")
  public UUID getUuid() {
    return uuid;
  }

  @JsonProperty("uuid")
  public void setUuid(UUID uuid) {
    this.uuid = uuid;
  }

  /**
   **/
  public MixedPropertiesAndAdditionalPropertiesClass dateTime(Date dateTime) {
    this.dateTime = dateTime;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return dateTime;
  }

  @JsonProperty("dateTime")
  public void setDateTime(Date dateTime) {
    this.dateTime = dateTime;
  }

  /**
   **/
  public MixedPropertiesAndAdditionalPropertiesClass map(Map<String, Animal> map) {
    this.map = map;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map")
  public Map<String, Animal> getMap() {
    return map;
  }

  @JsonProperty("map")
  public void setMap(Map<String, Animal> map) {
    this.map = map;
  }

  public MixedPropertiesAndAdditionalPropertiesClass putMapItem(String key, Animal mapItem) {
    if (this.map == null) {
      this.map = new HashMap<>();
    }

    this.map.put(key, mapItem);
    return this;
  }

  public MixedPropertiesAndAdditionalPropertiesClass removeMapItem(Animal mapItem) {
    if (mapItem != null && this.map != null) {
      this.map.remove(mapItem);
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
    MixedPropertiesAndAdditionalPropertiesClass mixedPropertiesAndAdditionalPropertiesClass = (MixedPropertiesAndAdditionalPropertiesClass) o;
    return Objects.equals(this.uuid, mixedPropertiesAndAdditionalPropertiesClass.uuid) &&
        Objects.equals(this.dateTime, mixedPropertiesAndAdditionalPropertiesClass.dateTime) &&
        Objects.equals(this.map, mixedPropertiesAndAdditionalPropertiesClass.map);
  }

  @Override
  public int hashCode() {
    return Objects.hash(uuid, dateTime, map);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MixedPropertiesAndAdditionalPropertiesClass {\n");
    
    sb.append("    uuid: ").append(toIndentedString(uuid)).append("\n");
    sb.append("    dateTime: ").append(toIndentedString(dateTime)).append("\n");
    sb.append("    map: ").append(toIndentedString(map)).append("\n");
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


  public static MixedPropertiesAndAdditionalPropertiesClassBuilder<?, ?> builder() {
    return new MixedPropertiesAndAdditionalPropertiesClassBuilderImpl();
  }

  private static final class MixedPropertiesAndAdditionalPropertiesClassBuilderImpl extends MixedPropertiesAndAdditionalPropertiesClassBuilder<MixedPropertiesAndAdditionalPropertiesClass, MixedPropertiesAndAdditionalPropertiesClassBuilderImpl> {

    @Override
    protected MixedPropertiesAndAdditionalPropertiesClassBuilderImpl self() {
      return this;
    }

    @Override
    public MixedPropertiesAndAdditionalPropertiesClass build() {
      return new MixedPropertiesAndAdditionalPropertiesClass(this);
    }
  }

  public static abstract class MixedPropertiesAndAdditionalPropertiesClassBuilder<C extends MixedPropertiesAndAdditionalPropertiesClass, B extends MixedPropertiesAndAdditionalPropertiesClassBuilder<C, B>>  {
    private UUID uuid;
    private Date dateTime;
    private Map<String, Animal> map = new HashMap<>();
    protected abstract B self();

    public abstract C build();

    public B uuid(UUID uuid) {
      this.uuid = uuid;
      return self();
    }
    public B dateTime(Date dateTime) {
      this.dateTime = dateTime;
      return self();
    }
    public B map(Map<String, Animal> map) {
      this.map = map;
      return self();
    }
  }
}

