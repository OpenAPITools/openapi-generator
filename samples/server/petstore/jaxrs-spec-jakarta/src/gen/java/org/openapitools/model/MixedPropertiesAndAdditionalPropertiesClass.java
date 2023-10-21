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
  private @Valid UUID _uuid;
  private @Valid Date _dateTime;
  private @Valid Map<String, Animal> _map = new HashMap<>();

  protected MixedPropertiesAndAdditionalPropertiesClass(MixedPropertiesAndAdditionalPropertiesClassBuilder<?, ?> b) {
    this._uuid = b._uuid;
    this._dateTime = b._dateTime;
    this._map = b._map;
  }

  public MixedPropertiesAndAdditionalPropertiesClass() {
  }

  /**
   **/
  public MixedPropertiesAndAdditionalPropertiesClass _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("uuid")
  public UUID getUuid() {
    return _uuid;
  }

  @JsonProperty("uuid")
  public void setUuid(UUID _uuid) {
    this._uuid = _uuid;
  }

  /**
   **/
  public MixedPropertiesAndAdditionalPropertiesClass _dateTime(Date _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("dateTime")
  public Date getDateTime() {
    return _dateTime;
  }

  @JsonProperty("dateTime")
  public void setDateTime(Date _dateTime) {
    this._dateTime = _dateTime;
  }

  /**
   **/
  public MixedPropertiesAndAdditionalPropertiesClass _map(Map<String, Animal> _map) {
    this._map = _map;
    return this;
  }

  
  @ApiModelProperty(value = "")
  @JsonProperty("map")
  public Map<String, Animal> getMap() {
    return _map;
  }

  @JsonProperty("map")
  public void setMap(Map<String, Animal> _map) {
    this._map = _map;
  }

  public MixedPropertiesAndAdditionalPropertiesClass putMapItem(String key, Animal _mapItem) {
    if (this._map == null) {
      this._map = new HashMap<>();
    }

    this._map.put(key, _mapItem);
    return this;
  }

  public MixedPropertiesAndAdditionalPropertiesClass removeMapItem(Animal _mapItem) {
    if (_mapItem != null && this._map != null) {
      this._map.remove(_mapItem);
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
    return Objects.equals(this._uuid, mixedPropertiesAndAdditionalPropertiesClass._uuid) &&
        Objects.equals(this._dateTime, mixedPropertiesAndAdditionalPropertiesClass._dateTime) &&
        Objects.equals(this._map, mixedPropertiesAndAdditionalPropertiesClass._map);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_uuid, _dateTime, _map);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class MixedPropertiesAndAdditionalPropertiesClass {\n");
    
    sb.append("    _uuid: ").append(toIndentedString(_uuid)).append("\n");
    sb.append("    _dateTime: ").append(toIndentedString(_dateTime)).append("\n");
    sb.append("    _map: ").append(toIndentedString(_map)).append("\n");
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
    private UUID _uuid;
    private Date _dateTime;
    private Map<String, Animal> _map = new HashMap<>();
    protected abstract B self();

    public abstract C build();

    public B _uuid(UUID _uuid) {
      this._uuid = _uuid;
      return self();
    }
    public B _dateTime(Date _dateTime) {
      this._dateTime = _dateTime;
      return self();
    }
    public B _map(Map<String, Animal> _map) {
      this._map = _map;
      return self();
    }
  }
}

