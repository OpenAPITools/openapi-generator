package apimodels;

import apimodels.Animal;
import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * MixedPropertiesAndAdditionalPropertiesClass
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class MixedPropertiesAndAdditionalPropertiesClass   {
  @JsonProperty("uuid")
  @Valid

  private UUID _uuid;

  @JsonProperty("dateTime")
  @Valid

  private OffsetDateTime _dateTime;

  @JsonProperty("map")
  @Valid

  private Map<String, Animal> _map = null;

  public MixedPropertiesAndAdditionalPropertiesClass _uuid(UUID _uuid) {
    this._uuid = _uuid;
    return this;
  }

   /**
   * Get _uuid
   * @return _uuid
  **/
  public UUID getUuid() {
    return _uuid;
  }

  public void setUuid(UUID _uuid) {
    this._uuid = _uuid;
  }

  public MixedPropertiesAndAdditionalPropertiesClass _dateTime(OffsetDateTime _dateTime) {
    this._dateTime = _dateTime;
    return this;
  }

   /**
   * Get _dateTime
   * @return _dateTime
  **/
  public OffsetDateTime getDateTime() {
    return _dateTime;
  }

  public void setDateTime(OffsetDateTime _dateTime) {
    this._dateTime = _dateTime;
  }

  public MixedPropertiesAndAdditionalPropertiesClass _map(Map<String, Animal> _map) {
    this._map = _map;
    return this;
  }

  public MixedPropertiesAndAdditionalPropertiesClass putMapItem(String key, Animal _mapItem) {
    if (this._map == null) {
      this._map = new HashMap<>();
    }
    this._map.put(key, _mapItem);
    return this;
  }

   /**
   * Get _map
   * @return _map
  **/
  public Map<String, Animal> getMap() {
    return _map;
  }

  public void setMap(Map<String, Animal> _map) {
    this._map = _map;
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
    return Objects.equals(_uuid, mixedPropertiesAndAdditionalPropertiesClass._uuid) &&
        Objects.equals(_dateTime, mixedPropertiesAndAdditionalPropertiesClass._dateTime) &&
        Objects.equals(_map, mixedPropertiesAndAdditionalPropertiesClass._map);
  }

  @Override
  public int hashCode() {
    return Objects.hash(_uuid, _dateTime, _map);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
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
}

