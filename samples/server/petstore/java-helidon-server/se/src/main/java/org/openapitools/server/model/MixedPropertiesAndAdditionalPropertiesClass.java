package org.openapitools.server.model;

import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.openapitools.server.model.Animal;



public class MixedPropertiesAndAdditionalPropertiesClass   {

    private UUID _uuid;
    private OffsetDateTime _dateTime;
    private Map<String, Animal> _map = new HashMap<>();

    /**
     * Default constructor.
     */
    public MixedPropertiesAndAdditionalPropertiesClass() {
    // JSON-B / Jackson
    }

    /**
     * Create MixedPropertiesAndAdditionalPropertiesClass.
     *
     * @param _uuid _uuid
     * @param _dateTime _dateTime
     * @param _map _map
     */
    public MixedPropertiesAndAdditionalPropertiesClass(
        UUID _uuid, 
        OffsetDateTime _dateTime, 
        Map<String, Animal> _map
    ) {
        this._uuid = _uuid;
        this._dateTime = _dateTime;
        this._map = _map;
    }



    /**
     * Get _uuid
     * @return _uuid
     */
    public UUID getUuid() {
        return _uuid;
    }

    public void setUuid(UUID _uuid) {
        this._uuid = _uuid;
    }

    /**
     * Get _dateTime
     * @return _dateTime
     */
    public OffsetDateTime getDateTime() {
        return _dateTime;
    }

    public void setDateTime(OffsetDateTime _dateTime) {
        this._dateTime = _dateTime;
    }

    /**
     * Get _map
     * @return _map
     */
    public Map<String, Animal> getMap() {
        return _map;
    }

    public void setMap(Map<String, Animal> _map) {
        this._map = _map;
    }

    /**
      * Create a string representation of this pojo.
    **/
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
    private static String toIndentedString(Object o) {
        if (o == null) {
          return "null";
        }
        return o.toString().replace("\n", "\n    ");
    }
}

