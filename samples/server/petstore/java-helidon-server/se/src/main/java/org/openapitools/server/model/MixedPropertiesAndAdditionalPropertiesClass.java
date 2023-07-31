package org.openapitools.server.model;

import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.openapitools.server.model.Animal;



public class MixedPropertiesAndAdditionalPropertiesClass   {

    private UUID uuid;
    private OffsetDateTime dateTime;
    private Map<String, Animal> map = new HashMap<>();

    /**
     * Default constructor.
     */
    public MixedPropertiesAndAdditionalPropertiesClass() {
    // JSON-B / Jackson
    }

    /**
     * Create MixedPropertiesAndAdditionalPropertiesClass.
     *
     * @param uuid uuid
     * @param dateTime dateTime
     * @param map map
     */
    public MixedPropertiesAndAdditionalPropertiesClass(
        UUID uuid, 
        OffsetDateTime dateTime, 
        Map<String, Animal> map
    ) {
        this.uuid = uuid;
        this.dateTime = dateTime;
        this.map = map;
    }



    /**
     * Get uuid
     * @return uuid
     */
    public UUID getUuid() {
        return uuid;
    }

    public void setUuid(UUID uuid) {
        this.uuid = uuid;
    }

    /**
     * Get dateTime
     * @return dateTime
     */
    public OffsetDateTime getDateTime() {
        return dateTime;
    }

    public void setDateTime(OffsetDateTime dateTime) {
        this.dateTime = dateTime;
    }

    /**
     * Get map
     * @return map
     */
    public Map<String, Animal> getMap() {
        return map;
    }

    public void setMap(Map<String, Animal> map) {
        this.map = map;
    }

    /**
      * Create a string representation of this pojo.
    **/
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
    private static String toIndentedString(Object o) {
        if (o == null) {
          return "null";
        }
        return o.toString().replace("\n", "\n    ");
    }
}

