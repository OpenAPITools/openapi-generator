package org.openapitools.server.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class AdditionalPropertiesClass   {

    private Map<String, String> mapProperty = new HashMap<>();
    private Map<String, Map<String, String>> mapOfMapProperty = new HashMap<>();

    /**
     * Default constructor.
     */
    public AdditionalPropertiesClass() {
    // JSON-B / Jackson
    }

    /**
     * Create AdditionalPropertiesClass.
     *
     * @param mapProperty mapProperty
     * @param mapOfMapProperty mapOfMapProperty
     */
    public AdditionalPropertiesClass(
        Map<String, String> mapProperty, 
        Map<String, Map<String, String>> mapOfMapProperty
    ) {
        this.mapProperty = mapProperty;
        this.mapOfMapProperty = mapOfMapProperty;
    }



    /**
     * Get mapProperty
     * @return mapProperty
     */
    public Map<String, String> getMapProperty() {
        return mapProperty;
    }

    public void setMapProperty(Map<String, String> mapProperty) {
        this.mapProperty = mapProperty;
    }

    /**
     * Get mapOfMapProperty
     * @return mapOfMapProperty
     */
    public Map<String, Map<String, String>> getMapOfMapProperty() {
        return mapOfMapProperty;
    }

    public void setMapOfMapProperty(Map<String, Map<String, String>> mapOfMapProperty) {
        this.mapOfMapProperty = mapOfMapProperty;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class AdditionalPropertiesClass {\n");
        
        sb.append("    mapProperty: ").append(toIndentedString(mapProperty)).append("\n");
        sb.append("    mapOfMapProperty: ").append(toIndentedString(mapOfMapProperty)).append("\n");
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

