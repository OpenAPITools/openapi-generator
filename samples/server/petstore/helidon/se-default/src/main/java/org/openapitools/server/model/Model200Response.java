package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;


/**
 * Model for testing model name starting with number
 */
public class Model200Response   {

    private Integer name;
    private String propertyClass;

    /**
     * Default constructor.
     */
    public Model200Response() {
    // JSON-B / Jackson
    }

    /**
     * Create Model200Response.
     *
     * @param name name
     * @param propertyClass propertyClass
     */
    public Model200Response(
        Integer name, 
        String propertyClass
    ) {
        this.name = name;
        this.propertyClass = propertyClass;
    }



    /**
     * Get name
     * @return name
     */
    public Integer getName() {
        return name;
    }

    public void setName(Integer name) {
        this.name = name;
    }

    /**
     * Get propertyClass
     * @return propertyClass
     */
    public String getPropertyClass() {
        return propertyClass;
    }

    public void setPropertyClass(String propertyClass) {
        this.propertyClass = propertyClass;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Model200Response {\n");
        
        sb.append("    name: ").append(toIndentedString(name)).append("\n");
        sb.append("    propertyClass: ").append(toIndentedString(propertyClass)).append("\n");
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

