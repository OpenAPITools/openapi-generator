package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;



public class Animal   {

    private String className;
    private String color = "red";

    /**
     * Default constructor.
     */
    public Animal() {
    // JSON-B / Jackson
    }

    /**
     * Create Animal.
     *
     * @param className className
     * @param color color
     */
    public Animal(
        String className, 
        String color
    ) {
        this.className = className;
        this.color = color;
    }



    /**
     * Get className
     * @return className
     */
    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    /**
     * Get color
     * @return color
     */
    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Animal {\n");
        
        sb.append("    className: ").append(toIndentedString(className)).append("\n");
        sb.append("    color: ").append(toIndentedString(color)).append("\n");
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

