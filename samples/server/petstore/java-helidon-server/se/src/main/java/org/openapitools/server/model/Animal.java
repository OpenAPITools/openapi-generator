package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.server.model.Cat;
import org.openapitools.server.model.Dog;



public class Animal   {

    private String species;
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
     * @param species species
     * @param color color
     */
    public Animal(
        String species, 
        String color
    ) {
        this.species = species;
        this.color = color;
    }



    /**
     * Get species
     * @return species
     */
    public String getSpecies() {
        return species;
    }

    public void setSpecies(String species) {
        this.species = species;
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
        
        sb.append("    species: ").append(toIndentedString(species)).append("\n");
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

