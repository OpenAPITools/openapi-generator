package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;



public class DogAllOf   {

    private String breed;

    /**
     * Default constructor.
     */
    public DogAllOf() {
    // JSON-B / Jackson
    }

    /**
     * Create DogAllOf.
     *
     * @param breed breed
     */
    public DogAllOf(
        String breed
    ) {
        this.breed = breed;
    }



    /**
     * Get breed
     * @return breed
     */
    public String getBreed() {
        return breed;
    }

    public void setBreed(String breed) {
        this.breed = breed;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class DogAllOf {\n");
        
        sb.append("    breed: ").append(toIndentedString(breed)).append("\n");
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

