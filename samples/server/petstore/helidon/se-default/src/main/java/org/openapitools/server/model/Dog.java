package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.server.model.Animal;
import org.openapitools.server.model.DogAllOf;



public class Dog extends Animal  {

    private String breed;

    /**
     * Default constructor.
     */
    public Dog() {
    // JSON-B / Jackson
    }

    /**
     * Create Dog.
     *
     * @param breed breed
     */
    public Dog(
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
        sb.append("class Dog {\n");
        sb.append("    ").append(toIndentedString(super.toString())).append("\n");
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

