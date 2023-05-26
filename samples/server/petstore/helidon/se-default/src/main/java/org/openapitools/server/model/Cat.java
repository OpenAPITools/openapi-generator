package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import org.openapitools.server.model.Animal;
import org.openapitools.server.model.CatAllOf;



public class Cat extends Animal  {

    private Boolean declawed;

    /**
     * Default constructor.
     */
    public Cat() {
    // JSON-B / Jackson
    }

    /**
     * Create Cat.
     *
     * @param declawed declawed
     */
    public Cat(
        Boolean declawed
    ) {
        this.declawed = declawed;
    }



    /**
     * Get declawed
     * @return declawed
     */
    public Boolean getDeclawed() {
        return declawed;
    }

    public void setDeclawed(Boolean declawed) {
        this.declawed = declawed;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Cat {\n");
        sb.append("    ").append(toIndentedString(super.toString())).append("\n");
        sb.append("    declawed: ").append(toIndentedString(declawed)).append("\n");
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

