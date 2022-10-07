package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;



public class CatAllOf   {

    private Boolean declawed;

    /**
     * Default constructor.
     */
    public CatAllOf() {
    // JSON-B / Jackson
    }

    /**
     * Create CatAllOf.
     *
     * @param declawed declawed
     */
    public CatAllOf(
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
        sb.append("class CatAllOf {\n");
        
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

