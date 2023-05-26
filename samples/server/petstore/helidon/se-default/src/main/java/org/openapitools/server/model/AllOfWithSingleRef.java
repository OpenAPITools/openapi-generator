package org.openapitools.server.model;

import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.server.model.SingleRefType;



public class AllOfWithSingleRef   {

    private String username;
    private SingleRefType singleRefType;

    /**
     * Default constructor.
     */
    public AllOfWithSingleRef() {
    // JSON-B / Jackson
    }

    /**
     * Create AllOfWithSingleRef.
     *
     * @param username username
     * @param singleRefType singleRefType
     */
    public AllOfWithSingleRef(
        String username, 
        SingleRefType singleRefType
    ) {
        this.username = username;
        this.singleRefType = singleRefType;
    }



    /**
     * Get username
     * @return username
     */
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * Get singleRefType
     * @return singleRefType
     */
    public SingleRefType getSingleRefType() {
        return singleRefType;
    }

    public void setSingleRefType(SingleRefType singleRefType) {
        this.singleRefType = singleRefType;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class AllOfWithSingleRef {\n");
        
        sb.append("    username: ").append(toIndentedString(username)).append("\n");
        sb.append("    singleRefType: ").append(toIndentedString(singleRefType)).append("\n");
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

