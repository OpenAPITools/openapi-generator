package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;



public class ModelApiResponse   {

    private Integer code;
    private String type;
    private String message;

    /**
     * Default constructor.
     */
    public ModelApiResponse() {
    // JSON-B / Jackson
    }

    /**
     * Create ModelApiResponse.
     *
     * @param code code
     * @param type type
     * @param message message
     */
    public ModelApiResponse(
        Integer code, 
        String type, 
        String message
    ) {
        this.code = code;
        this.type = type;
        this.message = message;
    }



    /**
     * Get code
     * @return code
     */
    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    /**
     * Get type
     * @return type
     */
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    /**
     * Get message
     * @return message
     */
    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ModelApiResponse {\n");
        
        sb.append("    code: ").append(toIndentedString(code)).append("\n");
        sb.append("    type: ").append(toIndentedString(type)).append("\n");
        sb.append("    message: ").append(toIndentedString(message)).append("\n");
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

