package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;


/**
 * Model for testing reserved words
 */
public class ModelReturn   {

    private Integer _return;

    /**
     * Default constructor.
     */
    public ModelReturn() {
    // JSON-B / Jackson
    }

    /**
     * Create ModelReturn.
     *
     * @param _return _return
     */
    public ModelReturn(
        Integer _return
    ) {
        this._return = _return;
    }



    /**
     * Get _return
     * @return _return
     */
    public Integer getReturn() {
        return _return;
    }

    public void setReturn(Integer _return) {
        this._return = _return;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ModelReturn {\n");
        
        sb.append("    _return: ").append(toIndentedString(_return)).append("\n");
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

