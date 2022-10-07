package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;



public class ModelList   {

    private String _123list;

    /**
     * Default constructor.
     */
    public ModelList() {
    // JSON-B / Jackson
    }

    /**
     * Create ModelList.
     *
     * @param _123list _123list
     */
    public ModelList(
        String _123list
    ) {
        this._123list = _123list;
    }



    /**
     * Get _123list
     * @return _123list
     */
    public String get123list() {
        return _123list;
    }

    public void set123list(String _123list) {
        this._123list = _123list;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ModelList {\n");
        
        sb.append("    _123list: ").append(toIndentedString(_123list)).append("\n");
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

