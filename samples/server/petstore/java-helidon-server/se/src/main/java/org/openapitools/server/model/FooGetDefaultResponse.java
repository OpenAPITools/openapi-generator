package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.server.model.Foo;



public class FooGetDefaultResponse   {

    private Foo _string;

    /**
     * Default constructor.
     */
    public FooGetDefaultResponse() {
    // JSON-B / Jackson
    }

    /**
     * Create FooGetDefaultResponse.
     *
     * @param _string _string
     */
    public FooGetDefaultResponse(
        Foo _string
    ) {
        this._string = _string;
    }



    /**
     * Get _string
     * @return _string
     */
    public Foo getString() {
        return _string;
    }

    public void setString(Foo _string) {
        this._string = _string;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class FooGetDefaultResponse {\n");
        
        sb.append("    _string: ").append(toIndentedString(_string)).append("\n");
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

