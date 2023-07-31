package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import org.openapitools.server.model.Foo;



public class FooGetDefaultResponse   {

    private Foo string;

    /**
     * Default constructor.
     */
    public FooGetDefaultResponse() {
    // JSON-B / Jackson
    }

    /**
     * Create FooGetDefaultResponse.
     *
     * @param string string
     */
    public FooGetDefaultResponse(
        Foo string
    ) {
        this.string = string;
    }



    /**
     * Get string
     * @return string
     */
    public Foo getString() {
        return string;
    }

    public void setString(Foo string) {
        this.string = string;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class FooGetDefaultResponse {\n");
        
        sb.append("    string: ").append(toIndentedString(string)).append("\n");
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

