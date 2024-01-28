package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.util.HashMap;
import java.util.Map;



public class TestInlineFreeformAdditionalPropertiesRequest extends HashMap<String, Object>  {

    private String someProperty;

    /**
     * Default constructor.
     */
    public TestInlineFreeformAdditionalPropertiesRequest() {
    // JSON-B / Jackson
    }

    /**
     * Create TestInlineFreeformAdditionalPropertiesRequest.
     *
     * @param someProperty someProperty
     */
    public TestInlineFreeformAdditionalPropertiesRequest(
        String someProperty
    ) {
        this.someProperty = someProperty;
    }



    /**
     * Get someProperty
     * @return someProperty
     */
    public String getSomeProperty() {
        return someProperty;
    }

    public void setSomeProperty(String someProperty) {
        this.someProperty = someProperty;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class TestInlineFreeformAdditionalPropertiesRequest {\n");
        sb.append("    ").append(toIndentedString(super.toString())).append("\n");
        sb.append("    someProperty: ").append(toIndentedString(someProperty)).append("\n");
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

