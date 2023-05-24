package org.openapitools.server.model;

import org.openapitools.server.model.OuterEnumInteger;



public class OuterObjectWithEnumProperty   {

    private OuterEnumInteger value;

    /**
     * Default constructor.
     */
    public OuterObjectWithEnumProperty() {
    // JSON-B / Jackson
    }

    /**
     * Create OuterObjectWithEnumProperty.
     *
     * @param value value
     */
    public OuterObjectWithEnumProperty(
        OuterEnumInteger value
    ) {
        this.value = value;
    }



    /**
     * Get value
     * @return value
     */
    public OuterEnumInteger getValue() {
        return value;
    }

    public void setValue(OuterEnumInteger value) {
        this.value = value;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class OuterObjectWithEnumProperty {\n");
        
        sb.append("    value: ").append(toIndentedString(value)).append("\n");
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

