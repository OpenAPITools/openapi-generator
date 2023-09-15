package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.server.model.ParentWithNullable;



public class ChildWithNullable extends ParentWithNullable  {

    private String otherProperty;

    /**
     * Default constructor.
     */
    public ChildWithNullable() {
    // JSON-B / Jackson
    }

    /**
     * Create ChildWithNullable.
     *
     * @param otherProperty otherProperty
     */
    public ChildWithNullable(
        String otherProperty
    ) {
        this.otherProperty = otherProperty;
    }



    /**
     * Get otherProperty
     * @return otherProperty
     */
    public String getOtherProperty() {
        return otherProperty;
    }

    public void setOtherProperty(String otherProperty) {
        this.otherProperty = otherProperty;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ChildWithNullable {\n");
        sb.append("    ").append(toIndentedString(super.toString())).append("\n");
        sb.append("    otherProperty: ").append(toIndentedString(otherProperty)).append("\n");
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

