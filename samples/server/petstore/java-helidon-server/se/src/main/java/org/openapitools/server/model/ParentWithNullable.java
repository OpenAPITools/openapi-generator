package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.jackson.nullable.JsonNullable;



public class ParentWithNullable   {


    /**
    * Gets or Sets type
    */
    public enum TypeEnum {
        CHILDWITHNULLABLE("ChildWithNullable");

        private String value;

        TypeEnum(String value) {
            this.value = value;
        }

        @JsonValue
        public String getValue() {
            return value;
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }


        @JsonCreator
        public static TypeEnum fromValue(String text) {
            for (TypeEnum b : TypeEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private TypeEnum type;
    private String nullableProperty;

    /**
     * Default constructor.
     */
    public ParentWithNullable() {
    // JSON-B / Jackson
    }

    /**
     * Create ParentWithNullable.
     *
     * @param type type
     * @param nullableProperty nullableProperty
     */
    public ParentWithNullable(
        TypeEnum type, 
        String nullableProperty
    ) {
        this.type = type;
        this.nullableProperty = nullableProperty;
    }



    /**
     * Get type
     * @return type
     */
    public TypeEnum getType() {
        return type;
    }

    public void setType(TypeEnum type) {
        this.type = type;
    }

    /**
     * Get nullableProperty
     * @return nullableProperty
     */
    public String getNullableProperty() {
        return nullableProperty;
    }

    public void setNullableProperty(String nullableProperty) {
        this.nullableProperty = nullableProperty;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ParentWithNullable {\n");
        
        sb.append("    type: ").append(toIndentedString(type)).append("\n");
        sb.append("    nullableProperty: ").append(toIndentedString(nullableProperty)).append("\n");
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

