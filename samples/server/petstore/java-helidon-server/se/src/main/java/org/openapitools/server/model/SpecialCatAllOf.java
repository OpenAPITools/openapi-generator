package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.JsonValue;



public class SpecialCatAllOf   {


    /**
    * Gets or Sets kind
    */
    public enum KindEnum {
        LIONS("lions"),
        TIGERS("tigers"),
        LEOPARDS("leopards"),
        JAGUARS("jaguars");

        private String value;

        KindEnum(String value) {
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
        public static KindEnum fromValue(String text) {
            for (KindEnum b : KindEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private KindEnum kind;

    /**
     * Default constructor.
     */
    public SpecialCatAllOf() {
    // JSON-B / Jackson
    }

    /**
     * Create SpecialCatAllOf.
     *
     * @param kind kind
     */
    public SpecialCatAllOf(
        KindEnum kind
    ) {
        this.kind = kind;
    }



    /**
     * Get kind
     * @return kind
     */
    public KindEnum getKind() {
        return kind;
    }

    public void setKind(KindEnum kind) {
        this.kind = kind;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class SpecialCatAllOf {\n");
        
        sb.append("    kind: ").append(toIndentedString(kind)).append("\n");
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

