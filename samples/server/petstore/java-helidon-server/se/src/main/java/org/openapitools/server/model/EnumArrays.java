package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;



public class EnumArrays   {


    /**
    * Gets or Sets justSymbol
    */
    public enum JustSymbolEnum {
        GREATER_THAN_OR_EQUAL_TO(">="),
        DOLLAR("$");

        private String value;

        JustSymbolEnum(String value) {
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
        public static JustSymbolEnum fromValue(String text) {
            for (JustSymbolEnum b : JustSymbolEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private JustSymbolEnum justSymbol;

    /**
    * Gets or Sets arrayEnum
    */
    public enum ArrayEnumEnum {
        FISH("fish"),
        CRAB("crab");

        private String value;

        ArrayEnumEnum(String value) {
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
        public static ArrayEnumEnum fromValue(String text) {
            for (ArrayEnumEnum b : ArrayEnumEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private List<ArrayEnumEnum> arrayEnum;

    /**
     * Default constructor.
     */
    public EnumArrays() {
    // JSON-B / Jackson
    }

    /**
     * Create EnumArrays.
     *
     * @param justSymbol justSymbol
     * @param arrayEnum arrayEnum
     */
    public EnumArrays(
        JustSymbolEnum justSymbol, 
        List<ArrayEnumEnum> arrayEnum
    ) {
        this.justSymbol = justSymbol;
        this.arrayEnum = arrayEnum;
    }



    /**
     * Get justSymbol
     * @return justSymbol
     */
    public JustSymbolEnum getJustSymbol() {
        return justSymbol;
    }

    public void setJustSymbol(JustSymbolEnum justSymbol) {
        this.justSymbol = justSymbol;
    }

    /**
     * Get arrayEnum
     * @return arrayEnum
     */
    public List<ArrayEnumEnum> getArrayEnum() {
        return arrayEnum;
    }

    public void setArrayEnum(List<ArrayEnumEnum> arrayEnum) {
        this.arrayEnum = arrayEnum;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class EnumArrays {\n");
        
        sb.append("    justSymbol: ").append(toIndentedString(justSymbol)).append("\n");
        sb.append("    arrayEnum: ").append(toIndentedString(arrayEnum)).append("\n");
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

