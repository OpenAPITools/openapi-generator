package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.JsonValue;
import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.server.model.OuterEnum;
import org.openapitools.server.model.OuterEnumDefaultValue;
import org.openapitools.server.model.OuterEnumInteger;
import org.openapitools.server.model.OuterEnumIntegerDefaultValue;



public class EnumTest   {


    /**
    * Gets or Sets enumString
    */
    public enum EnumStringEnum {
        UPPER("UPPER"),
        LOWER("lower"),
        EMPTY("");

        private String value;

        EnumStringEnum(String value) {
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
        public static EnumStringEnum fromValue(String text) {
            for (EnumStringEnum b : EnumStringEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private EnumStringEnum enumString;

    /**
    * Gets or Sets enumStringRequired
    */
    public enum EnumStringRequiredEnum {
        UPPER("UPPER"),
        LOWER("lower"),
        EMPTY("");

        private String value;

        EnumStringRequiredEnum(String value) {
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
        public static EnumStringRequiredEnum fromValue(String text) {
            for (EnumStringRequiredEnum b : EnumStringRequiredEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private EnumStringRequiredEnum enumStringRequired;

    /**
    * Gets or Sets enumInteger
    */
    public enum EnumIntegerEnum {
        NUMBER_1(1),
        NUMBER_MINUS_1(-1);

        private Integer value;

        EnumIntegerEnum(Integer value) {
            this.value = value;
        }

        @JsonValue
        public Integer getValue() {
            return value;
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }


        @JsonCreator
        public static EnumIntegerEnum fromValue(String text) {
            for (EnumIntegerEnum b : EnumIntegerEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private EnumIntegerEnum enumInteger;

    /**
    * Gets or Sets enumNumber
    */
    public enum EnumNumberEnum {
        NUMBER_1_DOT_1(1.1),
        NUMBER_MINUS_1_DOT_2(-1.2);

        private Double value;

        EnumNumberEnum(Double value) {
            this.value = value;
        }

        @JsonValue
        public Double getValue() {
            return value;
        }

        @Override
        public String toString() {
            return String.valueOf(value);
        }


        @JsonCreator
        public static EnumNumberEnum fromValue(String text) {
            for (EnumNumberEnum b : EnumNumberEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private EnumNumberEnum enumNumber;
    private OuterEnum outerEnum;
    private OuterEnumInteger outerEnumInteger;
    private OuterEnumDefaultValue outerEnumDefaultValue = OuterEnumDefaultValue.PLACED;
    private OuterEnumIntegerDefaultValue outerEnumIntegerDefaultValue = OuterEnumIntegerDefaultValue.NUMBER_0;

    /**
     * Default constructor.
     */
    public EnumTest() {
    // JSON-B / Jackson
    }

    /**
     * Create EnumTest.
     *
     * @param enumString enumString
     * @param enumStringRequired enumStringRequired
     * @param enumInteger enumInteger
     * @param enumNumber enumNumber
     * @param outerEnum outerEnum
     * @param outerEnumInteger outerEnumInteger
     * @param outerEnumDefaultValue outerEnumDefaultValue
     * @param outerEnumIntegerDefaultValue outerEnumIntegerDefaultValue
     */
    public EnumTest(
        EnumStringEnum enumString, 
        EnumStringRequiredEnum enumStringRequired, 
        EnumIntegerEnum enumInteger, 
        EnumNumberEnum enumNumber, 
        OuterEnum outerEnum, 
        OuterEnumInteger outerEnumInteger, 
        OuterEnumDefaultValue outerEnumDefaultValue, 
        OuterEnumIntegerDefaultValue outerEnumIntegerDefaultValue
    ) {
        this.enumString = enumString;
        this.enumStringRequired = enumStringRequired;
        this.enumInteger = enumInteger;
        this.enumNumber = enumNumber;
        this.outerEnum = outerEnum;
        this.outerEnumInteger = outerEnumInteger;
        this.outerEnumDefaultValue = outerEnumDefaultValue;
        this.outerEnumIntegerDefaultValue = outerEnumIntegerDefaultValue;
    }



    /**
     * Get enumString
     * @return enumString
     */
    public EnumStringEnum getEnumString() {
        return enumString;
    }

    public void setEnumString(EnumStringEnum enumString) {
        this.enumString = enumString;
    }

    /**
     * Get enumStringRequired
     * @return enumStringRequired
     */
    public EnumStringRequiredEnum getEnumStringRequired() {
        return enumStringRequired;
    }

    public void setEnumStringRequired(EnumStringRequiredEnum enumStringRequired) {
        this.enumStringRequired = enumStringRequired;
    }

    /**
     * Get enumInteger
     * @return enumInteger
     */
    public EnumIntegerEnum getEnumInteger() {
        return enumInteger;
    }

    public void setEnumInteger(EnumIntegerEnum enumInteger) {
        this.enumInteger = enumInteger;
    }

    /**
     * Get enumNumber
     * @return enumNumber
     */
    public EnumNumberEnum getEnumNumber() {
        return enumNumber;
    }

    public void setEnumNumber(EnumNumberEnum enumNumber) {
        this.enumNumber = enumNumber;
    }

    /**
     * Get outerEnum
     * @return outerEnum
     */
    public OuterEnum getOuterEnum() {
        return outerEnum;
    }

    public void setOuterEnum(OuterEnum outerEnum) {
        this.outerEnum = outerEnum;
    }

    /**
     * Get outerEnumInteger
     * @return outerEnumInteger
     */
    public OuterEnumInteger getOuterEnumInteger() {
        return outerEnumInteger;
    }

    public void setOuterEnumInteger(OuterEnumInteger outerEnumInteger) {
        this.outerEnumInteger = outerEnumInteger;
    }

    /**
     * Get outerEnumDefaultValue
     * @return outerEnumDefaultValue
     */
    public OuterEnumDefaultValue getOuterEnumDefaultValue() {
        return outerEnumDefaultValue;
    }

    public void setOuterEnumDefaultValue(OuterEnumDefaultValue outerEnumDefaultValue) {
        this.outerEnumDefaultValue = outerEnumDefaultValue;
    }

    /**
     * Get outerEnumIntegerDefaultValue
     * @return outerEnumIntegerDefaultValue
     */
    public OuterEnumIntegerDefaultValue getOuterEnumIntegerDefaultValue() {
        return outerEnumIntegerDefaultValue;
    }

    public void setOuterEnumIntegerDefaultValue(OuterEnumIntegerDefaultValue outerEnumIntegerDefaultValue) {
        this.outerEnumIntegerDefaultValue = outerEnumIntegerDefaultValue;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class EnumTest {\n");
        
        sb.append("    enumString: ").append(toIndentedString(enumString)).append("\n");
        sb.append("    enumStringRequired: ").append(toIndentedString(enumStringRequired)).append("\n");
        sb.append("    enumInteger: ").append(toIndentedString(enumInteger)).append("\n");
        sb.append("    enumNumber: ").append(toIndentedString(enumNumber)).append("\n");
        sb.append("    outerEnum: ").append(toIndentedString(outerEnum)).append("\n");
        sb.append("    outerEnumInteger: ").append(toIndentedString(outerEnumInteger)).append("\n");
        sb.append("    outerEnumDefaultValue: ").append(toIndentedString(outerEnumDefaultValue)).append("\n");
        sb.append("    outerEnumIntegerDefaultValue: ").append(toIndentedString(outerEnumIntegerDefaultValue)).append("\n");
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

