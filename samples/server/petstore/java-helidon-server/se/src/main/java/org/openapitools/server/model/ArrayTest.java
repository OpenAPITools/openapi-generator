package org.openapitools.server.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.openapitools.server.model.ReadOnlyFirst;



public class ArrayTest   {

    private List<String> arrayOfString;

    /**
    * Gets or Sets arrayWithUniqueItems
    */
    public enum ArrayWithUniqueItemsEnum {
        _1("unique_item_1"),
        _2("unique_item_2"),
        _3("unique_item_3");

        private String value;

        ArrayWithUniqueItemsEnum(String value) {
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
        public static ArrayWithUniqueItemsEnum fromValue(String text) {
            for (ArrayWithUniqueItemsEnum b : ArrayWithUniqueItemsEnum.values()) {
                if (String.valueOf(b.value).equals(text)) {
                    return b;
                }
            }
            throw new IllegalArgumentException("Unexpected value '" + text + "'");
        }
    }


    private Set<ArrayWithUniqueItemsEnum> arrayWithUniqueItems;
    private List<List<Long>> arrayArrayOfInteger;
    private List<List<ReadOnlyFirst>> arrayArrayOfModel;

    /**
     * Default constructor.
     */
    public ArrayTest() {
    // JSON-B / Jackson
    }

    /**
     * Create ArrayTest.
     *
     * @param arrayOfString arrayOfString
     * @param arrayWithUniqueItems arrayWithUniqueItems
     * @param arrayArrayOfInteger arrayArrayOfInteger
     * @param arrayArrayOfModel arrayArrayOfModel
     */
    public ArrayTest(
        List<String> arrayOfString, 
        Set<ArrayWithUniqueItemsEnum> arrayWithUniqueItems, 
        List<List<Long>> arrayArrayOfInteger, 
        List<List<ReadOnlyFirst>> arrayArrayOfModel
    ) {
        this.arrayOfString = arrayOfString;
        this.arrayWithUniqueItems = arrayWithUniqueItems;
        this.arrayArrayOfInteger = arrayArrayOfInteger;
        this.arrayArrayOfModel = arrayArrayOfModel;
    }



    /**
     * Get arrayOfString
     * @return arrayOfString
     */
    public List<String> getArrayOfString() {
        return arrayOfString;
    }

    public void setArrayOfString(List<String> arrayOfString) {
        this.arrayOfString = arrayOfString;
    }

    /**
     * Get arrayWithUniqueItems
     * @return arrayWithUniqueItems
     */
    public Set<ArrayWithUniqueItemsEnum> getArrayWithUniqueItems() {
        return arrayWithUniqueItems;
    }

    public void setArrayWithUniqueItems(Set<ArrayWithUniqueItemsEnum> arrayWithUniqueItems) {
        this.arrayWithUniqueItems = arrayWithUniqueItems;
    }

    /**
     * Get arrayArrayOfInteger
     * @return arrayArrayOfInteger
     */
    public List<List<Long>> getArrayArrayOfInteger() {
        return arrayArrayOfInteger;
    }

    public void setArrayArrayOfInteger(List<List<Long>> arrayArrayOfInteger) {
        this.arrayArrayOfInteger = arrayArrayOfInteger;
    }

    /**
     * Get arrayArrayOfModel
     * @return arrayArrayOfModel
     */
    public List<List<ReadOnlyFirst>> getArrayArrayOfModel() {
        return arrayArrayOfModel;
    }

    public void setArrayArrayOfModel(List<List<ReadOnlyFirst>> arrayArrayOfModel) {
        this.arrayArrayOfModel = arrayArrayOfModel;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ArrayTest {\n");
        
        sb.append("    arrayOfString: ").append(toIndentedString(arrayOfString)).append("\n");
        sb.append("    arrayWithUniqueItems: ").append(toIndentedString(arrayWithUniqueItems)).append("\n");
        sb.append("    arrayArrayOfInteger: ").append(toIndentedString(arrayArrayOfInteger)).append("\n");
        sb.append("    arrayArrayOfModel: ").append(toIndentedString(arrayArrayOfModel)).append("\n");
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

