package org.openapitools.server.model;

import java.util.ArrayList;
import java.util.List;
import org.openapitools.server.model.ReadOnlyFirst;



public class ArrayTest   {

    private List<String> arrayOfString;
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
     * @param arrayArrayOfInteger arrayArrayOfInteger
     * @param arrayArrayOfModel arrayArrayOfModel
     */
    public ArrayTest(
        List<String> arrayOfString, 
        List<List<Long>> arrayArrayOfInteger, 
        List<List<ReadOnlyFirst>> arrayArrayOfModel
    ) {
        this.arrayOfString = arrayOfString;
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

