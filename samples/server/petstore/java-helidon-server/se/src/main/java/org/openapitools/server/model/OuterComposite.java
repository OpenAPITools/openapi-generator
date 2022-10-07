package org.openapitools.server.model;

import java.math.BigDecimal;



public class OuterComposite   {

    private BigDecimal myNumber;
    private String myString;
    private Boolean myBoolean;

    /**
     * Default constructor.
     */
    public OuterComposite() {
    // JSON-B / Jackson
    }

    /**
     * Create OuterComposite.
     *
     * @param myNumber myNumber
     * @param myString myString
     * @param myBoolean myBoolean
     */
    public OuterComposite(
        BigDecimal myNumber, 
        String myString, 
        Boolean myBoolean
    ) {
        this.myNumber = myNumber;
        this.myString = myString;
        this.myBoolean = myBoolean;
    }



    /**
     * Get myNumber
     * @return myNumber
     */
    public BigDecimal getMyNumber() {
        return myNumber;
    }

    public void setMyNumber(BigDecimal myNumber) {
        this.myNumber = myNumber;
    }

    /**
     * Get myString
     * @return myString
     */
    public String getMyString() {
        return myString;
    }

    public void setMyString(String myString) {
        this.myString = myString;
    }

    /**
     * Get myBoolean
     * @return myBoolean
     */
    public Boolean getMyBoolean() {
        return myBoolean;
    }

    public void setMyBoolean(Boolean myBoolean) {
        this.myBoolean = myBoolean;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class OuterComposite {\n");
        
        sb.append("    myNumber: ").append(toIndentedString(myNumber)).append("\n");
        sb.append("    myString: ").append(toIndentedString(myString)).append("\n");
        sb.append("    myBoolean: ").append(toIndentedString(myBoolean)).append("\n");
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

