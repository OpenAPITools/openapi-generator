package org.openapitools.server.model;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.openapitools.jackson.nullable.JsonNullable;



public class NullableClass extends HashMap<String, Object>  {

    private Integer integerProp;
    private BigDecimal numberProp;
    private Boolean booleanProp;
    private String stringProp;
    private LocalDate dateProp;
    private OffsetDateTime datetimeProp;
    private List<Object> arrayNullableProp;
    private List<Object> arrayAndItemsNullableProp;
    private List<Object> arrayItemsNullable;
    private Map<String, Object> objectNullableProp;
    private Map<String, Object> objectAndItemsNullableProp;
    private Map<String, Object> objectItemsNullable = new HashMap<>();

    /**
     * Default constructor.
     */
    public NullableClass() {
    // JSON-B / Jackson
    }

    /**
     * Create NullableClass.
     *
     * @param integerProp integerProp
     * @param numberProp numberProp
     * @param booleanProp booleanProp
     * @param stringProp stringProp
     * @param dateProp dateProp
     * @param datetimeProp datetimeProp
     * @param arrayNullableProp arrayNullableProp
     * @param arrayAndItemsNullableProp arrayAndItemsNullableProp
     * @param arrayItemsNullable arrayItemsNullable
     * @param objectNullableProp objectNullableProp
     * @param objectAndItemsNullableProp objectAndItemsNullableProp
     * @param objectItemsNullable objectItemsNullable
     */
    public NullableClass(
        Integer integerProp, 
        BigDecimal numberProp, 
        Boolean booleanProp, 
        String stringProp, 
        LocalDate dateProp, 
        OffsetDateTime datetimeProp, 
        List<Object> arrayNullableProp, 
        List<Object> arrayAndItemsNullableProp, 
        List<Object> arrayItemsNullable, 
        Map<String, Object> objectNullableProp, 
        Map<String, Object> objectAndItemsNullableProp, 
        Map<String, Object> objectItemsNullable
    ) {
        this.integerProp = integerProp;
        this.numberProp = numberProp;
        this.booleanProp = booleanProp;
        this.stringProp = stringProp;
        this.dateProp = dateProp;
        this.datetimeProp = datetimeProp;
        this.arrayNullableProp = arrayNullableProp;
        this.arrayAndItemsNullableProp = arrayAndItemsNullableProp;
        this.arrayItemsNullable = arrayItemsNullable;
        this.objectNullableProp = objectNullableProp;
        this.objectAndItemsNullableProp = objectAndItemsNullableProp;
        this.objectItemsNullable = objectItemsNullable;
    }



    /**
     * Get integerProp
     * @return integerProp
     */
    public Integer getIntegerProp() {
        return integerProp;
    }

    public void setIntegerProp(Integer integerProp) {
        this.integerProp = integerProp;
    }

    /**
     * Get numberProp
     * @return numberProp
     */
    public BigDecimal getNumberProp() {
        return numberProp;
    }

    public void setNumberProp(BigDecimal numberProp) {
        this.numberProp = numberProp;
    }

    /**
     * Get booleanProp
     * @return booleanProp
     */
    public Boolean getBooleanProp() {
        return booleanProp;
    }

    public void setBooleanProp(Boolean booleanProp) {
        this.booleanProp = booleanProp;
    }

    /**
     * Get stringProp
     * @return stringProp
     */
    public String getStringProp() {
        return stringProp;
    }

    public void setStringProp(String stringProp) {
        this.stringProp = stringProp;
    }

    /**
     * Get dateProp
     * @return dateProp
     */
    public LocalDate getDateProp() {
        return dateProp;
    }

    public void setDateProp(LocalDate dateProp) {
        this.dateProp = dateProp;
    }

    /**
     * Get datetimeProp
     * @return datetimeProp
     */
    public OffsetDateTime getDatetimeProp() {
        return datetimeProp;
    }

    public void setDatetimeProp(OffsetDateTime datetimeProp) {
        this.datetimeProp = datetimeProp;
    }

    /**
     * Get arrayNullableProp
     * @return arrayNullableProp
     */
    public List<Object> getArrayNullableProp() {
        return arrayNullableProp;
    }

    public void setArrayNullableProp(List<Object> arrayNullableProp) {
        this.arrayNullableProp = arrayNullableProp;
    }

    /**
     * Get arrayAndItemsNullableProp
     * @return arrayAndItemsNullableProp
     */
    public List<Object> getArrayAndItemsNullableProp() {
        return arrayAndItemsNullableProp;
    }

    public void setArrayAndItemsNullableProp(List<Object> arrayAndItemsNullableProp) {
        this.arrayAndItemsNullableProp = arrayAndItemsNullableProp;
    }

    /**
     * Get arrayItemsNullable
     * @return arrayItemsNullable
     */
    public List<Object> getArrayItemsNullable() {
        return arrayItemsNullable;
    }

    public void setArrayItemsNullable(List<Object> arrayItemsNullable) {
        this.arrayItemsNullable = arrayItemsNullable;
    }

    /**
     * Get objectNullableProp
     * @return objectNullableProp
     */
    public Map<String, Object> getObjectNullableProp() {
        return objectNullableProp;
    }

    public void setObjectNullableProp(Map<String, Object> objectNullableProp) {
        this.objectNullableProp = objectNullableProp;
    }

    /**
     * Get objectAndItemsNullableProp
     * @return objectAndItemsNullableProp
     */
    public Map<String, Object> getObjectAndItemsNullableProp() {
        return objectAndItemsNullableProp;
    }

    public void setObjectAndItemsNullableProp(Map<String, Object> objectAndItemsNullableProp) {
        this.objectAndItemsNullableProp = objectAndItemsNullableProp;
    }

    /**
     * Get objectItemsNullable
     * @return objectItemsNullable
     */
    public Map<String, Object> getObjectItemsNullable() {
        return objectItemsNullable;
    }

    public void setObjectItemsNullable(Map<String, Object> objectItemsNullable) {
        this.objectItemsNullable = objectItemsNullable;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class NullableClass {\n");
        sb.append("    ").append(toIndentedString(super.toString())).append("\n");
        sb.append("    integerProp: ").append(toIndentedString(integerProp)).append("\n");
        sb.append("    numberProp: ").append(toIndentedString(numberProp)).append("\n");
        sb.append("    booleanProp: ").append(toIndentedString(booleanProp)).append("\n");
        sb.append("    stringProp: ").append(toIndentedString(stringProp)).append("\n");
        sb.append("    dateProp: ").append(toIndentedString(dateProp)).append("\n");
        sb.append("    datetimeProp: ").append(toIndentedString(datetimeProp)).append("\n");
        sb.append("    arrayNullableProp: ").append(toIndentedString(arrayNullableProp)).append("\n");
        sb.append("    arrayAndItemsNullableProp: ").append(toIndentedString(arrayAndItemsNullableProp)).append("\n");
        sb.append("    arrayItemsNullable: ").append(toIndentedString(arrayItemsNullable)).append("\n");
        sb.append("    objectNullableProp: ").append(toIndentedString(objectNullableProp)).append("\n");
        sb.append("    objectAndItemsNullableProp: ").append(toIndentedString(objectAndItemsNullableProp)).append("\n");
        sb.append("    objectItemsNullable: ").append(toIndentedString(objectItemsNullable)).append("\n");
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

