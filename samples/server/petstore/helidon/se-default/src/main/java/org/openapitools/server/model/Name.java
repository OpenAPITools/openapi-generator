package org.openapitools.server.model;



/**
 * Model for testing model name same as property name
 */
public class Name   {

    private Integer name;
    private Integer snakeCase;
    private String property;
    private Integer _123number;

    /**
     * Default constructor.
     */
    public Name() {
    // JSON-B / Jackson
    }

    /**
     * Create Name.
     *
     * @param name name
     * @param snakeCase snakeCase
     * @param property property
     * @param _123number _123number
     */
    public Name(
        Integer name, 
        Integer snakeCase, 
        String property, 
        Integer _123number
    ) {
        this.name = name;
        this.snakeCase = snakeCase;
        this.property = property;
        this._123number = _123number;
    }



    /**
     * Get name
     * @return name
     */
    public Integer getName() {
        return name;
    }

    public void setName(Integer name) {
        this.name = name;
    }

    /**
     * Get snakeCase
     * @return snakeCase
     */
    public Integer getSnakeCase() {
        return snakeCase;
    }

    public void setSnakeCase(Integer snakeCase) {
        this.snakeCase = snakeCase;
    }

    /**
     * Get property
     * @return property
     */
    public String getProperty() {
        return property;
    }

    public void setProperty(String property) {
        this.property = property;
    }

    /**
     * Get _123number
     * @return _123number
     */
    public Integer get123number() {
        return _123number;
    }

    public void set123number(Integer _123number) {
        this._123number = _123number;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Name {\n");
        
        sb.append("    name: ").append(toIndentedString(name)).append("\n");
        sb.append("    snakeCase: ").append(toIndentedString(snakeCase)).append("\n");
        sb.append("    property: ").append(toIndentedString(property)).append("\n");
        sb.append("    _123number: ").append(toIndentedString(_123number)).append("\n");
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

