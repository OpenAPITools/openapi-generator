package org.openapitools.server.model;




public class Capitalization   {

    private String smallCamel;
    private String capitalCamel;
    private String smallSnake;
    private String capitalSnake;
    private String scAETHFlowPoints;
    private String ATT_NAME;

    /**
     * Default constructor.
     */
    public Capitalization() {
    // JSON-B / Jackson
    }

    /**
     * Create Capitalization.
     *
     * @param smallCamel smallCamel
     * @param capitalCamel capitalCamel
     * @param smallSnake smallSnake
     * @param capitalSnake capitalSnake
     * @param scAETHFlowPoints scAETHFlowPoints
     * @param ATT_NAME Name of the pet 
     */
    public Capitalization(
        String smallCamel, 
        String capitalCamel, 
        String smallSnake, 
        String capitalSnake, 
        String scAETHFlowPoints, 
        String ATT_NAME
    ) {
        this.smallCamel = smallCamel;
        this.capitalCamel = capitalCamel;
        this.smallSnake = smallSnake;
        this.capitalSnake = capitalSnake;
        this.scAETHFlowPoints = scAETHFlowPoints;
        this.ATT_NAME = ATT_NAME;
    }



    /**
     * Get smallCamel
     * @return smallCamel
     */
    public String getSmallCamel() {
        return smallCamel;
    }

    public void setSmallCamel(String smallCamel) {
        this.smallCamel = smallCamel;
    }

    /**
     * Get capitalCamel
     * @return capitalCamel
     */
    public String getCapitalCamel() {
        return capitalCamel;
    }

    public void setCapitalCamel(String capitalCamel) {
        this.capitalCamel = capitalCamel;
    }

    /**
     * Get smallSnake
     * @return smallSnake
     */
    public String getSmallSnake() {
        return smallSnake;
    }

    public void setSmallSnake(String smallSnake) {
        this.smallSnake = smallSnake;
    }

    /**
     * Get capitalSnake
     * @return capitalSnake
     */
    public String getCapitalSnake() {
        return capitalSnake;
    }

    public void setCapitalSnake(String capitalSnake) {
        this.capitalSnake = capitalSnake;
    }

    /**
     * Get scAETHFlowPoints
     * @return scAETHFlowPoints
     */
    public String getScAETHFlowPoints() {
        return scAETHFlowPoints;
    }

    public void setScAETHFlowPoints(String scAETHFlowPoints) {
        this.scAETHFlowPoints = scAETHFlowPoints;
    }

    /**
     * Name of the pet 
     * @return ATT_NAME
     */
    public String getATTNAME() {
        return ATT_NAME;
    }

    public void setATTNAME(String ATT_NAME) {
        this.ATT_NAME = ATT_NAME;
    }

    /**
      * Create a string representation of this pojo.
    **/
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Capitalization {\n");
        
        sb.append("    smallCamel: ").append(toIndentedString(smallCamel)).append("\n");
        sb.append("    capitalCamel: ").append(toIndentedString(capitalCamel)).append("\n");
        sb.append("    smallSnake: ").append(toIndentedString(smallSnake)).append("\n");
        sb.append("    capitalSnake: ").append(toIndentedString(capitalSnake)).append("\n");
        sb.append("    scAETHFlowPoints: ").append(toIndentedString(scAETHFlowPoints)).append("\n");
        sb.append("    ATT_NAME: ").append(toIndentedString(ATT_NAME)).append("\n");
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

