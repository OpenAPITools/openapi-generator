package io.swagger.client.model;

import java.util.Objects;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import com.google.gson.annotations.SerializedName;


/**
 * Animal
 */
public class Animal   {
    @SerializedName("className")
    private String className = null;
    @SerializedName("color")
    private String color = "red";

    /**
     * Get className
     * @return className
     **/
    @ApiModelProperty(required = true, value = "")
    public String getClassName() {
        return className;
    }

    /**
     * Set className
     *
     * @param className className
     */
    public void setClassName(String className) {
        this.className = className;
    }

    /**
     * Get color
     * @return color
     **/
    @ApiModelProperty(value = "")
    public String getColor() {
        return color;
    }

    /**
     * Set color
     *
     * @param color color
     */
    public void setColor(String color) {
        this.color = color;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Animal animal = (Animal) o;
        return Objects.equals(this.className, animal.className) &&
        Objects.equals(this.color, animal.color);
    }

    @Override
    public int hashCode() {
        return Objects.hash(className, color);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Animal {\n");
        
        sb.append("    className: ").append(toIndentedString(className)).append("\n");
        sb.append("    color: ").append(toIndentedString(color)).append("\n");
        sb.append("}");
        return sb.toString();
    }

    /**
     * Convert the given object to string with each line indented by 4 spaces
     * (except the first line).
     *
     * @param o Object to be converted to indented string
     */
    private String toIndentedString(Object o) {
        if (o == null) {
            return "null";
        }
        return o.toString().replace("\n", "\n    ");
    }
}

