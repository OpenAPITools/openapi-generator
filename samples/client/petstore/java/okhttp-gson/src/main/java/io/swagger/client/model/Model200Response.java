package io.swagger.client.model;

import java.util.Objects;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import com.google.gson.annotations.SerializedName;


/**
 * Model for testing model name starting with number
 */
@ApiModel(description = "Model for testing model name starting with number")
public class Model200Response   {
    @SerializedName("name")
    private Integer name = null;

    /**
     * Get name
     * @return name
     **/
    @ApiModelProperty(value = "")
    public Integer getName() {
        return name;
    }

    /**
     * Set name
     *
     * @param name name
     */
    public void setName(Integer name) {
        this.name = name;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Model200Response _200Response = (Model200Response) o;
        return Objects.equals(this.name, _200Response.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class Model200Response {\n");
        
        sb.append("    name: ").append(toIndentedString(name)).append("\n");
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

