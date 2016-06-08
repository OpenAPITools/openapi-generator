package io.swagger.client.model;

import java.util.Objects;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import com.google.gson.annotations.SerializedName;


/**
 * ModelApiResponse
 */
public class ModelApiResponse   {
    @SerializedName("code")
    private Integer code = null;
    @SerializedName("type")
    private String type = null;
    @SerializedName("message")
    private String message = null;

    /**
     * Get code
     * @return code
     **/
    @ApiModelProperty(value = "")
    public Integer getCode() {
        return code;
    }

    /**
     * Set code
     *
     * @param code code
     */
    public void setCode(Integer code) {
        this.code = code;
    }

    /**
     * Get type
     * @return type
     **/
    @ApiModelProperty(value = "")
    public String getType() {
        return type;
    }

    /**
     * Set type
     *
     * @param type type
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Get message
     * @return message
     **/
    @ApiModelProperty(value = "")
    public String getMessage() {
        return message;
    }

    /**
     * Set message
     *
     * @param message message
     */
    public void setMessage(String message) {
        this.message = message;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ModelApiResponse _apiResponse = (ModelApiResponse) o;
        return Objects.equals(this.code, _apiResponse.code) &&
        Objects.equals(this.type, _apiResponse.type) &&
        Objects.equals(this.message, _apiResponse.message);
    }

    @Override
    public int hashCode() {
        return Objects.hash(code, type, message);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class ModelApiResponse {\n");
        
        sb.append("    code: ").append(toIndentedString(code)).append("\n");
        sb.append("    type: ").append(toIndentedString(type)).append("\n");
        sb.append("    message: ").append(toIndentedString(message)).append("\n");
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

