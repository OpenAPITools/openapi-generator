package io.swagger.client.model;


import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class ApiResponse  {
    

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("code")
    private Integer code = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("type")
    private String type = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("message")
    private String message = null;

        
    public Integer getCode() {
        return code;
    }
    public void setCode(Integer code) {
        this.code = code;
    }
        
    public String getType() {
        return type;
    }
    public void setType(String type) {
        this.type = type;
    }
        
    public String getMessage() {
        return message;
    }
    public void setMessage(String message) {
        this.message = message;
    }
    
    @Override
    public String toString()  {
        StringBuilder sb = new StringBuilder();
        sb.append("class ApiResponse {\n");
        
        sb.append("  code: ").append(code).append("\n");
        sb.append("  type: ").append(type).append("\n");
        sb.append("  message: ").append(message).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
