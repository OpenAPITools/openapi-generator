package io.swagger.client.model;


import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Category  {
    

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("id")
    private Long id = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("name")
    private String name = null;

        
    public Long getId() {
        return id;
    }
    public void setId(Long id) {
        this.id = id;
    }
        
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    
    @Override
    public String toString()  {
        StringBuilder sb = new StringBuilder();
        sb.append("class Category {\n");
        
        sb.append("  id: ").append(id).append("\n");
        sb.append("  name: ").append(name).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
