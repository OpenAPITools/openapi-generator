package io.swagger.client.model;

import io.swagger.client.model.Category;
import io.swagger.client.model.Tag;
import java.util.*;

import io.swagger.annotations.*;
import com.google.gson.annotations.SerializedName;


@ApiModel(description = "")
public class Pet  {
    

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("id")
    private Long id = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("category")
    private Category category = null;

    /**
     **/
    @ApiModelProperty(required = true, value = "")  
    @SerializedName("name")
    private String name = null;

    /**
     **/
    @ApiModelProperty(required = true, value = "")  
    @SerializedName("photoUrls")
    private List<String> photoUrls = null;

    /**
     **/
    @ApiModelProperty(value = "")  
    @SerializedName("tags")
    private List<Tag> tags = null;
    public enum StatusEnum {
         available,  pending,  sold, 
    };

    /**
     * pet status in the store
     **/
    @ApiModelProperty(value = "pet status in the store")  
    @SerializedName("status")    
    private StatusEnum status = null;

        
    public Long getId() {
        return id;
    }
    public void setId(Long id) {
        this.id = id;
    }
        
    public Category getCategory() {
        return category;
    }
    public void setCategory(Category category) {
        this.category = category;
    }
        
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
        
    public List<String> getPhotoUrls() {
        return photoUrls;
    }
    public void setPhotoUrls(List<String> photoUrls) {
        this.photoUrls = photoUrls;
    }
        
    public List<Tag> getTags() {
        return tags;
    }
    public void setTags(List<Tag> tags) {
        this.tags = tags;
    }
        
    public StatusEnum getStatus() {
        return status;
    }
    public void setStatus(StatusEnum status) {
        this.status = status;
    }
    
    @Override
    public String toString()  {
        StringBuilder sb = new StringBuilder();
        sb.append("class Pet {\n");
        
        sb.append("  id: ").append(id).append("\n");
        sb.append("  category: ").append(category).append("\n");
        sb.append("  name: ").append(name).append("\n");
        sb.append("  photoUrls: ").append(photoUrls).append("\n");
        sb.append("  tags: ").append(tags).append("\n");
        sb.append("  status: ").append(status).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
