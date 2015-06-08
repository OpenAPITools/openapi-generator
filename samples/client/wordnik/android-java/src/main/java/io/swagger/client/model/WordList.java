package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;

import java.util.Date;


@ApiModel(description = "")
public class WordList {

    private Long id = null;
    private String permalink = null;
    private String name = null;
    private Date createdAt = null;
    private Date updatedAt = null;
    private Date lastActivityAt = null;
    private String username = null;
    private Long userId = null;
    private String description = null;
    private Long numberWordsInList = null;
    private String type = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("id")
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("permalink")
    public String getPermalink() {
        return permalink;
    }

    public void setPermalink(String permalink) {
        this.permalink = permalink;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("name")
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("createdAt")
    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("updatedAt")
    public Date getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
        this.updatedAt = updatedAt;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("lastActivityAt")
    public Date getLastActivityAt() {
        return lastActivityAt;
    }

    public void setLastActivityAt(Date lastActivityAt) {
        this.lastActivityAt = lastActivityAt;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("username")
    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("userId")
    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("description")
    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("numberWordsInList")
    public Long getNumberWordsInList() {
        return numberWordsInList;
    }

    public void setNumberWordsInList(Long numberWordsInList) {
        this.numberWordsInList = numberWordsInList;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("type")
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class WordList {\n");

        sb.append("  id: ").append(id).append("\n");
        sb.append("  permalink: ").append(permalink).append("\n");
        sb.append("  name: ").append(name).append("\n");
        sb.append("  createdAt: ").append(createdAt).append("\n");
        sb.append("  updatedAt: ").append(updatedAt).append("\n");
        sb.append("  lastActivityAt: ").append(lastActivityAt).append("\n");
        sb.append("  username: ").append(username).append("\n");
        sb.append("  userId: ").append(userId).append("\n");
        sb.append("  description: ").append(description).append("\n");
        sb.append("  numberWordsInList: ").append(numberWordsInList).append("\n");
        sb.append("  type: ").append(type).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
