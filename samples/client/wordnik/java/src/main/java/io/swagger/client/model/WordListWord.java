package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;

import java.util.Date;


@ApiModel(description = "")
public class WordListWord {

    private Long id = null;
    private String word = null;
    private String username = null;
    private Long userId = null;
    private Date createdAt = null;
    private Long numberCommentsOnWord = null;
    private Long numberLists = null;


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
    @JsonProperty("word")
    public String getWord() {
        return word;
    }

    public void setWord(String word) {
        this.word = word;
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
    @JsonProperty("numberCommentsOnWord")
    public Long getNumberCommentsOnWord() {
        return numberCommentsOnWord;
    }

    public void setNumberCommentsOnWord(Long numberCommentsOnWord) {
        this.numberCommentsOnWord = numberCommentsOnWord;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("numberLists")
    public Long getNumberLists() {
        return numberLists;
    }

    public void setNumberLists(Long numberLists) {
        this.numberLists = numberLists;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class WordListWord {\n");

        sb.append("  id: ").append(id).append("\n");
        sb.append("  word: ").append(word).append("\n");
        sb.append("  username: ").append(username).append("\n");
        sb.append("  userId: ").append(userId).append("\n");
        sb.append("  createdAt: ").append(createdAt).append("\n");
        sb.append("  numberCommentsOnWord: ").append(numberCommentsOnWord).append("\n");
        sb.append("  numberLists: ").append(numberLists).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
