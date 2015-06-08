package io.swagger.client.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;

import java.util.Date;


@ApiModel(description = "")
public class AudioFile {

    private String attributionUrl = null;
    private Integer commentCount = null;
    private Integer voteCount = null;
    private String fileUrl = null;
    private String audioType = null;
    private Long id = null;
    private Double duration = null;
    private String attributionText = null;
    private String createdBy = null;
    private String description = null;
    private Date createdAt = null;
    private Float voteWeightedAverage = null;
    private Float voteAverage = null;
    private String word = null;


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("attributionUrl")
    public String getAttributionUrl() {
        return attributionUrl;
    }

    public void setAttributionUrl(String attributionUrl) {
        this.attributionUrl = attributionUrl;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("commentCount")
    public Integer getCommentCount() {
        return commentCount;
    }

    public void setCommentCount(Integer commentCount) {
        this.commentCount = commentCount;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("voteCount")
    public Integer getVoteCount() {
        return voteCount;
    }

    public void setVoteCount(Integer voteCount) {
        this.voteCount = voteCount;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("fileUrl")
    public String getFileUrl() {
        return fileUrl;
    }

    public void setFileUrl(String fileUrl) {
        this.fileUrl = fileUrl;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("audioType")
    public String getAudioType() {
        return audioType;
    }

    public void setAudioType(String audioType) {
        this.audioType = audioType;
    }


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
    @JsonProperty("duration")
    public Double getDuration() {
        return duration;
    }

    public void setDuration(Double duration) {
        this.duration = duration;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("attributionText")
    public String getAttributionText() {
        return attributionText;
    }

    public void setAttributionText(String attributionText) {
        this.attributionText = attributionText;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("createdBy")
    public String getCreatedBy() {
        return createdBy;
    }

    public void setCreatedBy(String createdBy) {
        this.createdBy = createdBy;
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
    @JsonProperty("voteWeightedAverage")
    public Float getVoteWeightedAverage() {
        return voteWeightedAverage;
    }

    public void setVoteWeightedAverage(Float voteWeightedAverage) {
        this.voteWeightedAverage = voteWeightedAverage;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("voteAverage")
    public Float getVoteAverage() {
        return voteAverage;
    }

    public void setVoteAverage(Float voteAverage) {
        this.voteAverage = voteAverage;
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


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class AudioFile {\n");

        sb.append("  attributionUrl: ").append(attributionUrl).append("\n");
        sb.append("  commentCount: ").append(commentCount).append("\n");
        sb.append("  voteCount: ").append(voteCount).append("\n");
        sb.append("  fileUrl: ").append(fileUrl).append("\n");
        sb.append("  audioType: ").append(audioType).append("\n");
        sb.append("  id: ").append(id).append("\n");
        sb.append("  duration: ").append(duration).append("\n");
        sb.append("  attributionText: ").append(attributionText).append("\n");
        sb.append("  createdBy: ").append(createdBy).append("\n");
        sb.append("  description: ").append(description).append("\n");
        sb.append("  createdAt: ").append(createdAt).append("\n");
        sb.append("  voteWeightedAverage: ").append(voteWeightedAverage).append("\n");
        sb.append("  voteAverage: ").append(voteAverage).append("\n");
        sb.append("  word: ").append(word).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
