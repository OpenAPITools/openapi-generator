package io.swagger.client.model;


import com.fasterxml.jackson.annotation.JsonProperty;
import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class User {

    private Long id = null;
    private String username = null;
    private String email = null;
    private Integer status = null;
    private String faceBookId = null;
    private String userName = null;
    private String displayName = null;
    private String password = null;


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
    @JsonProperty("email")
    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("status")
    public Integer getStatus() {
        return status;
    }

    public void setStatus(Integer status) {
        this.status = status;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("faceBookId")
    public String getFaceBookId() {
        return faceBookId;
    }

    public void setFaceBookId(String faceBookId) {
        this.faceBookId = faceBookId;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("userName")
    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("displayName")
    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }


    /**
     **/
    @ApiModelProperty(required = false, value = "")
    @JsonProperty("password")
    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("class User {\n");

        sb.append("  id: ").append(id).append("\n");
        sb.append("  username: ").append(username).append("\n");
        sb.append("  email: ").append(email).append("\n");
        sb.append("  status: ").append(status).append("\n");
        sb.append("  faceBookId: ").append(faceBookId).append("\n");
        sb.append("  userName: ").append(userName).append("\n");
        sb.append("  displayName: ").append(displayName).append("\n");
        sb.append("  password: ").append(password).append("\n");
        sb.append("}\n");
        return sb.toString();
    }
}
