package apimodels;

import java.io.InputStream;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * UploadFileWithRequiredFileBody
 */

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class UploadFileWithRequiredFileBody   {
  @JsonProperty("additionalMetadata")
  private String additionalMetadata;

  @JsonProperty("requiredFile")
  private InputStream requiredFile;

  public UploadFileWithRequiredFileBody additionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
    return this;
  }

   /**
   * Additional data to pass to server
   * @return additionalMetadata
  **/
    public String getAdditionalMetadata() {
    return additionalMetadata;
  }

  public void setAdditionalMetadata(String additionalMetadata) {
    this.additionalMetadata = additionalMetadata;
  }

  public UploadFileWithRequiredFileBody requiredFile(InputStream requiredFile) {
    this.requiredFile = requiredFile;
    return this;
  }

   /**
   * file to upload
   * @return requiredFile
  **/
  @NotNull
@Valid
  public InputStream getRequiredFile() {
    return requiredFile;
  }

  public void setRequiredFile(InputStream requiredFile) {
    this.requiredFile = requiredFile;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UploadFileWithRequiredFileBody uploadFileWithRequiredFileBody = (UploadFileWithRequiredFileBody) o;
    return Objects.equals(additionalMetadata, uploadFileWithRequiredFileBody.additionalMetadata) &&
        Objects.equals(requiredFile, uploadFileWithRequiredFileBody.requiredFile);
  }

  @Override
  public int hashCode() {
    return Objects.hash(additionalMetadata, requiredFile);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class UploadFileWithRequiredFileBody {\n");
    
    sb.append("    additionalMetadata: ").append(toIndentedString(additionalMetadata)).append("\n");
    sb.append("    requiredFile: ").append(toIndentedString(requiredFile)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(java.lang.Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

