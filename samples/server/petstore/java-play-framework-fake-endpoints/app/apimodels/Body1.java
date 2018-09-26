package apimodels;

import java.io.InputStream;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * Body1
 */

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class Body1   {
  @JsonProperty("additionalMetadata")
  private String additionalMetadata = null;

  @JsonProperty("file")
  private InputStream file = null;

  public Body1 additionalMetadata(String additionalMetadata) {
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

  public Body1 file(InputStream file) {
    this.file = file;
    return this;
  }

   /**
   * file to upload
   * @return file
  **/
  @Valid
  public InputStream getFile() {
    return file;
  }

  public void setFile(InputStream file) {
    this.file = file;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Body1 body1 = (Body1) o;
    return Objects.equals(additionalMetadata, body1.additionalMetadata) &&
        Objects.equals(file, body1.file);
  }

  @Override
  public int hashCode() {
    return Objects.hash(additionalMetadata, file);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Body1 {\n");
    
    sb.append("    additionalMetadata: ").append(toIndentedString(additionalMetadata)).append("\n");
    sb.append("    file: ").append(toIndentedString(file)).append("\n");
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

