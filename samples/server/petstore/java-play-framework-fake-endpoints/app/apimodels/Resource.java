package apimodels;

import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * Must be named &#x60;Resource&#x60; for test.
 */

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class Resource   {
  @JsonProperty("sourceURI")
  private String sourceURI;

  public Resource sourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
    return this;
  }

   /**
   * Test resource
   * @return sourceURI
  **/
    public String getSourceURI() {
    return sourceURI;
  }

  public void setSourceURI(String sourceURI) {
    this.sourceURI = sourceURI;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Resource resource = (Resource) o;
    return Objects.equals(sourceURI, resource.sourceURI);
  }

  @Override
  public int hashCode() {
    return Objects.hash(sourceURI);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class Resource {\n");
    
    sb.append("    sourceURI: ").append(toIndentedString(sourceURI)).append("\n");
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

