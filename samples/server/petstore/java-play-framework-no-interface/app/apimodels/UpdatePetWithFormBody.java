package apimodels;

import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * UpdatePetWithFormBody
 */

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class UpdatePetWithFormBody   {
  @JsonProperty("name")
  private String name;

  @JsonProperty("status")
  private String status;

  public UpdatePetWithFormBody name(String name) {
    this.name = name;
    return this;
  }

   /**
   * Updated name of the pet
   * @return name
  **/
    public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public UpdatePetWithFormBody status(String status) {
    this.status = status;
    return this;
  }

   /**
   * Updated status of the pet
   * @return status
  **/
    public String getStatus() {
    return status;
  }

  public void setStatus(String status) {
    this.status = status;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    UpdatePetWithFormBody updatePetWithFormBody = (UpdatePetWithFormBody) o;
    return Objects.equals(name, updatePetWithFormBody.name) &&
        Objects.equals(status, updatePetWithFormBody.status);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name, status);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class UpdatePetWithFormBody {\n");
    
    sb.append("    name: ").append(toIndentedString(name)).append("\n");
    sb.append("    status: ").append(toIndentedString(status)).append("\n");
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

