package apimodels;

import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * TestJsonFormDataBody
 */

@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class TestJsonFormDataBody   {
  @JsonProperty("param")
  private String param;

  @JsonProperty("param2")
  private String param2;

  public TestJsonFormDataBody param(String param) {
    this.param = param;
    return this;
  }

   /**
   * field1
   * @return param
  **/
  @NotNull
  public String getParam() {
    return param;
  }

  public void setParam(String param) {
    this.param = param;
  }

  public TestJsonFormDataBody param2(String param2) {
    this.param2 = param2;
    return this;
  }

   /**
   * field2
   * @return param2
  **/
  @NotNull
  public String getParam2() {
    return param2;
  }

  public void setParam2(String param2) {
    this.param2 = param2;
  }


  @Override
  public boolean equals(java.lang.Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    TestJsonFormDataBody testJsonFormDataBody = (TestJsonFormDataBody) o;
    return Objects.equals(param, testJsonFormDataBody.param) &&
        Objects.equals(param2, testJsonFormDataBody.param2);
  }

  @Override
  public int hashCode() {
    return Objects.hash(param, param2);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class TestJsonFormDataBody {\n");
    
    sb.append("    param: ").append(toIndentedString(param)).append("\n");
    sb.append("    param2: ").append(toIndentedString(param2)).append("\n");
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

