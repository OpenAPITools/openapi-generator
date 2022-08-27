package apimodels;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.fasterxml.jackson.annotation.*;
import java.util.Set;
import javax.validation.*;
import java.util.Objects;
import javax.validation.constraints.*;
/**
 * SpecialModelName
 */
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaPlayFrameworkCodegen")
@SuppressWarnings({"UnusedReturnValue", "WeakerAccess"})
public class SpecialModelName   {
  @JsonProperty("$special[property.name]")
  
  private Long $SpecialPropertyName;

  public SpecialModelName $SpecialPropertyName(Long $SpecialPropertyName) {
    this.$SpecialPropertyName = $SpecialPropertyName;
    return this;
  }

   /**
   * Get $SpecialPropertyName
   * @return $SpecialPropertyName
  **/
  public Long get$SpecialPropertyName() {
    return $SpecialPropertyName;
  }

  public void set$SpecialPropertyName(Long $SpecialPropertyName) {
    this.$SpecialPropertyName = $SpecialPropertyName;
  }


  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    SpecialModelName $SpecialModelName = (SpecialModelName) o;
    return Objects.equals($SpecialPropertyName, $SpecialModelName.$SpecialPropertyName);
  }

  @Override
  public int hashCode() {
    return Objects.hash($SpecialPropertyName);
  }

  @SuppressWarnings("StringBufferReplaceableByString")
  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class SpecialModelName {\n");
    
    sb.append("    $SpecialPropertyName: ").append(toIndentedString($SpecialPropertyName)).append("\n");
    sb.append("}");
    return sb.toString();
  }

  /**
   * Convert the given object to string with each line indented by 4 spaces
   * (except the first line).
   */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n    ");
  }
}

