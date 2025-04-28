package org.openapitools.model;

import com.fasterxml.jackson.annotation.JsonTypeName;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.io.Serializable;
import javax.validation.constraints.*;
import javax.validation.Valid;

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonTypeName;



@org.eclipse.microprofile.openapi.annotations.media.Schema(description="")
@JsonTypeName("fakeBigDecimalMap_200_response")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.14.0-SNAPSHOT")
public class FakeBigDecimalMap200Response  implements Serializable {
  private BigDecimal someId;
  private @Valid Map<String, BigDecimal> someMap = new HashMap<>();

  protected FakeBigDecimalMap200Response(FakeBigDecimalMap200ResponseBuilder<?, ?> b) {
    this.someId = b.someId;
    this.someMap = b.someMap;
  }

  public FakeBigDecimalMap200Response() {
  }

  /**
   **/
  public FakeBigDecimalMap200Response someId(BigDecimal someId) {
    this.someId = someId;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("someId")
  @Valid public BigDecimal getSomeId() {
    return someId;
  }

  @JsonProperty("someId")
  public void setSomeId(BigDecimal someId) {
    this.someId = someId;
  }

  /**
   **/
  public FakeBigDecimalMap200Response someMap(Map<String, BigDecimal> someMap) {
    this.someMap = someMap;
    return this;
  }

  
  @org.eclipse.microprofile.openapi.annotations.media.Schema(description = "")
  @JsonProperty("someMap")
  @Valid public Map<String, BigDecimal> getSomeMap() {
    return someMap;
  }

  @JsonProperty("someMap")
  public void setSomeMap(Map<String, BigDecimal> someMap) {
    this.someMap = someMap;
  }

  public FakeBigDecimalMap200Response putSomeMapItem(String key, BigDecimal someMapItem) {
    if (this.someMap == null) {
      this.someMap = new HashMap<>();
    }

    this.someMap.put(key, someMapItem);
    return this;
  }

  public FakeBigDecimalMap200Response removeSomeMapItem(String key) {
    if (this.someMap != null) {
      this.someMap.remove(key);
    }

    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    FakeBigDecimalMap200Response fakeBigDecimalMap200Response = (FakeBigDecimalMap200Response) o;
    return Objects.equals(this.someId, fakeBigDecimalMap200Response.someId) &&
        Objects.equals(this.someMap, fakeBigDecimalMap200Response.someMap);
  }

  @Override
  public int hashCode() {
    return Objects.hash(someId, someMap);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class FakeBigDecimalMap200Response {\n");
    
    sb.append("    someId: ").append(toIndentedString(someId)).append("\n");
    sb.append("    someMap: ").append(toIndentedString(someMap)).append("\n");
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


  public static FakeBigDecimalMap200ResponseBuilder<?, ?> builder() {
    return new FakeBigDecimalMap200ResponseBuilderImpl();
  }

  private static final class FakeBigDecimalMap200ResponseBuilderImpl extends FakeBigDecimalMap200ResponseBuilder<FakeBigDecimalMap200Response, FakeBigDecimalMap200ResponseBuilderImpl> {

    @Override
    protected FakeBigDecimalMap200ResponseBuilderImpl self() {
      return this;
    }

    @Override
    public FakeBigDecimalMap200Response build() {
      return new FakeBigDecimalMap200Response(this);
    }
  }

  public static abstract class FakeBigDecimalMap200ResponseBuilder<C extends FakeBigDecimalMap200Response, B extends FakeBigDecimalMap200ResponseBuilder<C, B>>  {
    private BigDecimal someId;
    private Map<String, BigDecimal> someMap = new HashMap<>();
    protected abstract B self();

    public abstract C build();

    public B someId(BigDecimal someId) {
      this.someId = someId;
      return self();
    }
    public B someMap(Map<String, BigDecimal> someMap) {
      this.someMap = someMap;
      return self();
    }
  }
}

