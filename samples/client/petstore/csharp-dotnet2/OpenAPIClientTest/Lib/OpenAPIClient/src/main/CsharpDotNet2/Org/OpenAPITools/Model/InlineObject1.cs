using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace Org.OpenAPITools.Model {

  /// <summary>
  /// 
  /// </summary>
  [DataContract]
  public class InlineObject1 {
    /// <summary>
    /// Additional data to pass to server
    /// </summary>
    /// <value>Additional data to pass to server</value>
    [DataMember(Name="additionalMetadata", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "additionalMetadata")]
    public string AdditionalMetadata { get; set; }

    /// <summary>
    /// file to upload
    /// </summary>
    /// <value>file to upload</value>
    [DataMember(Name="file", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "file")]
    public System.IO.Stream File { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class InlineObject1 {\n");
      sb.Append("  AdditionalMetadata: ").Append(AdditionalMetadata).Append("\n");
      sb.Append("  File: ").Append(File).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }

    /// <summary>
    /// Get the JSON string presentation of the object
    /// </summary>
    /// <returns>JSON string presentation of the object</returns>
    public string ToJson() {
      return JsonConvert.SerializeObject(this, Formatting.Indented);
    }

}
}
