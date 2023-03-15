using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace Org.OpenAPITools.Model {

  /// <summary>
  /// An order for a pets from the pet store
  /// </summary>
  [DataContract]
  public class Order {
    /// <summary>
    /// Gets or Sets Id
    /// </summary>
    [DataMember(Name="id", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "id")]
    public long? Id { get; set; }

    /// <summary>
    /// Gets or Sets PetId
    /// </summary>
    [DataMember(Name="petId", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "petId")]
    public long? PetId { get; set; }

    /// <summary>
    /// Gets or Sets Quantity
    /// </summary>
    [DataMember(Name="quantity", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "quantity")]
    public int? Quantity { get; set; }

    /// <summary>
    /// Gets or Sets ShipDate
    /// </summary>
    [DataMember(Name="shipDate", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "shipDate")]
    public DateTime? ShipDate { get; set; }

    /// <summary>
    /// Order Status
    /// </summary>
    /// <value>Order Status</value>
    [DataMember(Name="status", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "status")]
    public string Status { get; set; }

    /// <summary>
    /// Gets or Sets Complete
    /// </summary>
    [DataMember(Name="complete", EmitDefaultValue=false)]
    [JsonProperty(PropertyName = "complete")]
    public bool? Complete { get; set; }


    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Order {\n");
      sb.Append("  Id: ").Append(Id).Append("\n");
      sb.Append("  PetId: ").Append(PetId).Append("\n");
      sb.Append("  Quantity: ").Append(Quantity).Append("\n");
      sb.Append("  ShipDate: ").Append(ShipDate).Append("\n");
      sb.Append("  Status: ").Append(Status).Append("\n");
      sb.Append("  Complete: ").Append(Complete).Append("\n");
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
