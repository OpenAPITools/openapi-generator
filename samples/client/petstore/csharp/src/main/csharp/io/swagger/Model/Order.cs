using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

    
        namespace IO.Swagger.Model {

        ///
        <summary>
            /// 
            ///
        </summary>
        [DataContract]
        public class Order {
        
            
            [DataMember(Name="id", EmitDefaultValue=false)]
            public long? Id { get; set; }

        
            
            [DataMember(Name="petId", EmitDefaultValue=false)]
            public long? PetId { get; set; }

        
            
            [DataMember(Name="quantity", EmitDefaultValue=false)]
            public int? Quantity { get; set; }

        
            
            [DataMember(Name="shipDate", EmitDefaultValue=false)]
            public DateTime? ShipDate { get; set; }

        
            /* Order Status */
            [DataMember(Name="status", EmitDefaultValue=false)]
            public string Status { get; set; }

        
            
            [DataMember(Name="complete", EmitDefaultValue=false)]
            public bool? Complete { get; set; }

        

        ///
        <summary>
            /// Get the string presentation of the object
            ///
        </summary>
        ///
        <returns>String presentation of the object</returns>
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

        ///
        <summary>
            /// Get the JSON string presentation of the object
            ///
        </summary>
        ///
        <returns>JSON string presentation of the object</returns>
        public string ToJson() {
        return JsonConvert.SerializeObject(this, Formatting.Indented);
        }

        }
    
}
