using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.Model {
  public class order {
    

    
    public long? id { get; set; }

    

    
    public long? petId { get; set; }

    

    
    public int? quantity { get; set; }

    

    
    public DateTime shipDate { get; set; }

    

    /* Order Status */
    
    public string status { get; set; }

    

    
    public bool? complete { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class order {\n");
      
      sb.Append("  id: ").Append(id).Append("\n");
      
      sb.Append("  petId: ").Append(petId).Append("\n");
      
      sb.Append("  quantity: ").Append(quantity).Append("\n");
      
      sb.Append("  shipDate: ").Append(shipDate).Append("\n");
      
      sb.Append("  status: ").Append(status).Append("\n");
      
      sb.Append("  complete: ").Append(complete).Append("\n");
      
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  
  
}