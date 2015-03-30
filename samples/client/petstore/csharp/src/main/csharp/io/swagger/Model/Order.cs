using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.Model {
  public class Order {
    

    
    public long? Id { get; set; }

    

    
    public long? PetId { get; set; }

    

    
    public int? Quantity { get; set; }

    

    
    public DateTime ShipDate { get; set; }

    

    /* Order Status */
    
    public string Status { get; set; }

    

    
    public bool? Complete { get; set; }

    

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
  }
  
  
}