using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.client.model {
  public class Order {
    

    
    public Long id { get; set; }

    

    
    public Long petId { get; set; }

    

    
    public Integer quantity { get; set; }

    

    
    public Date shipDate { get; set; }

    

    /* Order Status */
    
    public String status { get; set; }

    

    
    public Boolean complete { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Order {\n");
      
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