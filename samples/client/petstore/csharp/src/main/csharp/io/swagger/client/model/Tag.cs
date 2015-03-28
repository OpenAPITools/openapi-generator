using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.client.model {
  public class Tag {
    

    
    public Long id { get; set; }

    

    
    public String name { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Tag {\n");
      
      sb.Append("  id: ").Append(id).Append("\n");
      
      sb.Append("  name: ").Append(name).Append("\n");
      
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  
  
}