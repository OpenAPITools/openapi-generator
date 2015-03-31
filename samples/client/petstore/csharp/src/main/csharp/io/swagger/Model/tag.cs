using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.Model {
  public class tag {
    

    
    public long? id { get; set; }

    

    
    public string name { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class tag {\n");
      
      sb.Append("  id: ").Append(id).Append("\n");
      
      sb.Append("  name: ").Append(name).Append("\n");
      
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  
  
}