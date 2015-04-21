using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.Model {
  public class PetTest {
    

    
    public long? Id { get; set; }

    

    
    public Category Category { get; set; }

    

    
    public string Name { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class PetTest {\n");
      
      sb.Append("  Id: ").Append(Id).Append("\n");
      
      sb.Append("  Category: ").Append(Category).Append("\n");
      
      sb.Append("  Name: ").Append(Name).Append("\n");
      
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  
  
}