using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.Model {
  public class Pet {
    

    
    public long? id { get; set; }

    

    
    public Category category { get; set; }

    

    
    public string name { get; set; }

    

    
    public array<string> photoUrls { get; set; }

    

    
    public array<Tag> tags { get; set; }

    

    /* pet status in the store */
    
    public string status { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Pet {\n");
      
      sb.Append("  id: ").Append(id).Append("\n");
      
      sb.Append("  category: ").Append(category).Append("\n");
      
      sb.Append("  name: ").Append(name).Append("\n");
      
      sb.Append("  photoUrls: ").Append(photoUrls).Append("\n");
      
      sb.Append("  tags: ").Append(tags).Append("\n");
      
      sb.Append("  status: ").Append(status).Append("\n");
      
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  
  
}