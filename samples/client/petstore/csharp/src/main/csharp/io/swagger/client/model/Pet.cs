using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.client.model {
  public class Pet {
    

    
    public Long id { get; set; }

    

    
    public Category category { get; set; }

    

    
    public String name { get; set; }

    

    
    public List<String> photoUrls { get; set; }

    

    
    public List<Tag> tags { get; set; }

    

    /* pet status in the store */
    
    public String status { get; set; }

    

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