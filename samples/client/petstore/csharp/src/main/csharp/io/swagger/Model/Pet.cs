using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.Model {
  public class Pet {
    

    
    public long? Id { get; set; }

    

    
    public Category Category { get; set; }

    

    
    public string Name { get; set; }

    

    
    public List<string> PhotoUrls { get; set; }

    

    
    public List<Tag> Tags { get; set; }

    

    /* pet status in the store */
    
    public string Status { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Pet {\n");
      
      sb.Append("  Id: ").Append(Id).Append("\n");
      
      sb.Append("  Category: ").Append(Category).Append("\n");
      
      sb.Append("  Name: ").Append(Name).Append("\n");
      
      sb.Append("  PhotoUrls: ").Append(PhotoUrls).Append("\n");
      
      sb.Append("  Tags: ").Append(Tags).Append("\n");
      
      sb.Append("  Status: ").Append(Status).Append("\n");
      
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  
  
}