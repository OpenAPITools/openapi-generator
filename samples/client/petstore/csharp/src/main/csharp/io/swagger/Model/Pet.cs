using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;

namespace IO.Swagger.Model {
  [DataContract]
  public class Pet {
    
    
    [DataMember(Name="id", EmitDefaultValue=false)]
    public long? Id { get; set; }

    
    
    [DataMember(Name="category", EmitDefaultValue=false)]
    public Category Category { get; set; }

    
    
    [DataMember(Name="name", EmitDefaultValue=false)]
    public string Name { get; set; }

    
    
    [DataMember(Name="photoUrls", EmitDefaultValue=false)]
    public List<string> PhotoUrls { get; set; }

    
    
    [DataMember(Name="tags", EmitDefaultValue=false)]
    public List<Tag> Tags { get; set; }

    
    /* pet status in the store */
    [DataMember(Name="status", EmitDefaultValue=false)]
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
