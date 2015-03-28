using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.client.model {
  public class User {
    

    
    public Long id { get; set; }

    

    
    public String username { get; set; }

    

    
    public String firstName { get; set; }

    

    
    public String lastName { get; set; }

    

    
    public String email { get; set; }

    

    
    public String password { get; set; }

    

    
    public String phone { get; set; }

    

    /* User Status */
    
    public Integer userStatus { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class User {\n");
      
      sb.Append("  id: ").Append(id).Append("\n");
      
      sb.Append("  username: ").Append(username).Append("\n");
      
      sb.Append("  firstName: ").Append(firstName).Append("\n");
      
      sb.Append("  lastName: ").Append(lastName).Append("\n");
      
      sb.Append("  email: ").Append(email).Append("\n");
      
      sb.Append("  password: ").Append(password).Append("\n");
      
      sb.Append("  phone: ").Append(phone).Append("\n");
      
      sb.Append("  userStatus: ").Append(userStatus).Append("\n");
      
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  
  
}