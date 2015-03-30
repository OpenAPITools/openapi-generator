using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace io.swagger.Model {
  public class User {
    

    
    public long? Id { get; set; }

    

    
    public string Username { get; set; }

    

    
    public string FirstName { get; set; }

    

    
    public string LastName { get; set; }

    

    
    public string Email { get; set; }

    

    
    public string Password { get; set; }

    

    
    public string Phone { get; set; }

    

    /* User Status */
    
    public int? UserStatus { get; set; }

    

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class User {\n");
      
      sb.Append("  Id: ").Append(Id).Append("\n");
      
      sb.Append("  Username: ").Append(Username).Append("\n");
      
      sb.Append("  FirstName: ").Append(FirstName).Append("\n");
      
      sb.Append("  LastName: ").Append(LastName).Append("\n");
      
      sb.Append("  Email: ").Append(Email).Append("\n");
      
      sb.Append("  Password: ").Append(Password).Append("\n");
      
      sb.Append("  Phone: ").Append(Phone).Append("\n");
      
      sb.Append("  UserStatus: ").Append(UserStatus).Append("\n");
      
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  
  
}