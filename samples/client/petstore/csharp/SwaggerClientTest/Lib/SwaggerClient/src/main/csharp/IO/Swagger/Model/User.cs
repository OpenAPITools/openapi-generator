using System;
using System.IO;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace IO.Swagger.Model {

  /// <summary>
  /// 
  /// </summary>
  [DataContract]
  public class User {
    
  
    private Id Id;

    /// <summary>
    /// Gets or Sets Id
    /// </summary>
    [DataMember(Name="id", EmitDefaultValue=false)]
    public Id Id { get; set; }
    
  
    private Username Username;

    /// <summary>
    /// Gets or Sets Username
    /// </summary>
    [DataMember(Name="username", EmitDefaultValue=false)]
    public Username Username { get; set; }
    
  
    private FirstName FirstName;

    /// <summary>
    /// Gets or Sets FirstName
    /// </summary>
    [DataMember(Name="firstName", EmitDefaultValue=false)]
    public FirstName FirstName { get; set; }
    
  
    private LastName LastName;

    /// <summary>
    /// Gets or Sets LastName
    /// </summary>
    [DataMember(Name="lastName", EmitDefaultValue=false)]
    public LastName LastName { get; set; }
    
  
    private Email Email;

    /// <summary>
    /// Gets or Sets Email
    /// </summary>
    [DataMember(Name="email", EmitDefaultValue=false)]
    public Email Email { get; set; }
    
  
    private Password Password;

    /// <summary>
    /// Gets or Sets Password
    /// </summary>
    [DataMember(Name="password", EmitDefaultValue=false)]
    public Password Password { get; set; }
    
  
    private Phone Phone;

    /// <summary>
    /// Gets or Sets Phone
    /// </summary>
    [DataMember(Name="phone", EmitDefaultValue=false)]
    public Phone Phone { get; set; }
    
  
    private UserStatus UserStatus;

    /// <summary>
    /// User Status
    /// </summary>
    /// <value>User Status</value>
    [DataMember(Name="userStatus", EmitDefaultValue=false)]
    public UserStatus UserStatus { get; set; }
    

    
    /// <summary>
    /// Gets or Sets Id
    /// </summary>
    [DataMember(Name="id", EmitDefaultValue=false)]
    public long? Id { get; set; }

    
    /// <summary>
    /// Gets or Sets Username
    /// </summary>
    [DataMember(Name="username", EmitDefaultValue=false)]
    public string Username { get; set; }

    
    /// <summary>
    /// Gets or Sets FirstName
    /// </summary>
    [DataMember(Name="firstName", EmitDefaultValue=false)]
    public string FirstName { get; set; }

    
    /// <summary>
    /// Gets or Sets LastName
    /// </summary>
    [DataMember(Name="lastName", EmitDefaultValue=false)]
    public string LastName { get; set; }

    
    /// <summary>
    /// Gets or Sets Email
    /// </summary>
    [DataMember(Name="email", EmitDefaultValue=false)]
    public string Email { get; set; }

    
    /// <summary>
    /// Gets or Sets Password
    /// </summary>
    [DataMember(Name="password", EmitDefaultValue=false)]
    public string Password { get; set; }

    
    /// <summary>
    /// Gets or Sets Phone
    /// </summary>
    [DataMember(Name="phone", EmitDefaultValue=false)]
    public string Phone { get; set; }

    
    /// <summary>
    /// User Status
    /// </summary>
    /// <value>User Status</value>
    [DataMember(Name="userStatus", EmitDefaultValue=false)]
    public int? UserStatus { get; set; }

    

    /// <summary>
    /// Get the string presentation of the object
    /// </summary>
    /// <returns>String presentation of the object</returns>
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

    /// <summary>
    /// Get the JSON string presentation of the object
    /// </summary>
    /// <returns>JSON string presentation of the object</returns>
    public string ToJson() {
      return JsonConvert.SerializeObject(this, Formatting.Indented);
    }

}
}
