using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

    
        namespace IO.Swagger.Model {

        ///
        <summary>
            /// 
            ///
        </summary>
        [DataContract]
        public class User {
        
            
            [DataMember(Name="id", EmitDefaultValue=false)]
            public long? Id { get; set; }

        
            
            [DataMember(Name="username", EmitDefaultValue=false)]
            public string Username { get; set; }

        
            
            [DataMember(Name="firstName", EmitDefaultValue=false)]
            public string FirstName { get; set; }

        
            
            [DataMember(Name="lastName", EmitDefaultValue=false)]
            public string LastName { get; set; }

        
            
            [DataMember(Name="email", EmitDefaultValue=false)]
            public string Email { get; set; }

        
            
            [DataMember(Name="password", EmitDefaultValue=false)]
            public string Password { get; set; }

        
            
            [DataMember(Name="phone", EmitDefaultValue=false)]
            public string Phone { get; set; }

        
            /* User Status */
            [DataMember(Name="userStatus", EmitDefaultValue=false)]
            public int? UserStatus { get; set; }

        

        ///
        <summary>
            /// Get the string presentation of the object
            ///
        </summary>
        ///
        <returns>String presentation of the object</returns>
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

        ///
        <summary>
            /// Get the JSON string presentation of the object
            ///
        </summary>
        ///
        <returns>JSON string presentation of the object</returns>
        public string ToJson() {
        return JsonConvert.SerializeObject(this, Formatting.Indented);
        }

        }
    
}
