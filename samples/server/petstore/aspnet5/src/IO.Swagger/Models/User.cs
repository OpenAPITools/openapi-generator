using System;
using System.Linq;
using System.IO;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace IO.Swagger.Models
{
    /// <summary>
    /// 
    /// </summary>
    public partial class User :  IEquatable<User>
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="User" /> class.
        /// </summary>
        public User()
        {
            
        }

        
        /// <summary>
        /// Gets or Sets Id
        /// </summary>
        public long? Id { get; set; }

        
        /// <summary>
        /// Gets or Sets Username
        /// </summary>
        public string Username { get; set; }

        
        /// <summary>
        /// Gets or Sets FirstName
        /// </summary>
        public string FirstName { get; set; }

        
        /// <summary>
        /// Gets or Sets LastName
        /// </summary>
        public string LastName { get; set; }

        
        /// <summary>
        /// Gets or Sets Email
        /// </summary>
        public string Email { get; set; }

        
        /// <summary>
        /// Gets or Sets Password
        /// </summary>
        public string Password { get; set; }

        
        /// <summary>
        /// Gets or Sets Phone
        /// </summary>
        public string Phone { get; set; }

        
        /// <summary>
        /// User Status
        /// </summary>
        /// <value>User Status</value>
        public int? UserStatus { get; set; }

        

        /// <summary>
        /// Returns the string presentation of the object
        /// </summary>
        /// <returns>String presentation of the object</returns>
        public override string ToString()
        {
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
        /// Returns the JSON string presentation of the object
        /// </summary>
        /// <returns>JSON string presentation of the object</returns>
        public string ToJson()
        {
            return JsonConvert.SerializeObject(this, Formatting.Indented);
        }

        /// <summary>
        /// Returns true if objects are equal
        /// </summary>
        /// <param name="obj">Object to be compared</param>
        /// <returns>Boolean</returns>
        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != GetType()) return false;
            return Equals((User)obj);
        }

        /// <summary>
        /// Returns true if User instances are equal
        /// </summary>
        /// <param name="other">Instance of User to be compared</param>
        /// <returns>Boolean</returns>
        public bool Equals(User other)
        {

            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;

            return 
                (
                    this.Id == other.Id ||
                    this.Id != null &&
                    this.Id.Equals(other.Id)
                ) && 
                (
                    this.Username == other.Username ||
                    this.Username != null &&
                    this.Username.Equals(other.Username)
                ) && 
                (
                    this.FirstName == other.FirstName ||
                    this.FirstName != null &&
                    this.FirstName.Equals(other.FirstName)
                ) && 
                (
                    this.LastName == other.LastName ||
                    this.LastName != null &&
                    this.LastName.Equals(other.LastName)
                ) && 
                (
                    this.Email == other.Email ||
                    this.Email != null &&
                    this.Email.Equals(other.Email)
                ) && 
                (
                    this.Password == other.Password ||
                    this.Password != null &&
                    this.Password.Equals(other.Password)
                ) && 
                (
                    this.Phone == other.Phone ||
                    this.Phone != null &&
                    this.Phone.Equals(other.Phone)
                ) && 
                (
                    this.UserStatus == other.UserStatus ||
                    this.UserStatus != null &&
                    this.UserStatus.Equals(other.UserStatus)
                );
        }

        /// <summary>
        /// Gets the hash code
        /// </summary>
        /// <returns>Hash code</returns>
        public override int GetHashCode()
        {
            // credit: http://stackoverflow.com/a/263416/677735
            unchecked // Overflow is fine, just wrap
            {
                int hash = 41;
                // Suitable nullity checks etc, of course :)
                
                    if (this.Id != null)
                    hash = hash * 59 + this.Id.GetHashCode();
                
                    if (this.Username != null)
                    hash = hash * 59 + this.Username.GetHashCode();
                
                    if (this.FirstName != null)
                    hash = hash * 59 + this.FirstName.GetHashCode();
                
                    if (this.LastName != null)
                    hash = hash * 59 + this.LastName.GetHashCode();
                
                    if (this.Email != null)
                    hash = hash * 59 + this.Email.GetHashCode();
                
                    if (this.Password != null)
                    hash = hash * 59 + this.Password.GetHashCode();
                
                    if (this.Phone != null)
                    hash = hash * 59 + this.Phone.GetHashCode();
                
                    if (this.UserStatus != null)
                    hash = hash * 59 + this.UserStatus.GetHashCode();
                
                return hash;
            }
        }

        #region Operators

        public static bool operator ==(User left, User right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(User left, User right)
        {
            return !Equals(left, right);
        }

        #endregion Operators

    }
}
