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
    public partial class Pet :  IEquatable<Pet>
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="Pet" /> class.
        /// </summary>
        public Pet()
        {
            
        }

        
        /// <summary>
        /// Gets or Sets Id
        /// </summary>
        public long? Id { get; set; }

        
        /// <summary>
        /// Gets or Sets Category
        /// </summary>
        public Category Category { get; set; }

        
        /// <summary>
        /// Gets or Sets Name
        /// </summary>
        public string Name { get; set; }

        
        /// <summary>
        /// Gets or Sets PhotoUrls
        /// </summary>
        public List<string> PhotoUrls { get; set; }

        
        /// <summary>
        /// Gets or Sets Tags
        /// </summary>
        public List<Tag> Tags { get; set; }

        
        /// <summary>
        /// pet status in the store
        /// </summary>
        /// <value>pet status in the store</value>
        public string Status { get; set; }

        

        /// <summary>
        /// Returns the string presentation of the object
        /// </summary>
        /// <returns>String presentation of the object</returns>
        public override string ToString()
        {
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
            return Equals((Pet)obj);
        }

        /// <summary>
        /// Returns true if Pet instances are equal
        /// </summary>
        /// <param name="other">Instance of Pet to be compared</param>
        /// <returns>Boolean</returns>
        public bool Equals(Pet other)
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
                    this.Category == other.Category ||
                    this.Category != null &&
                    this.Category.Equals(other.Category)
                ) && 
                (
                    this.Name == other.Name ||
                    this.Name != null &&
                    this.Name.Equals(other.Name)
                ) && 
                (
                    this.PhotoUrls == other.PhotoUrls ||
                    this.PhotoUrls != null &&
                    this.PhotoUrls.SequenceEqual(other.PhotoUrls)
                ) && 
                (
                    this.Tags == other.Tags ||
                    this.Tags != null &&
                    this.Tags.SequenceEqual(other.Tags)
                ) && 
                (
                    this.Status == other.Status ||
                    this.Status != null &&
                    this.Status.Equals(other.Status)
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
                
                    if (this.Category != null)
                    hash = hash * 59 + this.Category.GetHashCode();
                
                    if (this.Name != null)
                    hash = hash * 59 + this.Name.GetHashCode();
                
                    if (this.PhotoUrls != null)
                    hash = hash * 59 + this.PhotoUrls.GetHashCode();
                
                    if (this.Tags != null)
                    hash = hash * 59 + this.Tags.GetHashCode();
                
                    if (this.Status != null)
                    hash = hash * 59 + this.Status.GetHashCode();
                
                return hash;
            }
        }

        #region Operators

        public static bool operator ==(Pet left, Pet right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(Pet left, Pet right)
        {
            return !Equals(left, right);
        }

        #endregion Operators

    }
}
