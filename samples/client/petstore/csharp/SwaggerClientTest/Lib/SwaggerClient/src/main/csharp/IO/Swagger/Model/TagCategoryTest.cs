using System;
using System.Linq;
using System.IO;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.Serialization;
using Newtonsoft.Json;

namespace IO.Swagger.Model
{

    /// <summary>
    /// 
    /// </summary>
    [DataContract]
    public class TagCategoryTest : Category,  IEquatable<TagCategoryTest>
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="TagCategoryTest" /> class.
        /// </summary>
        public TagCategoryTest()
        {
            this.TagCategoryInteger = 0;
            
        }

        
        /// <summary>
        /// Gets or Sets Id
        /// </summary>
        [DataMember(Name="id", EmitDefaultValue=false)]
        public long? Id { get; set; }
  
        
        /// <summary>
        /// integer property for testing allOf
        /// </summary>
        /// <value>integer property for testing allOf</value>
        [DataMember(Name="TagCategoryInteger", EmitDefaultValue=false)]
        public int? TagCategoryInteger { get; set; }
  
        
        /// <summary>
        /// Gets or Sets Name
        /// </summary>
        [DataMember(Name="name", EmitDefaultValue=false)]
        public string Name { get; set; }
  
        
  
        /// <summary>
        /// Returns the string presentation of the object
        /// </summary>
        /// <returns>String presentation of the object</returns>
        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("class TagCategoryTest {\n");
            sb.Append("  Id: ").Append(Id).Append("\n");
            sb.Append("  TagCategoryInteger: ").Append(TagCategoryInteger).Append("\n");
            sb.Append("  Name: ").Append(Name).Append("\n");
            
            sb.Append("}\n");
            return sb.ToString();
        }
  
        /// <summary>
        /// Returns the JSON string presentation of the object
        /// </summary>
        /// <returns>JSON string presentation of the object</returns>
        public  new string ToJson()
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
            // credit: http://stackoverflow.com/a/10454552/677735
            return this.Equals(obj as TagCategoryTest);
        }

        /// <summary>
        /// Returns true if TagCategoryTest instances are equal
        /// </summary>
        /// <param name="other">Instance of TagCategoryTest to be compared</param>
        /// <returns>Boolean</returns>
        public bool Equals(TagCategoryTest other)
        {
            // credit: http://stackoverflow.com/a/10454552/677735
            if (other == null)
                return false;

            return 
                (
                    this.Id == other.Id ||
                    this.Id != null &&
                    this.Id.Equals(other.Id)
                ) && 
                (
                    this.TagCategoryInteger == other.TagCategoryInteger ||
                    this.TagCategoryInteger != null &&
                    this.TagCategoryInteger.Equals(other.TagCategoryInteger)
                ) && 
                (
                    this.Name == other.Name ||
                    this.Name != null &&
                    this.Name.Equals(other.Name)
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
                    hash = hash * 57 + this.Id.GetHashCode();
                
                if (this.TagCategoryInteger != null)
                    hash = hash * 57 + this.TagCategoryInteger.GetHashCode();
                
                if (this.Name != null)
                    hash = hash * 57 + this.Name.GetHashCode();
                
                return hash;
            }
        }

    }
}
