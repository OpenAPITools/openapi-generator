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
    public class CatDogTest : Cat,  IEquatable<CatDogTest>
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="CatDogTest" /> class.
        /// </summary>
        public CatDogTest()
        {
            this.CatDogInteger = 0;
            
        }

        
        /// <summary>
        /// Gets or Sets CatId
        /// </summary>
        [DataMember(Name="cat_id", EmitDefaultValue=false)]
        public long? CatId { get; set; }
  
        
        /// <summary>
        /// Gets or Sets DogId
        /// </summary>
        [DataMember(Name="dog_id", EmitDefaultValue=false)]
        public long? DogId { get; set; }
  
        
        /// <summary>
        /// integer property for testing allOf
        /// </summary>
        /// <value>integer property for testing allOf</value>
        [DataMember(Name="CatDogInteger", EmitDefaultValue=false)]
        public int? CatDogInteger { get; set; }
  
        
        /// <summary>
        /// Gets or Sets DogName
        /// </summary>
        [DataMember(Name="dog_name", EmitDefaultValue=false)]
        public string DogName { get; set; }
  
        
        /// <summary>
        /// Gets or Sets CatName
        /// </summary>
        [DataMember(Name="cat_name", EmitDefaultValue=false)]
        public string CatName { get; set; }
  
        
  
        /// <summary>
        /// Returns the string presentation of the object
        /// </summary>
        /// <returns>String presentation of the object</returns>
        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("class CatDogTest {\n");
            sb.Append("  CatId: ").Append(CatId).Append("\n");
            sb.Append("  DogId: ").Append(DogId).Append("\n");
            sb.Append("  CatDogInteger: ").Append(CatDogInteger).Append("\n");
            sb.Append("  DogName: ").Append(DogName).Append("\n");
            sb.Append("  CatName: ").Append(CatName).Append("\n");
            
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
            return this.Equals(obj as CatDogTest);
        }

        /// <summary>
        /// Returns true if CatDogTest instances are equal
        /// </summary>
        /// <param name="other">Instance of CatDogTest to be compared</param>
        /// <returns>Boolean</returns>
        public bool Equals(CatDogTest other)
        {
            // credit: http://stackoverflow.com/a/10454552/677735
            if (other == null)
                return false;

            return 
                (
                    this.CatId == other.CatId ||
                    this.CatId != null &&
                    this.CatId.Equals(other.CatId)
                ) && 
                (
                    this.DogId == other.DogId ||
                    this.DogId != null &&
                    this.DogId.Equals(other.DogId)
                ) && 
                (
                    this.CatDogInteger == other.CatDogInteger ||
                    this.CatDogInteger != null &&
                    this.CatDogInteger.Equals(other.CatDogInteger)
                ) && 
                (
                    this.DogName == other.DogName ||
                    this.DogName != null &&
                    this.DogName.Equals(other.DogName)
                ) && 
                (
                    this.CatName == other.CatName ||
                    this.CatName != null &&
                    this.CatName.Equals(other.CatName)
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
                
                if (this.CatId != null)
                    hash = hash * 57 + this.CatId.GetHashCode();
                
                if (this.DogId != null)
                    hash = hash * 57 + this.DogId.GetHashCode();
                
                if (this.CatDogInteger != null)
                    hash = hash * 57 + this.CatDogInteger.GetHashCode();
                
                if (this.DogName != null)
                    hash = hash * 57 + this.DogName.GetHashCode();
                
                if (this.CatName != null)
                    hash = hash * 57 + this.CatName.GetHashCode();
                
                return hash;
            }
        }

    }
}
