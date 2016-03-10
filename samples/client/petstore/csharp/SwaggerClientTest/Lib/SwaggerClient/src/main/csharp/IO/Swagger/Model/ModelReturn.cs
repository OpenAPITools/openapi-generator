using System;
using System.Linq;
using System.IO;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Runtime.Serialization;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;

namespace IO.Swagger.Model
{
    /// <summary>
    /// 
    /// </summary>
    [DataContract]
    public partial class ModelReturn :  IEquatable<ModelReturn>
    { 
    
        /// <summary>
        /// Initializes a new instance of the <see cref="ModelReturn" /> class.
        /// Initializes a new instance of the <see cref="ModelReturn" />class.
        /// </summary>
        /// <param name="_Return">_Return.</param>

        public ModelReturn(int? _Return = null)
        {
            this._Return = _Return;
            
        }
        
    
        /// <summary>
        /// Gets or Sets _Return
        /// </summary>
        [DataMember(Name="return", EmitDefaultValue=false)]
        public int? _Return { get; set; }
    
        /// <summary>
        /// Returns the string presentation of the object
        /// </summary>
        /// <returns>String presentation of the object</returns>
        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("class ModelReturn {\n");
            sb.Append("  _Return: ").Append(_Return).Append("\n");
            
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
            // credit: http://stackoverflow.com/a/10454552/677735
            return this.Equals(obj as ModelReturn);
        }

        /// <summary>
        /// Returns true if ModelReturn instances are equal
        /// </summary>
        /// <param name="other">Instance of ModelReturn to be compared</param>
        /// <returns>Boolean</returns>
        public bool Equals(ModelReturn other)
        {
            // credit: http://stackoverflow.com/a/10454552/677735
            if (other == null)
                return false;

            return 
                (
                    this._Return == other._Return ||
                    this._Return != null &&
                    this._Return.Equals(other._Return)
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
                
                if (this._Return != null)
                    hash = hash * 59 + this._Return.GetHashCode();
                
                return hash;
            }
        }

    }
}
