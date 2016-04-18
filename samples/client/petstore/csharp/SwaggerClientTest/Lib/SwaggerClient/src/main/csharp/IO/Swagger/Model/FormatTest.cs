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
    public partial class FormatTest :  IEquatable<FormatTest>
    { 
    
        /// <summary>
        /// Initializes a new instance of the <see cref="FormatTest" /> class.
        /// Initializes a new instance of the <see cref="FormatTest" />class.
        /// </summary>
        /// <param name="Integer">Integer.</param>
        /// <param name="Int32">Int32.</param>
        /// <param name="Int64">Int64.</param>
        /// <param name="Number">Number (required).</param>
        /// <param name="_Float">_Float.</param>
        /// <param name="_Double">_Double.</param>
        /// <param name="_String">_String.</param>
        /// <param name="_Byte">_Byte.</param>
        /// <param name="Binary">Binary.</param>
        /// <param name="Date">Date.</param>
        /// <param name="DateTime">DateTime.</param>
        /// <param name="Password">Password.</param>

        public FormatTest(int? Integer = null, int? Int32 = null, long? Int64 = null, double? Number = null, float? _Float = null, double? _Double = null, string _String = null, byte[] _Byte = null, byte[] Binary = null, DateTime? Date = null, DateTime? DateTime = null, string Password = null)
        {
            // to ensure "Number" is required (not null)
            if (Number == null)
            {
                throw new InvalidDataException("Number is a required property for FormatTest and cannot be null");
            }
            else
            {
                this.Number = Number;
            }
            this.Integer = Integer;
            this.Int32 = Int32;
            this.Int64 = Int64;
            this._Float = _Float;
            this._Double = _Double;
            this._String = _String;
            this._Byte = _Byte;
            this.Binary = Binary;
            this.Date = Date;
            this.DateTime = DateTime;
            this.Password = Password;
            
        }

    
        /// <summary>
        /// Gets or Sets Integer
        /// </summary>
        [DataMember(Name="integer", EmitDefaultValue=false)]
        public int? Integer { get; set; }
    
        /// <summary>
        /// Gets or Sets Int32
        /// </summary>
        [DataMember(Name="int32", EmitDefaultValue=false)]
        public int? Int32 { get; set; }
    
        /// <summary>
        /// Gets or Sets Int64
        /// </summary>
        [DataMember(Name="int64", EmitDefaultValue=false)]
        public long? Int64 { get; set; }
    
        /// <summary>
        /// Gets or Sets Number
        /// </summary>
        [DataMember(Name="number", EmitDefaultValue=false)]
        public double? Number { get; set; }
    
        /// <summary>
        /// Gets or Sets _Float
        /// </summary>
        [DataMember(Name="float", EmitDefaultValue=false)]
        public float? _Float { get; set; }
    
        /// <summary>
        /// Gets or Sets _Double
        /// </summary>
        [DataMember(Name="double", EmitDefaultValue=false)]
        public double? _Double { get; set; }
    
        /// <summary>
        /// Gets or Sets _String
        /// </summary>
        [DataMember(Name="string", EmitDefaultValue=false)]
        public string _String { get; set; }
    
        /// <summary>
        /// Gets or Sets _Byte
        /// </summary>
        [DataMember(Name="byte", EmitDefaultValue=false)]
        public byte[] _Byte { get; set; }
    
        /// <summary>
        /// Gets or Sets Binary
        /// </summary>
        [DataMember(Name="binary", EmitDefaultValue=false)]
        public byte[] Binary { get; set; }
    
        /// <summary>
        /// Gets or Sets Date
        /// </summary>
        [DataMember(Name="date", EmitDefaultValue=false)]
        public DateTime? Date { get; set; }
    
        /// <summary>
        /// Gets or Sets DateTime
        /// </summary>
        [DataMember(Name="dateTime", EmitDefaultValue=false)]
        public DateTime? DateTime { get; set; }
    
        /// <summary>
        /// Gets or Sets Password
        /// </summary>
        [DataMember(Name="password", EmitDefaultValue=false)]
        public string Password { get; set; }
    
        /// <summary>
        /// Returns the string presentation of the object
        /// </summary>
        /// <returns>String presentation of the object</returns>
        public override string ToString()
        {
            var sb = new StringBuilder();
            sb.Append("class FormatTest {\n");
            sb.Append("  Integer: ").Append(Integer).Append("\n");
            sb.Append("  Int32: ").Append(Int32).Append("\n");
            sb.Append("  Int64: ").Append(Int64).Append("\n");
            sb.Append("  Number: ").Append(Number).Append("\n");
            sb.Append("  _Float: ").Append(_Float).Append("\n");
            sb.Append("  _Double: ").Append(_Double).Append("\n");
            sb.Append("  _String: ").Append(_String).Append("\n");
            sb.Append("  _Byte: ").Append(_Byte).Append("\n");
            sb.Append("  Binary: ").Append(Binary).Append("\n");
            sb.Append("  Date: ").Append(Date).Append("\n");
            sb.Append("  DateTime: ").Append(DateTime).Append("\n");
            sb.Append("  Password: ").Append(Password).Append("\n");
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
            return this.Equals(obj as FormatTest);
        }

        /// <summary>
        /// Returns true if FormatTest instances are equal
        /// </summary>
        /// <param name="other">Instance of FormatTest to be compared</param>
        /// <returns>Boolean</returns>
        public bool Equals(FormatTest other)
        {
            // credit: http://stackoverflow.com/a/10454552/677735
            if (other == null)
                return false;

            return 
                (
                    this.Integer == other.Integer ||
                    this.Integer != null &&
                    this.Integer.Equals(other.Integer)
                ) && 
                (
                    this.Int32 == other.Int32 ||
                    this.Int32 != null &&
                    this.Int32.Equals(other.Int32)
                ) && 
                (
                    this.Int64 == other.Int64 ||
                    this.Int64 != null &&
                    this.Int64.Equals(other.Int64)
                ) && 
                (
                    this.Number == other.Number ||
                    this.Number != null &&
                    this.Number.Equals(other.Number)
                ) && 
                (
                    this._Float == other._Float ||
                    this._Float != null &&
                    this._Float.Equals(other._Float)
                ) && 
                (
                    this._Double == other._Double ||
                    this._Double != null &&
                    this._Double.Equals(other._Double)
                ) && 
                (
                    this._String == other._String ||
                    this._String != null &&
                    this._String.Equals(other._String)
                ) && 
                (
                    this._Byte == other._Byte ||
                    this._Byte != null &&
                    this._Byte.Equals(other._Byte)
                ) && 
                (
                    this.Binary == other.Binary ||
                    this.Binary != null &&
                    this.Binary.Equals(other.Binary)
                ) && 
                (
                    this.Date == other.Date ||
                    this.Date != null &&
                    this.Date.Equals(other.Date)
                ) && 
                (
                    this.DateTime == other.DateTime ||
                    this.DateTime != null &&
                    this.DateTime.Equals(other.DateTime)
                ) && 
                (
                    this.Password == other.Password ||
                    this.Password != null &&
                    this.Password.Equals(other.Password)
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
                if (this.Integer != null)
                    hash = hash * 59 + this.Integer.GetHashCode();
                if (this.Int32 != null)
                    hash = hash * 59 + this.Int32.GetHashCode();
                if (this.Int64 != null)
                    hash = hash * 59 + this.Int64.GetHashCode();
                if (this.Number != null)
                    hash = hash * 59 + this.Number.GetHashCode();
                if (this._Float != null)
                    hash = hash * 59 + this._Float.GetHashCode();
                if (this._Double != null)
                    hash = hash * 59 + this._Double.GetHashCode();
                if (this._String != null)
                    hash = hash * 59 + this._String.GetHashCode();
                if (this._Byte != null)
                    hash = hash * 59 + this._Byte.GetHashCode();
                if (this.Binary != null)
                    hash = hash * 59 + this.Binary.GetHashCode();
                if (this.Date != null)
                    hash = hash * 59 + this.Date.GetHashCode();
                if (this.DateTime != null)
                    hash = hash * 59 + this.DateTime.GetHashCode();
                if (this.Password != null)
                    hash = hash * 59 + this.Password.GetHashCode();
                return hash;
            }
        }

    }
}
