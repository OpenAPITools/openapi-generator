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
    /// Gets or Sets EnumClass
    /// </summary>
    [JsonConverter(typeof(StringEnumConverter))]
    public enum EnumClass
    {
        
        /// <summary>
        /// Enum Abc for "_abc"
        /// </summary>
        [EnumMember(Value = "_abc")]
        Abc,
        
        /// <summary>
        /// Enum efg for "-efg"
        /// </summary>
        [EnumMember(Value = "-efg")]
        efg,
        
        /// <summary>
        /// Enum xyz for "(xyz)"
        /// </summary>
        [EnumMember(Value = "(xyz)")]
        xyz
    }

}
