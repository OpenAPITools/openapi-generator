using System;
using System.Linq;
using System.IO;
using System.Text;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Runtime.Serialization;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;

namespace IO.Swagger.Model
{
        public enum EnumClass {
            
            [EnumMember(Value = "_abc")]
            Abc,
            
            [EnumMember(Value = "-efg")]
            efg,
            
            [EnumMember(Value = "(xyz)")]
            xyz
        }

}
