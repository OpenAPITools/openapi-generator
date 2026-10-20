using System;
using System.ComponentModel;
using System.Globalization;
using Newtonsoft.Json;

namespace Org.OpenAPITools.Converters
{
    /// <summary>
    /// Custom string to enum converter that round-trips through the JSON serializer so that the
    /// enum's wire values (declared via [JsonStringEnumMemberName] / [EnumMember]) are honored.
    /// The generated enum types carry their own [JsonConverter] attribute, which takes precedence
    /// over any converters registered on JsonSerializerOptions, so the default serializer options
    /// used here still produce the correct mapping.
    /// </summary>
    public class CustomEnumConverter<T> : TypeConverter
    {
        public override bool CanConvertFrom(ITypeDescriptorContext context, Type sourceType)
        {
            if (sourceType == typeof(string))
            {
                return true;
            }
            return base.CanConvertFrom(context, sourceType);
        }

        public override object ConvertFrom(ITypeDescriptorContext context, CultureInfo culture, object value)
        {
            var s = value as string;
            if (string.IsNullOrEmpty(s))
            {
                return null;
            }

            return JsonConvert.DeserializeObject<T>(JsonConvert.SerializeObject(s));
        }
    }
}
