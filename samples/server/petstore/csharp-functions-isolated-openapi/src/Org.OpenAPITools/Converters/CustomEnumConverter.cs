using System;
using System.ComponentModel;
using System.Globalization;
using System.Text.Json;

namespace Org.OpenAPITools.Converters
{
    /// <summary>
    /// Custom string to enum converter that defers to the configured JSON serializer so that
    /// the enum's wire values are honored.
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

            return JsonSerializer.Deserialize<T>(JsonSerializer.Serialize(s));
        }
    }
}
