using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Nancy;
using NodaTime;
using NodaTime.Text;
using Sharpility.Base;
using Sharpility.Extensions;
using Sharpility.Util;

namespace IO.Swagger.v2.Utils
{
    internal static class Parameters
    {
        private static readonly IDictionary<Type, Func<Parameter, object>> Parsers = CreateParsers();

        internal static TValue ValueOf<TValue>(dynamic parameters, Request request, string name, ParameterType parameterType)
        {
            var valueType = typeof(TValue);
            var valueUnderlyingType = Nullable.GetUnderlyingType(valueType);
            var isNullable = default(TValue) == null;
            string value = RawValueOf(parameters, request, name, parameterType);
            Preconditions.Evaluate(!string.IsNullOrEmpty(value) || isNullable, string.Format("Required parameter: '{0}' is missing", name));
            if (value == null && isNullable)
            {
                return default(TValue);
            }
            if (valueType.IsEnum || (valueUnderlyingType != null && valueUnderlyingType.IsEnum))
            {
                return EnumValueOf<TValue>(name, value);
            }
            return ValueOf<TValue>(parameters, name, value, valueType, request, parameterType);
        }

        private static string RawValueOf(dynamic parameters, Request request, string name, ParameterType parameterType)
        {
            try
            {
                switch (parameterType)
                {
                    case ParameterType.Query:
                        string querValue = request.Query[name];
                        return querValue;
                    case ParameterType.Path:
                        string pathValue = parameters[name];
                        return pathValue;
                    case ParameterType.Header:
                        var headerValue = request.Headers[name];
                        return headerValue != null ? string.Join(",", headerValue) : null;
                }
            }
            catch (Exception e)
            {
                throw new InvalidOperationException(string.Format("Could not obtain value of '{0}' parameter", name), e);
            }
            throw new InvalidOperationException(string.Format("Parameter with type: {0} is not supported", parameterType));
        }

        private static TValue EnumValueOf<TValue>(string name, string value)
        {
            var valueType = typeof(TValue);
            var enumType = valueType.IsEnum ? valueType : Nullable.GetUnderlyingType(valueType);
            Preconditions.IsNotNull(enumType, () => new InvalidOperationException(
                string.Format("Could not parse parameter: '{0}' to enum. Type {1} is not enum", name, valueType)));
            var values = Enum.GetValues(enumType);
            foreach (var entry in values)
            {
                if (entry.ToString().EqualsIgnoreCases(value)
                    || ((int)entry).ToString().EqualsIgnoreCases(value))
                {
                    return (TValue)entry;
                }
            }
            throw new ArgumentException(string.Format("Parameter: '{0}' value: '{1}' is not supported. Expected one of: {2}",
                    name, value, Strings.ToString(values)));
        }

        private static TValue ValueOf<TValue>(dynamic parameters, string name, string value, Type valueType, Request request, ParameterType parameterType)
        {
            var parser = Parsers.GetIfPresent(valueType);
            if (parser != null)
            {
                return ParseValueUsing<TValue>(name, value, valueType, parser);
            }
            if (parameterType == ParameterType.Path)
            {
                return DynamicValueOf<TValue>(parameters, name);
            }
            if (parameterType == ParameterType.Query)
            {
                return DynamicValueOf<TValue>(request.Query, name);
            }
            throw new InvalidOperationException(string.Format("Could not get value for {0} with type {1}", name, valueType));
        }

        private static TValue ParseValueUsing<TValue>(string name, string value, Type valueType, Func<Parameter, object> parser)
        {
            var result = parser(Parameter.Of(name, value));
            try
            {
                return (TValue)result;
            }
            catch (InvalidCastException)
            {
                throw new InvalidOperationException(
                    string.Format("Could not parse parameter: '{0}' with value: '{1}'. " +
                                  "Received: '{2}', expected: '{3}'.",
                                  name, value, result.GetType(), valueType));
            }
        }

        private static TValue DynamicValueOf<TValue>(dynamic parameters, string name)
        {
            string value = parameters[name];
            try
            {
                TValue result = parameters[name];
                return result;
            }
            catch (InvalidCastException)
            {
                throw new InvalidOperationException(Strings.Format("Parameter: '{0}' value: '{1}' could not be parsed. " +
                                                                  "Expected type: '{2}' is not supported",
                                                                  name, value, typeof(TValue)));
            }
            catch (Exception e)
            {
                throw new InvalidOperationException(string.Format("Could not get '{0}' value of '{1}' type dynamicly",
                    name, typeof(TValue)), e);
            }
        }

        private static IDictionary<Type, Func<Parameter, object>> CreateParsers()
        {
            var parsers = ImmutableDictionary.CreateBuilder<Type, Func<Parameter, object>>();
            parsers.Put(typeof(string), value => value.Value);
            parsers.Put(typeof(bool), SafeParse(bool.Parse));
            parsers.Put(typeof(bool?), SafeParse(bool.Parse));
            parsers.Put(typeof(byte), SafeParse(byte.Parse));
            parsers.Put(typeof(sbyte?), SafeParse(sbyte.Parse));
            parsers.Put(typeof(short), SafeParse(short.Parse));
            parsers.Put(typeof(short?), SafeParse(short.Parse));
            parsers.Put(typeof(ushort), SafeParse(ushort.Parse));
            parsers.Put(typeof(ushort?), SafeParse(ushort.Parse));
            parsers.Put(typeof(int), SafeParse(int.Parse));
            parsers.Put(typeof(int?), SafeParse(int.Parse));
            parsers.Put(typeof(uint), SafeParse(uint.Parse));
            parsers.Put(typeof(uint?), SafeParse(uint.Parse));
            parsers.Put(typeof(long), SafeParse(long.Parse));
            parsers.Put(typeof(long?), SafeParse(long.Parse));
            parsers.Put(typeof(ulong), SafeParse(ulong.Parse));
            parsers.Put(typeof(ulong?), SafeParse(ulong.Parse));
            parsers.Put(typeof(float), SafeParse(float.Parse));
            parsers.Put(typeof(float?), SafeParse(float.Parse));
            parsers.Put(typeof(double), SafeParse(double.Parse));
            parsers.Put(typeof(double?), SafeParse(double.Parse));
            parsers.Put(typeof(decimal), SafeParse(decimal.Parse));
            parsers.Put(typeof(decimal?), SafeParse(decimal.Parse));
            parsers.Put(typeof(DateTime), SafeParse(DateTime.Parse));
            parsers.Put(typeof(DateTime?), SafeParse(DateTime.Parse));
            parsers.Put(typeof(TimeSpan), SafeParse(TimeSpan.Parse));
            parsers.Put(typeof(TimeSpan?), SafeParse(TimeSpan.Parse));
            parsers.Put(typeof(ZonedDateTime), SafeParse(ParseZonedDateTime));
            parsers.Put(typeof(ZonedDateTime?), SafeParse(ParseZonedDateTime));
            parsers.Put(typeof(LocalTime), SafeParse(ParseLocalTime));
            parsers.Put(typeof(LocalTime?), SafeParse(ParseLocalTime));

            parsers.Put(typeof(IEnumerable<string>), ImmutableListParse(value => value));
            parsers.Put(typeof(ICollection<string>), ImmutableListParse(value => value));
            parsers.Put(typeof(IList<string>), ImmutableListParse(value => value));
            parsers.Put(typeof(List<string>), ListParse(value => value));
            parsers.Put(typeof(ISet<string>), ImmutableListParse(value => value));
            parsers.Put(typeof(HashSet<string>), SetParse(value => value));

            parsers.Put(typeof(IEnumerable<bool?>), NullableImmutableListParse(bool.Parse));
            parsers.Put(typeof(ICollection<bool?>), NullableImmutableListParse(bool.Parse));
            parsers.Put(typeof(IList<bool?>), NullableImmutableListParse(bool.Parse));
            parsers.Put(typeof(List<bool?>), NullableListParse(bool.Parse));
            parsers.Put(typeof(ISet<bool?>), NullableImmutableSetParse(bool.Parse));
            parsers.Put(typeof(HashSet<bool?>), NullableSetParse(bool.Parse));

            parsers.Put(typeof(IEnumerable<byte>), ImmutableListParse(byte.Parse));
            parsers.Put(typeof(ICollection<byte>), ImmutableListParse(byte.Parse));
            parsers.Put(typeof(IList<byte>), ImmutableListParse(byte.Parse));
            parsers.Put(typeof(List<byte>), ListParse(byte.Parse));
            parsers.Put(typeof(ISet<byte>), ImmutableSetParse(byte.Parse));
            parsers.Put(typeof(HashSet<byte>), SetParse(byte.Parse));

            parsers.Put(typeof(IEnumerable<sbyte>), ImmutableListParse(sbyte.Parse));
            parsers.Put(typeof(ICollection<sbyte>), ImmutableListParse(sbyte.Parse));
            parsers.Put(typeof(IList<sbyte>), ImmutableListParse(sbyte.Parse));
            parsers.Put(typeof(List<sbyte>), ListParse(sbyte.Parse));
            parsers.Put(typeof(ISet<sbyte>), ImmutableSetParse(sbyte.Parse));
            parsers.Put(typeof(HashSet<sbyte>), SetParse(sbyte.Parse));

            parsers.Put(typeof(IEnumerable<short>), ImmutableListParse(short.Parse));
            parsers.Put(typeof(ICollection<short>), ImmutableListParse(short.Parse));
            parsers.Put(typeof(IList<short>), ImmutableListParse(short.Parse));
            parsers.Put(typeof(List<short>), ListParse(short.Parse));
            parsers.Put(typeof(ISet<short>), ImmutableSetParse(short.Parse));
            parsers.Put(typeof(HashSet<short>), SetParse(short.Parse));

            parsers.Put(typeof(IEnumerable<ushort>), ImmutableListParse(ushort.Parse));
            parsers.Put(typeof(ICollection<ushort>), ImmutableListParse(ushort.Parse));
            parsers.Put(typeof(IList<ushort>), ImmutableListParse(ushort.Parse));
            parsers.Put(typeof(List<ushort>), ListParse(ushort.Parse));
            parsers.Put(typeof(ISet<ushort>), ImmutableSetParse(ushort.Parse));
            parsers.Put(typeof(HashSet<ushort>), SetParse(ushort.Parse));

            parsers.Put(typeof(IEnumerable<int?>), NullableImmutableListParse(int.Parse));
            parsers.Put(typeof(ICollection<int?>), NullableImmutableListParse(int.Parse));
            parsers.Put(typeof(IList<int?>), NullableImmutableListParse(int.Parse));
            parsers.Put(typeof(List<int?>), NullableListParse(int.Parse));
            parsers.Put(typeof(ISet<int?>), NullableImmutableSetParse(int.Parse));
            parsers.Put(typeof(HashSet<int?>), NullableSetParse(int.Parse));

            parsers.Put(typeof(IEnumerable<uint>), ImmutableListParse(uint.Parse));
            parsers.Put(typeof(ICollection<uint>), ImmutableListParse(uint.Parse));
            parsers.Put(typeof(IList<uint>), ImmutableListParse(uint.Parse));
            parsers.Put(typeof(List<uint>), ListParse(uint.Parse));
            parsers.Put(typeof(ISet<uint>), ImmutableSetParse(uint.Parse));
            parsers.Put(typeof(HashSet<uint>), SetParse(uint.Parse));

            parsers.Put(typeof(IEnumerable<long?>), NullableImmutableListParse(long.Parse));
            parsers.Put(typeof(ICollection<long?>), NullableImmutableListParse(long.Parse));
            parsers.Put(typeof(IList<long?>), NullableImmutableListParse(long.Parse));
            parsers.Put(typeof(List<long?>), NullableListParse(long.Parse));
            parsers.Put(typeof(ISet<long?>), NullableImmutableSetParse(long.Parse));
            parsers.Put(typeof(HashSet<long?>), NullableSetParse(long.Parse));

            parsers.Put(typeof(IEnumerable<ulong>), ImmutableListParse(ulong.Parse));
            parsers.Put(typeof(ICollection<ulong>), ImmutableListParse(ulong.Parse));
            parsers.Put(typeof(IList<ulong>), ImmutableListParse(ulong.Parse));
            parsers.Put(typeof(List<ulong>), ListParse(ulong.Parse));
            parsers.Put(typeof(ISet<ulong>), ImmutableSetParse(ulong.Parse));
            parsers.Put(typeof(HashSet<ulong>), SetParse(ulong.Parse));

            parsers.Put(typeof(IEnumerable<float?>), NullableImmutableListParse(float.Parse));
            parsers.Put(typeof(ICollection<float?>), NullableImmutableListParse(float.Parse));
            parsers.Put(typeof(IList<float?>), NullableImmutableListParse(float.Parse));
            parsers.Put(typeof(List<float?>), NullableListParse(float.Parse));
            parsers.Put(typeof(ISet<float?>), NullableImmutableSetParse(float.Parse));
            parsers.Put(typeof(HashSet<float?>), NullableSetParse(float.Parse));

            parsers.Put(typeof(IEnumerable<double?>), NullableImmutableListParse(double.Parse));
            parsers.Put(typeof(ICollection<double?>), NullableImmutableListParse(double.Parse));
            parsers.Put(typeof(IList<double?>), NullableImmutableListParse(double.Parse));
            parsers.Put(typeof(List<double?>), NullableListParse(double.Parse));
            parsers.Put(typeof(ISet<double?>), NullableImmutableSetParse(double.Parse));
            parsers.Put(typeof(HashSet<double?>), NullableSetParse(double.Parse));

            parsers.Put(typeof(IEnumerable<decimal?>), NullableImmutableListParse(decimal.Parse));
            parsers.Put(typeof(ICollection<decimal?>), NullableImmutableListParse(decimal.Parse));
            parsers.Put(typeof(IList<decimal?>), NullableImmutableListParse(decimal.Parse));
            parsers.Put(typeof(List<decimal?>), NullableListParse(decimal.Parse));
            parsers.Put(typeof(ISet<decimal?>), NullableImmutableSetParse(decimal.Parse));
            parsers.Put(typeof(HashSet<decimal?>), NullableSetParse(decimal.Parse));

            parsers.Put(typeof(IEnumerable<DateTime?>), NullableImmutableListParse(DateTime.Parse));
            parsers.Put(typeof(ICollection<DateTime?>), NullableImmutableListParse(DateTime.Parse));
            parsers.Put(typeof(IList<DateTime?>), NullableImmutableListParse(DateTime.Parse));
            parsers.Put(typeof(List<DateTime?>), NullableListParse(DateTime.Parse));
            parsers.Put(typeof(ISet<DateTime?>), NullableImmutableSetParse(DateTime.Parse));
            parsers.Put(typeof(HashSet<DateTime?>), NullableSetParse(DateTime.Parse));

            parsers.Put(typeof(IEnumerable<TimeSpan>), ImmutableListParse(TimeSpan.Parse));
            parsers.Put(typeof(ICollection<TimeSpan>), ImmutableListParse(TimeSpan.Parse));
            parsers.Put(typeof(IList<TimeSpan>), ImmutableListParse(TimeSpan.Parse));
            parsers.Put(typeof(List<TimeSpan>), ListParse(TimeSpan.Parse));
            parsers.Put(typeof(ISet<TimeSpan>), ImmutableSetParse(TimeSpan.Parse));
            parsers.Put(typeof(HashSet<TimeSpan>), SetParse(TimeSpan.Parse));

            return parsers.ToImmutableDictionary();
        }

        private static Func<Parameter, object> SafeParse<T>(Func<string, T> parse)
        {
            return parameter =>
            {
                try
                {
                    return parse(parameter.Value);
                }
                catch (OverflowException)
                {
                    throw ParameterOutOfRange(parameter, typeof(T));
                }
                catch (FormatException)
                {
                    throw InvalidParameterFormat(parameter, typeof(T));
                }
                catch (Exception e)
                {
                    throw new InvalidOperationException(Strings.Format("Unable to parse parameter: '{0}' with value: '{1}' to {2}",
                        parameter.Name, parameter.Value, typeof(T)), e);
                }
            };
        }

        private static Func<Parameter, object> NullableListParse<T>(Func<string, T> itemParser) where T: struct
        {
            return ListParse(it => it.ToNullable(itemParser));
        }

        private static Func<Parameter, object> ListParse<T>(Func<string, T> itemParser)
        {
            return parameter =>
            {
                if (string.IsNullOrEmpty(parameter.Value))
                {
                    return new List<T>();
                }
                return ParseCollection(parameter.Value, itemParser).ToList();
            };
        }

        private static Func<Parameter, object> NullableImmutableListParse<T>(Func<string, T> itemParser) where T: struct
        {
            return ImmutableListParse(it => it.ToNullable(itemParser));
        }

        private static Func<Parameter, object> ImmutableListParse<T>(Func<string, T> itemParser)
        {
            return parameter =>
            {
                if (string.IsNullOrEmpty(parameter.Value))
                {
                    return Lists.EmptyList<T>();
                }
                return ParseCollection(parameter.Value, itemParser).ToImmutableList();
            };
        }

        private static Func<Parameter, object> NullableSetParse<T>(Func<string, T> itemParser) where T: struct
        {
            return SetParse(it => it.ToNullable(itemParser));
        }

        private static Func<Parameter, object> SetParse<T>(Func<string, T> itemParser)
        {
            return parameter =>
            {
                if (string.IsNullOrEmpty(parameter.Value))
                {
                    return new HashSet<T>();
                }
                return ParseCollection(parameter.Value, itemParser).ToSet();
            };
        }

        private static Func<Parameter, object> NullableImmutableSetParse<T>(Func<string, T> itemParser) where T: struct
        {
            return ImmutableSetParse(it => it.ToNullable(itemParser));
        }

        private static Func<Parameter, object> ImmutableSetParse<T>(Func<string, T> itemParser)
        {
            return parameter =>
            {
                if (string.IsNullOrEmpty(parameter.Value))
                {
                    return Sets.EmptySet<T>();
                }
                return ParseCollection(parameter.Value, itemParser).ToImmutableHashSet();
            };
        }

        private static ZonedDateTime ParseZonedDateTime(string value)
        {
            var dateTime = DateTime.Parse(value);
            return new ZonedDateTime(Instant.FromDateTimeUtc(dateTime.ToUniversalTime()), DateTimeZone.Utc);
        }

        private static LocalTime ParseLocalTime(string value)
        {
            return LocalTimePattern.ExtendedIsoPattern.Parse(value).Value;
        }

        private static ArgumentException ParameterOutOfRange(Parameter parameter, Type type)
        {
            return new ArgumentException(Strings.Format("Query: '{0}' value: '{1}' is out of range for: '{2}'",
                parameter.Name, parameter.Value, type));
        }

        private static ArgumentException InvalidParameterFormat(Parameter parameter, Type type)
        {
            return new ArgumentException(Strings.Format("Query '{0}' value: '{1}' format is invalid for: '{2}'",
                parameter.Name, parameter.Value, type));
        }

        private static IEnumerable<T> ParseCollection<T>(string value, Func<string, T> itemParser)
        {
            var results = value.Split(new[] { ',' }, StringSplitOptions.None)
                        .Where(it => it != null)
                        .Select(it => it.Trim())
                        .Select(itemParser);
            return results;
        }

        public static T? ToNullable<T>(this string s, Func<string, T> itemParser) where T : struct
        {
            T? result = new T?();
            try
            {
                if (!string.IsNullOrEmpty(s) && s.Trim().Length > 0)
                {
                    result = itemParser(s);
                }
            }
            catch (Exception e)
            {
                throw new InvalidOperationException(Strings.Format("Unable to parse value: '{0}' to nullable: '{1}'", s, typeof(T).ToString()), e);
            }
            return result;
        }

        private class Parameter
        {
            internal string Name { get; private set; }
            internal string Value { get; private set; }

            private Parameter(string name, string value)
            {
                Name = name;
                Value = value;
            }

            internal static Parameter Of(string name, string value)
            {
                return new Parameter(name, value);
            }
        }
    }

    internal enum ParameterType
    {
        Undefined,
        Query,
        Path,
        Header
    }
}
