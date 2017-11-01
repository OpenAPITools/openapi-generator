using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// Describes the result of uploading an image resource
    /// </summary>
    public sealed class ApiResponse:  IEquatable<ApiResponse>
    { 
        /// <summary>
        /// Code
        /// </summary>
        public int? Code { get; private set; }

        /// <summary>
        /// Type
        /// </summary>
        public string Type { get; private set; }

        /// <summary>
        /// Message
        /// </summary>
        public string Message { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use ApiResponse.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public ApiResponse()
        {
        }

        private ApiResponse(int? Code, string Type, string Message)
        {
            
            this.Code = Code;
            
            this.Type = Type;
            
            this.Message = Message;
            
        }

        /// <summary>
        /// Returns builder of ApiResponse.
        /// </summary>
        /// <returns>ApiResponseBuilder</returns>
        public static ApiResponseBuilder Builder()
        {
            return new ApiResponseBuilder();
        }

        /// <summary>
        /// Returns ApiResponseBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>ApiResponseBuilder</returns>
        public ApiResponseBuilder With()
        {
            return Builder()
                .Code(Code)
                .Type(Type)
                .Message(Message);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(ApiResponse other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (ApiResponse.
        /// </summary>
        /// <param name="left">Compared (ApiResponse</param>
        /// <param name="right">Compared (ApiResponse</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (ApiResponse left, ApiResponse right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (ApiResponse.
        /// </summary>
        /// <param name="left">Compared (ApiResponse</param>
        /// <param name="right">Compared (ApiResponse</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (ApiResponse left, ApiResponse right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of ApiResponse.
        /// </summary>
        public sealed class ApiResponseBuilder
        {
            private int? _Code;
            private string _Type;
            private string _Message;

            internal ApiResponseBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for ApiResponse.Code property.
            /// </summary>
            /// <param name="value">Code</param>
            public ApiResponseBuilder Code(int? value)
            {
                _Code = value;
                return this;
            }

            /// <summary>
            /// Sets value for ApiResponse.Type property.
            /// </summary>
            /// <param name="value">Type</param>
            public ApiResponseBuilder Type(string value)
            {
                _Type = value;
                return this;
            }

            /// <summary>
            /// Sets value for ApiResponse.Message property.
            /// </summary>
            /// <param name="value">Message</param>
            public ApiResponseBuilder Message(string value)
            {
                _Message = value;
                return this;
            }


            /// <summary>
            /// Builds instance of ApiResponse.
            /// </summary>
            /// <returns>ApiResponse</returns>
            public ApiResponse Build()
            {
                Validate();
                return new ApiResponse(
                    Code: _Code,
                    Type: _Type,
                    Message: _Message
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}