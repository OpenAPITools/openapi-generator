using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// A tag for a pet
    /// </summary>
    public sealed class Tag:  IEquatable<Tag>
    { 
        /// <summary>
        /// Id
        /// </summary>
        public long? Id { get; private set; }

        /// <summary>
        /// Name
        /// </summary>
        public string Name { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Tag.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Tag()
        {
        }

        private Tag(long? Id, string Name)
        {
            
            this.Id = Id;
            
            this.Name = Name;
            
        }

        /// <summary>
        /// Returns builder of Tag.
        /// </summary>
        /// <returns>TagBuilder</returns>
        public static TagBuilder Builder()
        {
            return new TagBuilder();
        }

        /// <summary>
        /// Returns TagBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>TagBuilder</returns>
        public TagBuilder With()
        {
            return Builder()
                .Id(Id)
                .Name(Name);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Tag other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Tag.
        /// </summary>
        /// <param name="left">Compared (Tag</param>
        /// <param name="right">Compared (Tag</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Tag left, Tag right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Tag.
        /// </summary>
        /// <param name="left">Compared (Tag</param>
        /// <param name="right">Compared (Tag</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Tag left, Tag right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Tag.
        /// </summary>
        public sealed class TagBuilder
        {
            private long? _Id;
            private string _Name;

            internal TagBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Tag.Id property.
            /// </summary>
            /// <param name="value">Id</param>
            public TagBuilder Id(long? value)
            {
                _Id = value;
                return this;
            }

            /// <summary>
            /// Sets value for Tag.Name property.
            /// </summary>
            /// <param name="value">Name</param>
            public TagBuilder Name(string value)
            {
                _Name = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Tag.
            /// </summary>
            /// <returns>Tag</returns>
            public Tag Build()
            {
                Validate();
                return new Tag(
                    Id: _Id,
                    Name: _Name
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}
