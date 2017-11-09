using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// A category for a pet
    /// </summary>
    public sealed class Category:  IEquatable<Category>
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
        /// Use Category.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Category()
        {
        }

        private Category(long? Id, string Name)
        {
            
            this.Id = Id;
            
            this.Name = Name;
            
        }

        /// <summary>
        /// Returns builder of Category.
        /// </summary>
        /// <returns>CategoryBuilder</returns>
        public static CategoryBuilder Builder()
        {
            return new CategoryBuilder();
        }

        /// <summary>
        /// Returns CategoryBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>CategoryBuilder</returns>
        public CategoryBuilder With()
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

        public bool Equals(Category other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Category.
        /// </summary>
        /// <param name="left">Compared (Category</param>
        /// <param name="right">Compared (Category</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Category left, Category right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Category.
        /// </summary>
        /// <param name="left">Compared (Category</param>
        /// <param name="right">Compared (Category</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Category left, Category right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Category.
        /// </summary>
        public sealed class CategoryBuilder
        {
            private long? _Id;
            private string _Name;

            internal CategoryBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Category.Id property.
            /// </summary>
            /// <param name="value">Id</param>
            public CategoryBuilder Id(long? value)
            {
                _Id = value;
                return this;
            }

            /// <summary>
            /// Sets value for Category.Name property.
            /// </summary>
            /// <param name="value">Name</param>
            public CategoryBuilder Name(string value)
            {
                _Name = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Category.
            /// </summary>
            /// <returns>Category</returns>
            public Category Build()
            {
                Validate();
                return new Category(
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