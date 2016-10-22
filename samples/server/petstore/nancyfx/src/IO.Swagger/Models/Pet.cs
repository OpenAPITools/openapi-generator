using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// A pet for sale in the pet store
    /// </summary>
    public sealed class Pet:  IEquatable<Pet>
    { 
        /// <summary>
        /// Id
        /// </summary>
        public long? Id { get; private set; }

        /// <summary>
        /// Category
        /// </summary>
        public Category Category { get; private set; }

        /// <summary>
        /// Name
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// PhotoUrls
        /// </summary>
        public List<string> PhotoUrls { get; private set; }

        /// <summary>
        /// Tags
        /// </summary>
        public List<Tag> Tags { get; private set; }

        /// <summary>
        /// pet status in the store
        /// </summary>
        public StatusEnum? Status { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Pet.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Pet()
        {
        }

        private Pet(long? Id, Category Category, string Name, List<string> PhotoUrls, List<Tag> Tags, StatusEnum? Status)
        {
            
            this.Id = Id;
            
            this.Category = Category;
            
            this.Name = Name;
            
            this.PhotoUrls = PhotoUrls;
            
            this.Tags = Tags;
            
            this.Status = Status;
            
        }

        /// <summary>
        /// Returns builder of Pet.
        /// </summary>
        /// <returns>PetBuilder</returns>
        public static PetBuilder Builder()
        {
            return new PetBuilder();
        }

        /// <summary>
        /// Returns PetBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>PetBuilder</returns>
        public PetBuilder With()
        {
            return Builder()
                .Id(Id)
                .Category(Category)
                .Name(Name)
                .PhotoUrls(PhotoUrls)
                .Tags(Tags)
                .Status(Status);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Pet other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Pet.
        /// </summary>
        /// <param name="left">Compared (Pet</param>
        /// <param name="right">Compared (Pet</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Pet left, Pet right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Pet.
        /// </summary>
        /// <param name="left">Compared (Pet</param>
        /// <param name="right">Compared (Pet</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Pet left, Pet right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Pet.
        /// </summary>
        public sealed class PetBuilder
        {
            private long? _Id;
            private Category _Category;
            private string _Name;
            private List<string> _PhotoUrls;
            private List<Tag> _Tags;
            private StatusEnum? _Status;

            internal PetBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for Pet.Id property.
            /// </summary>
            /// <param name="value">Id</param>
            public PetBuilder Id(long? value)
            {
                _Id = value;
                return this;
            }

            /// <summary>
            /// Sets value for Pet.Category property.
            /// </summary>
            /// <param name="value">Category</param>
            public PetBuilder Category(Category value)
            {
                _Category = value;
                return this;
            }

            /// <summary>
            /// Sets value for Pet.Name property.
            /// </summary>
            /// <param name="value">Name</param>
            public PetBuilder Name(string value)
            {
                _Name = value;
                return this;
            }

            /// <summary>
            /// Sets value for Pet.PhotoUrls property.
            /// </summary>
            /// <param name="value">PhotoUrls</param>
            public PetBuilder PhotoUrls(List<string> value)
            {
                _PhotoUrls = value;
                return this;
            }

            /// <summary>
            /// Sets value for Pet.Tags property.
            /// </summary>
            /// <param name="value">Tags</param>
            public PetBuilder Tags(List<Tag> value)
            {
                _Tags = value;
                return this;
            }

            /// <summary>
            /// Sets value for Pet.Status property.
            /// </summary>
            /// <param name="value">pet status in the store</param>
            public PetBuilder Status(StatusEnum? value)
            {
                _Status = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Pet.
            /// </summary>
            /// <returns>Pet</returns>
            public Pet Build()
            {
                Validate();
                return new Pet(
                    Id: _Id,
                    Category: _Category,
                    Name: _Name,
                    PhotoUrls: _PhotoUrls,
                    Tags: _Tags,
                    Status: _Status
                );
            }

            private void Validate()
            { 
                if (_Name == null)
                {
                    throw new ArgumentException("Name is a required property for Pet and cannot be null");
                } 
                if (_PhotoUrls == null)
                {
                    throw new ArgumentException("PhotoUrls is a required property for Pet and cannot be null");
                } 
            }
        }

        
        public enum StatusEnum { Available, Pending, Sold };
    }
}
