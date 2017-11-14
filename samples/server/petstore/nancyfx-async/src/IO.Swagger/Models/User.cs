using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace IO.Swagger.v2.Models
{
    /// <summary>
    /// A User who is purchasing from the pet store
    /// </summary>
    public sealed class User:  IEquatable<User>
    { 
        /// <summary>
        /// Id
        /// </summary>
        public long? Id { get; private set; }

        /// <summary>
        /// Username
        /// </summary>
        public string Username { get; private set; }

        /// <summary>
        /// FirstName
        /// </summary>
        public string FirstName { get; private set; }

        /// <summary>
        /// LastName
        /// </summary>
        public string LastName { get; private set; }

        /// <summary>
        /// Email
        /// </summary>
        public string Email { get; private set; }

        /// <summary>
        /// Password
        /// </summary>
        public string Password { get; private set; }

        /// <summary>
        /// Phone
        /// </summary>
        public string Phone { get; private set; }

        /// <summary>
        /// User Status
        /// </summary>
        public int? UserStatus { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use User.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public User()
        {
        }

        private User(long? Id, string Username, string FirstName, string LastName, string Email, string Password, string Phone, int? UserStatus)
        {
            
            this.Id = Id;
            
            this.Username = Username;
            
            this.FirstName = FirstName;
            
            this.LastName = LastName;
            
            this.Email = Email;
            
            this.Password = Password;
            
            this.Phone = Phone;
            
            this.UserStatus = UserStatus;
            
        }

        /// <summary>
        /// Returns builder of User.
        /// </summary>
        /// <returns>UserBuilder</returns>
        public static UserBuilder Builder()
        {
            return new UserBuilder();
        }

        /// <summary>
        /// Returns UserBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>UserBuilder</returns>
        public UserBuilder With()
        {
            return Builder()
                .Id(Id)
                .Username(Username)
                .FirstName(FirstName)
                .LastName(LastName)
                .Email(Email)
                .Password(Password)
                .Phone(Phone)
                .UserStatus(UserStatus);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(User other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (User.
        /// </summary>
        /// <param name="left">Compared (User</param>
        /// <param name="right">Compared (User</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (User left, User right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (User.
        /// </summary>
        /// <param name="left">Compared (User</param>
        /// <param name="right">Compared (User</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (User left, User right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of User.
        /// </summary>
        public sealed class UserBuilder
        {
            private long? _Id;
            private string _Username;
            private string _FirstName;
            private string _LastName;
            private string _Email;
            private string _Password;
            private string _Phone;
            private int? _UserStatus;

            internal UserBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
            }

            /// <summary>
            /// Sets value for User.Id property.
            /// </summary>
            /// <param name="value">Id</param>
            public UserBuilder Id(long? value)
            {
                _Id = value;
                return this;
            }

            /// <summary>
            /// Sets value for User.Username property.
            /// </summary>
            /// <param name="value">Username</param>
            public UserBuilder Username(string value)
            {
                _Username = value;
                return this;
            }

            /// <summary>
            /// Sets value for User.FirstName property.
            /// </summary>
            /// <param name="value">FirstName</param>
            public UserBuilder FirstName(string value)
            {
                _FirstName = value;
                return this;
            }

            /// <summary>
            /// Sets value for User.LastName property.
            /// </summary>
            /// <param name="value">LastName</param>
            public UserBuilder LastName(string value)
            {
                _LastName = value;
                return this;
            }

            /// <summary>
            /// Sets value for User.Email property.
            /// </summary>
            /// <param name="value">Email</param>
            public UserBuilder Email(string value)
            {
                _Email = value;
                return this;
            }

            /// <summary>
            /// Sets value for User.Password property.
            /// </summary>
            /// <param name="value">Password</param>
            public UserBuilder Password(string value)
            {
                _Password = value;
                return this;
            }

            /// <summary>
            /// Sets value for User.Phone property.
            /// </summary>
            /// <param name="value">Phone</param>
            public UserBuilder Phone(string value)
            {
                _Phone = value;
                return this;
            }

            /// <summary>
            /// Sets value for User.UserStatus property.
            /// </summary>
            /// <param name="value">User Status</param>
            public UserBuilder UserStatus(int? value)
            {
                _UserStatus = value;
                return this;
            }


            /// <summary>
            /// Builds instance of User.
            /// </summary>
            /// <returns>User</returns>
            public User Build()
            {
                Validate();
                return new User(
                    Id: _Id,
                    Username: _Username,
                    FirstName: _FirstName,
                    LastName: _LastName,
                    Email: _Email,
                    Password: _Password,
                    Phone: _Phone,
                    UserStatus: _UserStatus
                );
            }

            private void Validate()
            { 
            }
        }

        
    }
}