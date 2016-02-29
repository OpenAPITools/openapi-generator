using NUnit.Framework;

using System;
using System.Linq;
using System.IO;
using System.Collections.Generic;
using IO.Swagger.Api;
using IO.Swagger.Model;
using IO.Swagger.Client;
using System.Reflection;

namespace IO.Swagger.Test
{
    [TestFixture]
    public class UserTests
    {
        private User instance;

        /// <summary>
        /// Setup before each test
        /// </summary>
        [SetUp]
        public void Init()
        {
            instance = new User();
        }
    
        /// <summary>
        /// Clean up after each test
        /// </summary>
        [TearDown]
        public void Cleanup()
        {

        }   

        /// <summary>
        /// Test an instance of User
        /// </summary>
        [Test]
        public void UserInstanceTest()
        {
            Assert.IsInstanceOf<User> (instance, "instance is a User");
        }

        
        /// <summary>
        /// Test the property 'Id' 
        /// </summary>
        [Test]
        public void IdTest()
        {
            // TODO: unit test for the property 'Id' 
        }
        
        /// <summary>
        /// Test the property 'Username' 
        /// </summary>
        [Test]
        public void UsernameTest()
        {
            // TODO: unit test for the property 'Username' 
        }
        
        /// <summary>
        /// Test the property 'FirstName' 
        /// </summary>
        [Test]
        public void FirstNameTest()
        {
            // TODO: unit test for the property 'FirstName' 
        }
        
        /// <summary>
        /// Test the property 'LastName' 
        /// </summary>
        [Test]
        public void LastNameTest()
        {
            // TODO: unit test for the property 'LastName' 
        }
        
        /// <summary>
        /// Test the property 'Email' 
        /// </summary>
        [Test]
        public void EmailTest()
        {
            // TODO: unit test for the property 'Email' 
        }
        
        /// <summary>
        /// Test the property 'Password' 
        /// </summary>
        [Test]
        public void PasswordTest()
        {
            // TODO: unit test for the property 'Password' 
        }
        
        /// <summary>
        /// Test the property 'Phone' 
        /// </summary>
        [Test]
        public void PhoneTest()
        {
            // TODO: unit test for the property 'Phone' 
        }
        
        /// <summary>
        /// Test the property 'UserStatus' 
        /// </summary>
        [Test]
        public void UserStatusTest()
        {
            // TODO: unit test for the property 'UserStatus' 
        }
        

    }

}
