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
    public class OrderTests
    {
        private Order instance;

        /// <summary>
        /// Setup before each test
        /// </summary>
        [SetUp]
        public void Init()
        {
            instance = new Order();
        }
    
        /// <summary>
        /// Clean up after each test
        /// </summary>
        [TearDown]
        public void Cleanup()
        {

        }   

        /// <summary>
        /// Test an instance of Order
        /// </summary>
        [Test]
        public void OrderInstanceTest()
        {
            Assert.IsInstanceOf<Order> (instance, "instance is a Order");
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
        /// Test the property 'PetId' 
        /// </summary>
        [Test]
        public void PetIdTest()
        {
            // TODO: unit test for the property 'PetId' 
        }
        
        /// <summary>
        /// Test the property 'Quantity' 
        /// </summary>
        [Test]
        public void QuantityTest()
        {
            // TODO: unit test for the property 'Quantity' 
        }
        
        /// <summary>
        /// Test the property 'ShipDate' 
        /// </summary>
        [Test]
        public void ShipDateTest()
        {
            // TODO: unit test for the property 'ShipDate' 
        }
        
        /// <summary>
        /// Test the property 'Status' 
        /// </summary>
        [Test]
        public void StatusTest()
        {
            // TODO: unit test for the property 'Status' 
        }
        
        /// <summary>
        /// Test the property 'Complete' 
        /// </summary>
        [Test]
        public void CompleteTest()
        {
            // TODO: unit test for the property 'Complete' 
        }
        

    }

}
