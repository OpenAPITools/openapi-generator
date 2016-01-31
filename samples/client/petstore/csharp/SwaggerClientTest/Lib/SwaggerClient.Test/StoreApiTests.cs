using System;
using System.IO;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection;
using RestSharp;
using NUnit.Framework;

using IO.Swagger.Client;
using IO.Swagger.Api;
using IO.Swagger.Model;

namespace IO.Swagger.Test
{
    [TestFixture]
    public class StoreApiTests
    {
        private StoreApi instance;

        /// <summary>
        /// Setup before each unit test
        /// </summary>
        [SetUp]
        public void Init()
        {
           instance = new StoreApi();
        }

        /// <summary>
        /// Clean up after each unit test
        /// </summary>
        [TearDown]
        public void Cleanup()
        {

        }

        /// <summary>
        /// Test an instance of StoreApi
        /// </summary>
        [Test]
        public void InstanceTest()
        {
            Assert.IsInstanceOf<StoreApi> (instance, "instance is a StoreApi");
        }

        
        /// <summary>
        /// Test GetInventory
        /// </summary>
        [Test]
        public void GetInventoryTest()
        {
            // TODO: add unit test for the method 'GetInventory'
            
            var response = instance.GetInventory();
            Assert.IsInstanceOf<Dictionary<string, int?>> (response, "response is Dictionary<string, int?>"); 
        }
        
        /// <summary>
        /// Test PlaceOrder
        /// </summary>
        [Test]
        public void PlaceOrderTest()
        {
            // TODO: add unit test for the method 'PlaceOrder'
            Order body = null; // TODO: replace null with proper value
            
            var response = instance.PlaceOrder(body);
            Assert.IsInstanceOf<Order> (response, "response is Order"); 
        }
        
        /// <summary>
        /// Test GetOrderById
        /// </summary>
        [Test]
        public void GetOrderByIdTest()
        {
            // TODO: add unit test for the method 'GetOrderById'
            string orderId = null; // TODO: replace null with proper value
            
            var response = instance.GetOrderById(orderId);
            Assert.IsInstanceOf<Order> (response, "response is Order"); 
        }
        
        /// <summary>
        /// Test DeleteOrder
        /// </summary>
        [Test]
        public void DeleteOrderTest()
        {
            // TODO: add unit test for the method 'DeleteOrder'
            string orderId = null; // TODO: replace null with proper value
            
            instance.DeleteOrder(orderId);
             
        }
        
    }

}
