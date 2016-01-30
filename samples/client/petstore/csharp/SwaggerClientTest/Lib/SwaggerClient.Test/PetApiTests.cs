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
    public class PetApiTests
    {
        private PetApi instance;

        /// <summary>
        /// Setup before each unit test
        /// </summary>
        [SetUp]
        public void Init()
        {
           instance = new PetApi();
        }

        /// <summary>
        /// Clean up after each unit test
        /// </summary>
        [TearDown]
        public void Cleanup()
        {

        }

        /// <summary>
        /// Test an instance of PetApi
        /// </summary>
        [Test]
        public void InstanceTest()
        {
            Assert.IsInstanceOf<PetApi> (instance, "instance is a PetApi");
        }

        
        /// <summary>
        /// Test UpdatePet
        /// </summary>
        [Test]
        public void UpdatePetTest()
        {
            // TODO: add unit test for the method 'UpdatePet'
        }
        
        /// <summary>
        /// Test AddPet
        /// </summary>
        [Test]
        public void AddPetTest()
        {
            // TODO: add unit test for the method 'AddPet'
        }
        
        /// <summary>
        /// Test FindPetsByStatus
        /// </summary>
        [Test]
        public void FindPetsByStatusTest()
        {
            // TODO: add unit test for the method 'FindPetsByStatus'
        }
        
        /// <summary>
        /// Test FindPetsByTags
        /// </summary>
        [Test]
        public void FindPetsByTagsTest()
        {
            // TODO: add unit test for the method 'FindPetsByTags'
        }
        
        /// <summary>
        /// Test GetPetById
        /// </summary>
        [Test]
        public void GetPetByIdTest()
        {
            // TODO: add unit test for the method 'GetPetById'
        }
        
        /// <summary>
        /// Test UpdatePetWithForm
        /// </summary>
        [Test]
        public void UpdatePetWithFormTest()
        {
            // TODO: add unit test for the method 'UpdatePetWithForm'
        }
        
        /// <summary>
        /// Test DeletePet
        /// </summary>
        [Test]
        public void DeletePetTest()
        {
            // TODO: add unit test for the method 'DeletePet'
        }
        
        /// <summary>
        /// Test UploadFile
        /// </summary>
        [Test]
        public void UploadFileTest()
        {
            // TODO: add unit test for the method 'UploadFile'
        }
        
        /// <summary>
        /// Test GetPetByIdWithByteArray
        /// </summary>
        [Test]
        public void GetPetByIdWithByteArrayTest()
        {
            // TODO: add unit test for the method 'GetPetByIdWithByteArray'
        }
        
        /// <summary>
        /// Test AddPetUsingByteArray
        /// </summary>
        [Test]
        public void AddPetUsingByteArrayTest()
        {
            // TODO: add unit test for the method 'AddPetUsingByteArray'
        }
        
    }

}
