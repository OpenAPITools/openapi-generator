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
            Pet body = null; // TODO: replace null with proper value
            
            instance.UpdatePet(body);
             
        }
        
        /// <summary>
        /// Test AddPet
        /// </summary>
        [Test]
        public void AddPetTest()
        {
            // TODO: add unit test for the method 'AddPet'
            Pet body = null; // TODO: replace null with proper value
            
            instance.AddPet(body);
             
        }
        
        /// <summary>
        /// Test FindPetsByStatus
        /// </summary>
        [Test]
        public void FindPetsByStatusTest()
        {
            // TODO: add unit test for the method 'FindPetsByStatus'
            List<string> status = null; // TODO: replace null with proper value
            
            var response = instance.FindPetsByStatus(status);
            Assert.IsInstanceOf<List<Pet>> (response, "response is List<Pet>"); 
        }
        
        /// <summary>
        /// Test FindPetsByTags
        /// </summary>
        [Test]
        public void FindPetsByTagsTest()
        {
            // TODO: add unit test for the method 'FindPetsByTags'
            List<string> tags = null; // TODO: replace null with proper value
            
            var response = instance.FindPetsByTags(tags);
            Assert.IsInstanceOf<List<Pet>> (response, "response is List<Pet>"); 
        }
        
        /// <summary>
        /// Test GetPetById
        /// </summary>
        [Test]
        public void GetPetByIdTest()
        {
            // TODO: add unit test for the method 'GetPetById'
            long? petId = null; // TODO: replace null with proper value
            
            var response = instance.GetPetById(petId);
            Assert.IsInstanceOf<Pet> (response, "response is Pet"); 
        }
        
        /// <summary>
        /// Test UpdatePetWithForm
        /// </summary>
        [Test]
        public void UpdatePetWithFormTest()
        {
            // TODO: add unit test for the method 'UpdatePetWithForm'
            string petId = null; // TODO: replace null with proper value
            string name = null; // TODO: replace null with proper value
            string status = null; // TODO: replace null with proper value
            
            instance.UpdatePetWithForm(petId, name, status);
             
        }
        
        /// <summary>
        /// Test DeletePet
        /// </summary>
        [Test]
        public void DeletePetTest()
        {
            // TODO: add unit test for the method 'DeletePet'
            long? petId = null; // TODO: replace null with proper value
            string apiKey = null; // TODO: replace null with proper value
            
            instance.DeletePet(petId, apiKey);
             
        }
        
        /// <summary>
        /// Test UploadFile
        /// </summary>
        [Test]
        public void UploadFileTest()
        {
            // TODO: add unit test for the method 'UploadFile'
            long? petId = null; // TODO: replace null with proper value
            string additionalMetadata = null; // TODO: replace null with proper value
            Stream file = null; // TODO: replace null with proper value
            
            instance.UploadFile(petId, additionalMetadata, file);
             
        }
        
        /// <summary>
        /// Test GetPetByIdWithByteArray
        /// </summary>
        [Test]
        public void GetPetByIdWithByteArrayTest()
        {
            // TODO: add unit test for the method 'GetPetByIdWithByteArray'
            long? petId = null; // TODO: replace null with proper value
            
            var response = instance.GetPetByIdWithByteArray(petId);
            Assert.IsInstanceOf<byte[]> (response, "response is byte[]"); 
        }
        
        /// <summary>
        /// Test AddPetUsingByteArray
        /// </summary>
        [Test]
        public void AddPetUsingByteArrayTest()
        {
            // TODO: add unit test for the method 'AddPetUsingByteArray'
            byte[] body = null; // TODO: replace null with proper value
            
            instance.AddPetUsingByteArray(body);
             
        }
        
    }

}
