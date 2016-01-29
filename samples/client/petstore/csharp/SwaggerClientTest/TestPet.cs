using NUnit.Framework;
using System;
using System.Linq;
using System.IO;
using System.Collections.Generic;
using IO.Swagger.Api;
using IO.Swagger.Model;
using IO.Swagger.Client;
using System.Reflection;

namespace SwaggerClientTest.TestPet
{
	[TestFixture ()]
	public class TestPet
	{
		public long petId = 11088;

		/// <summary>
		/// Create a Pet object
		/// </summary>
		private Pet createPet()
		{
			// create pet
			Pet p = new Pet();
			p.Id = petId;
			p.Name = "Csharp test";
			p.Status = "available";
			// create Category object
			Category category = new Category();
			category.Id = 56;
			category.Name = "sample category name2";
			List<String> photoUrls = new List<String>(new String[] {"sample photoUrls"});
			// create Tag object
			Tag tag = new Tag();
			tag.Id = petId;
			tag.Name = "sample tag name1";
			List<Tag> tags = new List<Tag>(new Tag[] {tag});
			p.Tags = tags;
			p.Category = category;
			p.PhotoUrls = photoUrls;

			return p;
		}

		/// <summary>
		/// Convert string to byte array
		/// </summary>
		private byte[] GetBytes(string str)
		{
			byte[] bytes = new byte[str.Length * sizeof(char)];
			System.Buffer.BlockCopy(str.ToCharArray(), 0, bytes, 0, bytes.Length);
			return bytes;
		}

		[SetUp] public void Init()
		{
			// create pet
			Pet p = createPet();

			// add pet before testing
			PetApi petApi = new PetApi("http://petstore.swagger.io/v2/");
			petApi.AddPet (p);

		}

		[TearDown] public void Cleanup()
		{
			// remove the pet after testing
			PetApi petApi = new PetApi ();
			petApi.DeletePet(petId, "test key");
		}

		/// <summary>
		/// Test GetPetByIdAsync
		/// </summary>
		[Test ()]
		public void TestGetPetByIdAsync ()
		{
			PetApi petApi = new PetApi ();
			var task = petApi.GetPetByIdAsync (petId);
			Pet response = task.Result;
			Assert.IsInstanceOf<Pet> (response, "Response is a Pet");

			Assert.AreEqual ("Csharp test", response.Name);
			Assert.AreEqual ("available", response.Status);

			Assert.IsInstanceOf<List<Tag>> (response.Tags, "Response.Tags is a Array");
			Assert.AreEqual (petId, response.Tags [0].Id);
			Assert.AreEqual ("sample tag name1", response.Tags [0].Name);

			Assert.IsInstanceOf<List<String>> (response.PhotoUrls, "Response.PhotoUrls is a Array");
			Assert.AreEqual ("sample photoUrls", response.PhotoUrls [0]);

			Assert.IsInstanceOf<Category> (response.Category, "Response.Category is a Category");
			Assert.AreEqual (56, response.Category.Id);
			Assert.AreEqual ("sample category name2", response.Category.Name);

		}

		/// <summary>
		/// Test GetPetByIdAsyncWithHttpInfo
		/// </summary>
		[Test ()]
		public void TestGetPetByIdAsyncWithHttpInfo ()
		{
			PetApi petApi = new PetApi ();
			var task = petApi.GetPetByIdAsyncWithHttpInfo (petId);

			Assert.AreEqual (200, task.Result.StatusCode);
			Assert.IsTrue (task.Result.Headers.ContainsKey("Content-Type"));
			Assert.AreEqual (task.Result.Headers["Content-Type"], "application/json");

			Pet response = task.Result.Data;
			Assert.IsInstanceOf<Pet> (response, "Response is a Pet");

			Assert.AreEqual ("Csharp test", response.Name);
			Assert.AreEqual ("available", response.Status);

			Assert.IsInstanceOf<List<Tag>> (response.Tags, "Response.Tags is a Array");
			Assert.AreEqual (petId, response.Tags [0].Id);
			Assert.AreEqual ("sample tag name1", response.Tags [0].Name);

			Assert.IsInstanceOf<List<String>> (response.PhotoUrls, "Response.PhotoUrls is a Array");
			Assert.AreEqual ("sample photoUrls", response.PhotoUrls [0]);

			Assert.IsInstanceOf<Category> (response.Category, "Response.Category is a Category");
			Assert.AreEqual (56, response.Category.Id);
			Assert.AreEqual ("sample category name2", response.Category.Name);

		}

		/// <summary>
		/// Test GetPetById
		/// </summary>
		[Test ()]
		public void TestGetPetById ()
		{
			// set timeout to 10 seconds
			Configuration c1 = new Configuration (timeout: 10000);

			PetApi petApi = new PetApi (c1);
			Pet response = petApi.GetPetById (petId);
			Assert.IsInstanceOf<Pet> (response, "Response is a Pet");

			Assert.AreEqual ("Csharp test", response.Name);
			Assert.AreEqual ("available", response.Status);

			Assert.IsInstanceOf<List<Tag>> (response.Tags, "Response.Tags is a Array");
			Assert.AreEqual (petId, response.Tags [0].Id);
			Assert.AreEqual ("sample tag name1", response.Tags [0].Name);

			Assert.IsInstanceOf<List<String>> (response.PhotoUrls, "Response.PhotoUrls is a Array");
			Assert.AreEqual ("sample photoUrls", response.PhotoUrls [0]);

			Assert.IsInstanceOf<Category> (response.Category, "Response.Category is a Category");
			Assert.AreEqual (56, response.Category.Id);
			Assert.AreEqual ("sample category name2", response.Category.Name);

		}

		/// <summary>
		/// Test GetPetByIdWithByteArray
		/// </summary>
		[Test ()]
		public void TestGetPetByIdWithByteArray ()
		{
			// set timeout to 10 seconds
			Configuration c1 = new Configuration (timeout: 10000);

			PetApi petApi = new PetApi (c1);
			byte[] response = petApi.GetPetByIdWithByteArray (petId);
			Assert.IsInstanceOf<byte[]> (response, "Response is byte array");
		}

		/// <summary>
		/// Test AddPetUsingByteArray
		/// </summary>
		[Test ()]
		public void TestAddPetUsingByteArray ()
		{
			// set timeout to 10 seconds
			Configuration c1 = new Configuration (timeout: 10000);

			PetApi petApi = new PetApi (c1);
			Pet p = createPet ();
			byte[] petByteArray = GetBytes ((string)petApi.Configuration.ApiClient.Serialize (p));
			petApi.AddPetUsingByteArray (petByteArray);
		}

		/// <summary>
		/// Test UpdatePetWithForm
		/// </summary>
		[Test ()]
		public void TestUpdatePetWithForm ()
		{
			PetApi petApi = new PetApi ();
			petApi.UpdatePetWithForm (petId.ToString(), "new form name", "pending");

			Pet response = petApi.GetPetById (petId);
			Assert.IsInstanceOf<Pet> (response, "Response is a Pet");
			Assert.IsInstanceOf<Category> (response.Category, "Response.Category is a Category");
			Assert.IsInstanceOf<List<Tag>> (response.Tags, "Response.Tags is a Array");

			Assert.AreEqual ("new form name", response.Name);
			Assert.AreEqual ("pending", response.Status);

			Assert.AreEqual (petId, response.Tags [0].Id);
			Assert.AreEqual (56, response.Category.Id);

			// test optional parameter
			petApi.UpdatePetWithForm (petId.ToString(), "new form name2");
			Pet response2 = petApi.GetPetById (petId);
			Assert.AreEqual ("new form name2", response2.Name);
		}

		/// <summary>
		/// Test UploadFile
		/// </summary>
		[Test ()]
		public void TestUploadFile ()
		{
		    Assembly _assembly = Assembly.GetExecutingAssembly();
		    Stream _imageStream = _assembly.GetManifestResourceStream("SwaggerClientTest.swagger-logo.png");
            PetApi petApi = new PetApi ();
			// test file upload with form parameters
			petApi.UploadFile(petId, "new form name", _imageStream);

			// test file upload without any form parameters
			// using optional parameter syntax introduced at .net 4.0
			petApi.UploadFile(petId: petId, file: _imageStream);

		}

		/// <summary>
		/// Test FindPetByStatus
		/// </summary>
		[Test ()]
		public void TestFindPetByStatus ()
		{
			PetApi petApi = new PetApi ();
			List<String> statusList = new List<String>(new String[] {"available"});

			List<Pet> listPet = petApi.FindPetsByStatus (statusList);
			foreach (Pet pet in listPet) // Loop through List with foreach.
			{
				Assert.IsInstanceOf<Pet> (pet, "Response is a Pet");
				Assert.AreEqual ("available", pet.Status);
			}

		}

		/// <summary>
		/// Test Equal
		/// </summary>
		[Test ()]
		public void TestEqual()
		{
			// create pet
			Pet p1 = new Pet();
			p1.Id = petId;
			p1.Name = "Csharp test";
			p1.Status = "available";
			// create Category object
			Category category1 = new Category();
			category1.Id = 56;
			category1.Name = "sample category name2";
			List<String> photoUrls1 = new List<String>(new String[] {"sample photoUrls"});
			// create Tag object
			Tag tag1 = new Tag();
			tag1.Id = petId;
			tag1.Name = "sample tag name1";
			List<Tag> tags1 = new List<Tag>(new Tag[] {tag1});
			p1.Tags = tags1;
			p1.Category = category1;
			p1.PhotoUrls = photoUrls1;

			// create pet 2
			Pet p2 = new Pet();
			p2.Id = petId;
			p2.Name = "Csharp test";
			p2.Status = "available";
			// create Category object
			Category category2 = new Category();
			category2.Id = 56;
			category2.Name = "sample category name2";
			List<String> photoUrls2 = new List<String>(new String[] {"sample photoUrls"});
			// create Tag object
			Tag tag2 = new Tag();
			tag2.Id = petId;
			tag2.Name = "sample tag name1";
			List<Tag> tags2 = new List<Tag>(new Tag[] {tag2});
			p2.Tags = tags2;
			p2.Category = category2;
			p2.PhotoUrls = photoUrls2;

			// p1 and p2 should be equal (both object and attribute level)
			Assert.IsTrue (category1.Equals (category2));
			Assert.IsTrue (tags1.SequenceEqual (tags2));
			Assert.IsTrue (p1.PhotoUrls.SequenceEqual(p2.PhotoUrls));

			Assert.IsTrue (p1.Equals (p2));

			// update attribute to that p1 and p2 are not equal
			category2.Name = "new category name";
			Assert.IsFalse(category1.Equals (category2));

			tags2 = new List<Tag> ();
			Assert.IsFalse (tags1.SequenceEqual (tags2));

			// photoUrls has not changed so it should be equal
			Assert.IsTrue (p1.PhotoUrls.SequenceEqual(p2.PhotoUrls));

			Assert.IsFalse (p1.Equals (p2));

		}

		/// <summary>
		/// Test status code
		/// </summary>
		[Test ()]
		public void TestStatusCodeAndHeader ()
		{
			PetApi petApi = new PetApi ();
			var response = petApi.GetPetByIdWithHttpInfo (petId);
			Assert.AreEqual (response.StatusCode, 200);
			Assert.IsTrue (response.Headers.ContainsKey("Content-Type"));
			Assert.AreEqual (response.Headers["Content-Type"], "application/json");
		}

		/// <summary>
		/// Test default header (should be deprecated
		/// </summary>
		[Test ()]
		public void TestDefaultHeader ()
		{
			PetApi petApi = new PetApi ();
			// there should be a warning for using AddDefaultHeader (deprecated) below
			petApi.AddDefaultHeader ("header_key", "header_value");
			// the following should be used instead as suggested in the doc
			petApi.Configuration.AddDefaultHeader ("header_key2", "header_value2");

		}
	}
}

