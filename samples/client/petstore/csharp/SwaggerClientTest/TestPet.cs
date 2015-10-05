using NUnit.Framework;
using System;
using System.IO;
using System.Collections.Generic;
using IO.Swagger.Api;
using IO.Swagger.Model;
using IO.Swagger.Client;


namespace SwaggerClient.TestPet
{
	[TestFixture ()]
	public class TestPet
	{
		public long petId = 11088;

		[SetUp] public void Init()
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

		[Test ()]
		public void TestGetPetById ()
		{
			PetApi petApi = new PetApi ();
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
		}

		[Test ()]
		public void TestUploadFile ()
		{
			PetApi petApi = new PetApi ();
			//NOTE: please provide a valid file (full path)
			FileStream fileStream = new FileStream("/var/tmp/small.gif", FileMode.Open);
			// test file upload with form parameters
			petApi.UploadFile(petId, "new form name", fileStream);

			// test file upload without any form parameters
			petApi.UploadFile(petId, null, fileStream);

		}


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

	}
}

