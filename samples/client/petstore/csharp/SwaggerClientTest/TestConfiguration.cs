using NUnit.Framework;
using System;
using System.Collections.Generic;
using IO.Swagger.Client;
using IO.Swagger.Api;
using IO.Swagger.Model;

namespace SwaggerClient.TestConfiguration
{
	public class TestConfiguration
	{
		[Test ()]
		public void TestAuthentication ()
		{
			Configuration c = new Configuration ();
			c.Username = "test_username";
			c.Password = "test_password";

			c.ApiKey ["api_key_identifier"] = "1233456778889900";
			c.ApiKeyPrefix ["api_key_identifier"] = "PREFIX";
			Assert.AreEqual (c.GetApiKeyWithPrefix("api_key_identifier"), "PREFIX 1233456778889900");

		}

		[Test ()]
		public void TestBasePath ()
		{	
			PetApi p = new PetApi ("http://new-basepath.com");
			Assert.AreEqual (p.Configuration.ApiClient.BasePath, "http://new-basepath.com");
			Assert.AreSame (p.Configuration, Configuration.DefaultConfiguration);

		}

		[Test ()]
		public void TestDefautlConfiguration ()
		{	
			PetApi p1 = new PetApi ();
			PetApi p2 = new PetApi ();
			Assert.AreSame (p1.Configuration, p2.Configuration);
			// same as the default
			Assert.AreSame (p1.Configuration, Configuration.DefaultConfiguration);

			Configuration c = new Configuration ();
			Assert.AreNotSame (c, p1.Configuration);

			PetApi p3 = new PetApi (c);
			// same as c
			Assert.AreSame (p3.Configuration, c);
			// not same as default
			Assert.AreNotSame (p3.Configuration, p1.Configuration);

		}
	}
}