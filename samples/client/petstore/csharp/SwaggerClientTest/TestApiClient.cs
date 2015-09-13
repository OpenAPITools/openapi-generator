using NUnit.Framework;
using System;
using System.Collections.Generic;
using IO.Swagger.Client;

namespace SwaggerClient.TestApiClient
{
	public class TestApiClient
	{
		[Test ()]
		public void TestParameterToString ()
		{	
			ApiClient api = new ApiClient ();

			// test array of string
			List<string> statusList = new List<String>(new String[] {"available", "sold"});
			Assert.AreEqual("available,sold", api.ParameterToString (statusList));

			// test array of int
			List<int> numList = new List<int>(new int[] {1, 37});
			Assert.AreEqual("1,37", api.ParameterToString (numList));
		}

	}
}

