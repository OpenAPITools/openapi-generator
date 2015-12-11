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

			// test datetime
			DateTime dateUtc = DateTime.Parse("2008-04-10T13:30:00.0000000z", null, System.Globalization.DateTimeStyles.RoundtripKind);
			Assert.AreEqual("2008-04-10T13:30:00.0000000Z", api.ParameterToString (dateUtc));

			// test datetime with no timezone
			DateTime dateWithNoTz = DateTime.Parse("2008-04-10T13:30:00.000", null, System.Globalization.DateTimeStyles.RoundtripKind);
			Assert.AreEqual("2008-04-10T13:30:00.0000000", api.ParameterToString (dateWithNoTz));

			// test datetime with a time zone
			DateTime dateWithTz = DateTime.Parse("2008-04-10T13:30:00.0000000-04:00", null, System.Globalization.DateTimeStyles.RoundtripKind);
			Assert.AreEqual("2008-04-10T13:30:00.0000000-04:00", api.ParameterToString (dateWithTz));

		}
	}
}

