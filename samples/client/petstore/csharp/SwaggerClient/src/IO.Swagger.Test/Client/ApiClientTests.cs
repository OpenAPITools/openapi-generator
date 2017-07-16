using NUnit.Framework;
using System;
using System.Collections.Generic;
using IO.Swagger.Client;
using IO.Swagger.Api;
using IO.Swagger.Model;

namespace IO.Swagger.Test
{
	public class ApiClientTests
	{
		public ApiClientTests ()
		{
		}

		[SetUp()]
		public void BeforeEach()
		{
			var config = new GlobalConfiguration();
			Configuration.Default = config;
		}

		[TearDown()]
		public void TearDown()
		{
			// Reset to default, just in case
			Configuration.Default.DateTimeFormat = "o";
		}

		/// <summary>
		/// Test SelectHeaderContentType
		/// </summary>
		[Test ()]
		public void TestSelectHeaderContentType ()
		{	
			ApiClient api = new ApiClient ();
			String[] contentTypes = new String[] { "application/json", "application/xml" };
			Assert.AreEqual("application/json", api.SelectHeaderContentType (contentTypes));

			contentTypes = new String[] { "application/xml" };
			Assert.AreEqual("application/xml", api.SelectHeaderContentType (contentTypes));

			contentTypes = new String[] {};
			Assert.IsNull(api.SelectHeaderContentType (contentTypes));	
		}

		/// <summary>
		/// Test ParameterToString
		/// </summary>
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

		[Test ()]
		public void TestParameterToStringForDateTime ()
		{
			ApiClient api = new ApiClient ();

			// test datetime
			DateTime dateUtc = DateTime.Parse ("2008-04-10T13:30:00.0000000z", null, System.Globalization.DateTimeStyles.RoundtripKind);
			Assert.AreEqual ("2008-04-10T13:30:00.0000000Z", api.ParameterToString (dateUtc));

			// test datetime with no timezone
			DateTime dateWithNoTz = DateTime.Parse ("2008-04-10T13:30:00.000", null, System.Globalization.DateTimeStyles.RoundtripKind);
			Assert.AreEqual ("2008-04-10T13:30:00.0000000", api.ParameterToString (dateWithNoTz));
		}

		// The test below only passes when running at -04:00 timezone
		[Ignore ()]
		public void TestParameterToStringWithTimeZoneForDateTime ()
		{
			ApiClient api = new ApiClient ();
			// test datetime with a time zone
			DateTimeOffset dateWithTz = DateTimeOffset.Parse("2008-04-10T13:30:00.0000000-04:00", null, System.Globalization.DateTimeStyles.RoundtripKind);
			Assert.AreEqual("2008-04-10T13:30:00.0000000-04:00", api.ParameterToString(dateWithTz));
		}

		[Test ()]
		public void TestParameterToStringForDateTimeWithUFormat ()
		{
			// Setup the DateTimeFormat across all of the calls
			Configuration.Default.DateTimeFormat = "u";
			ApiClient api = new ApiClient();

			// test datetime
			DateTime dateUtc = DateTime.Parse("2009-06-15 20:45:30Z", null, System.Globalization.DateTimeStyles.RoundtripKind);
			Assert.AreEqual("2009-06-15 20:45:30Z", api.ParameterToString(dateUtc));
		}

		[Test ()]
		public void TestParameterToStringForDateTimeWithCustomFormat ()
		{
			// Setup the DateTimeFormat across all of the calls
			Configuration.Default.DateTimeFormat = "dd/MM/yy HH:mm:ss";
			ApiClient api = new ApiClient();

			// test datetime
			DateTime dateUtc = DateTime.Parse("2009-06-15 20:45:30Z", null, System.Globalization.DateTimeStyles.RoundtripKind);
			Assert.AreEqual("15/06/09 20:45:30", api.ParameterToString(dateUtc));
		}

		[Test ()]
		public void TestSanitizeFilename ()
		{
			Assert.AreEqual("sun.gif", ApiClient.SanitizeFilename("sun.gif"));
			Assert.AreEqual("sun.gif", ApiClient.SanitizeFilename("../sun.gif"));
			Assert.AreEqual("sun.gif", ApiClient.SanitizeFilename("/var/tmp/sun.gif"));
			Assert.AreEqual("sun.gif", ApiClient.SanitizeFilename("./sun.gif"));

			Assert.AreEqual("sun", ApiClient.SanitizeFilename("sun"));
			Assert.AreEqual("sun.gif", ApiClient.SanitizeFilename("..\\sun.gif"));
			Assert.AreEqual("sun.gif", ApiClient.SanitizeFilename("\\var\\tmp\\sun.gif"));
			Assert.AreEqual("sun.gif", ApiClient.SanitizeFilename("c:\\var\\tmp\\sun.gif"));
			Assert.AreEqual("sun.gif", ApiClient.SanitizeFilename(".\\sun.gif"));

		}
	}
}