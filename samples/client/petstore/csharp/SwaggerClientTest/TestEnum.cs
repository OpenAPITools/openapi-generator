using NUnit.Framework;
using System;
using System.Linq;
using System.IO;
using System.Collections.Generic;
using IO.Swagger.Api;
using IO.Swagger.Model;
using IO.Swagger.Client;
using System.Reflection;
using Newtonsoft.Json;

namespace SwaggerClientTest.TestEnum
{
	[TestFixture ()]
	public class TestEnum
	{
		public TestEnum ()
		{
		}

		/// <summary>
		/// Test EnumClass
		/// </summary>
		[Test ()]
		public void TestEnumClass ()
		{
			// test serialization for string
			Assert.AreEqual (Newtonsoft.Json.JsonConvert.SerializeObject(EnumClass.Abc), "\"_abc\"");

			// test serialization for number
			Assert.AreEqual (Newtonsoft.Json.JsonConvert.SerializeObject(EnumTest.EnumIntegerEnum.NUMBER_MINUS_1), "\"-1\"");

			// test cast to int
			Assert.AreEqual ((int)EnumTest.EnumIntegerEnum.NUMBER_MINUS_1, -1);

		}
	}
}

