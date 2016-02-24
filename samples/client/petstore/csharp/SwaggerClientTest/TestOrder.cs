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


namespace SwaggerClientTest.TestORder
{
	[TestFixture ()]
	public class TestOrder
	{
		public TestOrder ()
		{
			
		}

		/// <summary>
		/// Test creating a new instance of Order
		/// </summary>
		[Test ()]
		public void TestNewOrder()
		{
			Order o = new Order ();

			Assert.IsNull (o.Id);

		}

		/// <summary>
		/// Test deserialization of JSON to Order and its readonly property
		/// </summary>
		[Test ()]
		public void TesOrderDeserialization()
		{
			string json = @"{
'id': 1982,
'petId': 1020,
'quantity': 1,
'status': 'placed',
'complete': true,
}";
			var o = JsonConvert.DeserializeObject<Order>(json);
			Assert.AreEqual (1982, o.Id);
			Assert.AreEqual (1020, o.PetId);
			Assert.AreEqual (1, o.Quantity);
			Assert.AreEqual ("placed", o.Status);
			Assert.AreEqual (true, o.Complete);

		}
	}
}

