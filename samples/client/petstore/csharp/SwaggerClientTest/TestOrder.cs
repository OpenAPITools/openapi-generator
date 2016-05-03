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


namespace SwaggerClientTest.TestOrder
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
			Assert.AreEqual (Order.StatusEnum.Placed, o.Status);
			Assert.AreEqual (true, o.Complete);

		}

		/// <summary>
		/// Test GetInvetory
		/// </summary>
		[Test ()]
		public void TestGetInventory ()
		{
			// set timeout to 10 seconds
			Configuration c1 = new Configuration (timeout: 10000);

			StoreApi storeApi = new StoreApi (c1);
			Dictionary<String, int?> response = storeApi.GetInventory ();

			foreach(KeyValuePair<string, int?> entry in response)
			{
				Assert.IsInstanceOf (typeof(int?), entry.Value);
			}
				
		}

		/*
		 * Skip the following test as the fake endpiont has been removed from the swagger spec
		 * We'll uncomment below after we update the Petstore server
		/// <summary>
		/// Test TestGetInventoryInObject
		/// </summary>
		[Test ()]
		public void TestGetInventoryInObject()
		{
			// set timeout to 10 seconds
			Configuration c1 = new Configuration (timeout: 10000);

			StoreApi storeApi = new StoreApi (c1);
			Newtonsoft.Json.Linq.JObject response = (Newtonsoft.Json.Linq.JObject)storeApi.GetInventoryInObject ();

			// should be a Newtonsoft.Json.Linq.JObject since type is object
			Assert.IsInstanceOf (typeof(Newtonsoft.Json.Linq.JObject), response);

			foreach(KeyValuePair<string, string> entry in response.ToObject<Dictionary<string, string>>())
			{
				Assert.IsInstanceOf (typeof(int?), Int32.Parse(entry.Value));
			}
		}
		*/

		/// <summary>
		/// Test Enum
		/// </summary>
		[Test ()]
		public void TestEnum ()
		{
			Assert.AreEqual (Order.StatusEnum.Approved.ToString(), "Approved");

		}
	}
}

