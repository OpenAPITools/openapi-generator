using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using Microsoft.AspNet.Mvc;
using Newtonsoft.Json;
using Swashbuckle.SwaggerGen.Annotations;
using IO.Swagger.Models;

namespace IO.Swagger.Controllers
{ 
    /// <summary>
    /// 
    /// </summary>
    public class StoreApiController : Controller
    { 

        /// <summary>
        /// Returns pet inventories by status
        /// </summary>
        /// <remarks>Returns a map of status codes to quantities</remarks>
        /// <response code="200">successful operation</response>
        [HttpGet]
        [Route("/store/inventory")]
        [SwaggerOperation("GetInventory")]
        [SwaggerResponse(200, type: typeof(Dictionary<string, int?>))]
        public IActionResult GetInventory()
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<Dictionary<string, int?>>(exampleJson)
            : default(Dictionary<string, int?>);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Place an order for a pet
        /// </summary>
        /// <remarks></remarks>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <response code="200">successful operation</response>
        /// <response code="400">Invalid Order</response>
        [HttpPost]
        [Route("/store/order")]
        [SwaggerOperation("PlaceOrder")]
        [SwaggerResponse(200, type: typeof(Order))]
        public IActionResult PlaceOrder([FromBody]Order body)
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<Order>(exampleJson)
            : default(Order);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Find purchase order by ID
        /// </summary>
        /// <remarks>For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions</remarks>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <response code="200">successful operation</response>
        /// <response code="400">Invalid ID supplied</response>
        /// <response code="404">Order not found</response>
        [HttpGet]
        [Route("/store/order/{orderId}")]
        [SwaggerOperation("GetOrderById")]
        [SwaggerResponse(200, type: typeof(Order))]
        public IActionResult GetOrderById([FromRoute]string orderId)
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<Order>(exampleJson)
            : default(Order);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Delete purchase order by ID
        /// </summary>
        /// <remarks>For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors</remarks>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <response code="400">Invalid ID supplied</response>
        /// <response code="404">Order not found</response>
        [HttpDelete]
        [Route("/store/order/{orderId}")]
        [SwaggerOperation("DeleteOrder")]
        public void DeleteOrder([FromRoute]string orderId)
        { 
            throw new NotImplementedException();
        }
    }
}
