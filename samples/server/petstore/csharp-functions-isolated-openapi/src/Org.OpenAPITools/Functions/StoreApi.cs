using System.Net;
using System.Threading.Tasks;
using System.ComponentModel.DataAnnotations;
using Microsoft.Azure.Functions.Worker;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using Microsoft.Azure.WebJobs.Extensions.OpenApi.Core.Attributes;
using Microsoft.OpenApi.Models;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Functions
{ 
    public partial class StoreApi
    { 
        [Function("StoreApi_DeleteOrder")]
        [OpenApiOperation(operationId: "DeleteOrder", tags: new[] { "store" }, Summary = "Delete purchase order by ID")]
        [OpenApiParameter(name: "orderId", In = ParameterLocation.Path, Required = true, Type = typeof(string))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)404)]
        public async Task<IActionResult> _DeleteOrder([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2store/order/{orderId}")] HttpRequest req, FunctionContext context, string orderId)
        {
            var method = this.GetType().GetMethod("DeleteOrder");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, orderId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_GetInventory")]
        [OpenApiOperation(operationId: "GetInventory", tags: new[] { "store" }, Summary = "Returns pet inventories by status")]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(Dictionary<string, int>))]
        public async Task<IActionResult> _GetInventory([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2store/inventory")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("GetInventory");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_GetOrderById")]
        [OpenApiOperation(operationId: "GetOrderById", tags: new[] { "store" }, Summary = "Find purchase order by ID")]
        [OpenApiParameter(name: "orderId", In = ParameterLocation.Path, Required = true, Type = typeof(long))]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.Order))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)404)]
        public async Task<IActionResult> _GetOrderById([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2store/order/{orderId}")] HttpRequest req, FunctionContext context, [Range(1, 5)]long orderId)
        {
            var method = this.GetType().GetMethod("GetOrderById");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, orderId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_PlaceOrder")]
        [OpenApiOperation(operationId: "PlaceOrder", tags: new[] { "store" }, Summary = "Place an order for a pet")]
        [OpenApiRequestBody(contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.Order), Required = true)]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.Order))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        public async Task<IActionResult> _PlaceOrder([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2store/order")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("PlaceOrder");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }
    }
}
