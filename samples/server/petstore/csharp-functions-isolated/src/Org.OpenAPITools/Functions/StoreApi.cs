using System.Net;
using System.Threading.Tasks;
using System.ComponentModel.DataAnnotations;
using Microsoft.Azure.Functions.Worker;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Functions
{ 
    public partial class StoreApi
    { 
        [Function("StoreApi_DeleteOrder")]
        public async Task<IActionResult> _DeleteOrder([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2store/order/{orderId}")] HttpRequest req, FunctionContext context, string orderId)
        {
            var method = this.GetType().GetMethod("DeleteOrder");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, orderId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_GetInventory")]
        public async Task<IActionResult> _GetInventory([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2store/inventory")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("GetInventory");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_GetOrderById")]
        public async Task<IActionResult> _GetOrderById([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2store/order/{orderId}")] HttpRequest req, FunctionContext context, [Range(1, 5)]long orderId)
        {
            var method = this.GetType().GetMethod("GetOrderById");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, orderId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_PlaceOrder")]
        public async Task<IActionResult> _PlaceOrder([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2store/order")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("PlaceOrder");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }
    }
}
