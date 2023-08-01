using System.IO;
using System.Net;
using System.Threading.Tasks;
using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Azure.WebJobs;
using Microsoft.Azure.WebJobs.Extensions.Http;
using Microsoft.Azure.WebJobs.Extensions.OpenApi.Core.Attributes;
using Microsoft.Azure.WebJobs.Extensions.OpenApi.Core.Enums;
using Microsoft.Extensions.Logging;
using Microsoft.OpenApi.Models;
using Newtonsoft.Json;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Functions
{ 
    public partial class StoreApi
    { 
        [FunctionName("StoreApi_DeleteOrder")]
        public async Task<ActionResult<>> _DeleteOrder([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2store/order/{orderId}")]HttpRequest req, ExecutionContext context, string orderId)
        {
            var method = this.GetType().GetMethod("DeleteOrder");
            if(method == null)
            {
                return new StatusCodeResult((int)HttpStatusCode.NotImplemented);
            }
            return (await ((Task<>)method.Invoke(this, new object[] { req, context, id })).ConfigureAwait(false));
        }

        [FunctionName("StoreApi_GetInventory")]
        public async Task<ActionResult<Dictionary<string, int>>> _GetInventory([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2store/inventory")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("GetInventory");
            if(method == null)
            {
                return new StatusCodeResult((int)HttpStatusCode.NotImplemented);
            }
            return (await ((Task<Dictionary<string, int>>)method.Invoke(this, new object[] { req, context, id })).ConfigureAwait(false));
        }

        [FunctionName("StoreApi_GetOrderById")]
        public async Task<ActionResult<Order>> _GetOrderById([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2store/order/{orderId}")]HttpRequest req, ExecutionContext context, [Range(1, 5)]long orderId)
        {
            var method = this.GetType().GetMethod("GetOrderById");
            if(method == null)
            {
                return new StatusCodeResult((int)HttpStatusCode.NotImplemented);
            }
            return (await ((Task<Order>)method.Invoke(this, new object[] { req, context, id })).ConfigureAwait(false));
        }

        [FunctionName("StoreApi_PlaceOrder")]
        public async Task<ActionResult<Order>> _PlaceOrder([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2store/order")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("PlaceOrder");
            if(method == null)
            {
                return new StatusCodeResult((int)HttpStatusCode.NotImplemented);
            }
            return (await ((Task<Order>)method.Invoke(this, new object[] { req, context, id })).ConfigureAwait(false));
        }
    }
}
