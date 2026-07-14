using System.Net;
using System.Threading.Tasks;
using System.ComponentModel.DataAnnotations;
using Microsoft.Azure.Functions.Worker;
using Microsoft.Azure.Functions.Worker.Http;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Functions
{ 
    public partial class StoreApi
    { 
        [Function("StoreApi_DeleteOrder")]
        public async Task<HttpResponseData> _DeleteOrder([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2store/order/{orderId}")] HttpRequestData req, FunctionContext context, string orderId)
        {
            var method = this.GetType().GetMethod("DeleteOrder");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, orderId })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_GetInventory")]
        public async Task<HttpResponseData> _GetInventory([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2store/inventory")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("GetInventory");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_GetOrderById")]
        public async Task<HttpResponseData> _GetOrderById([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2store/order/{orderId}")] HttpRequestData req, FunctionContext context, [Range(1, 5)]long orderId)
        {
            var method = this.GetType().GetMethod("GetOrderById");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, orderId })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("StoreApi_PlaceOrder")]
        public async Task<HttpResponseData> _PlaceOrder([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2store/order")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("PlaceOrder");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }
    }
}
