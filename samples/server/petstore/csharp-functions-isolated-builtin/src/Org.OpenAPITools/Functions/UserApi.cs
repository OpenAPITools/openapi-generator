using System.Net;
using System.Threading.Tasks;
using System.ComponentModel.DataAnnotations;
using Microsoft.Azure.Functions.Worker;
using Microsoft.Azure.Functions.Worker.Http;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Functions
{ 
    public partial class UserApi
    { 
        [Function("UserApi_CreateUser")]
        public async Task<HttpResponseData> _CreateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUser");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_CreateUsersWithArrayInput")]
        public async Task<HttpResponseData> _CreateUsersWithArrayInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user/createWithArray")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUsersWithArrayInput");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_CreateUsersWithListInput")]
        public async Task<HttpResponseData> _CreateUsersWithListInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user/createWithList")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUsersWithListInput");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_DeleteUser")]
        public async Task<HttpResponseData> _DeleteUser([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2user/{username}")] HttpRequestData req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("DeleteUser");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_GetUserByName")]
        public async Task<HttpResponseData> _GetUserByName([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/{username}")] HttpRequestData req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("GetUserByName");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_LoginUser")]
        public async Task<HttpResponseData> _LoginUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/login")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("LoginUser");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_LogoutUser")]
        public async Task<HttpResponseData> _LogoutUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/logout")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("LogoutUser");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_UpdateUser")]
        public async Task<HttpResponseData> _UpdateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2user/{username}")] HttpRequestData req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("UpdateUser");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }
    }
}
