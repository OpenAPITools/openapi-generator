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
    public partial class UserApi
    { 
        [Function("UserApi_CreateUser")]
        [OpenApiOperation(operationId: "CreateUser", tags: new[] { "user" }, Summary = "Create user")]
        [OpenApiRequestBody(contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.User), Required = true)]
        public async Task<IActionResult> _CreateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/user")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_CreateUsersWithArrayInput")]
        [OpenApiOperation(operationId: "CreateUsersWithArrayInput", tags: new[] { "user" }, Summary = "Creates list of users with given input array")]
        [OpenApiRequestBody(contentType: "application/json", bodyType: typeof(List<Org.OpenAPITools.Models.User>), Required = true)]
        public async Task<IActionResult> _CreateUsersWithArrayInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/user/createWithArray")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUsersWithArrayInput");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_CreateUsersWithListInput")]
        [OpenApiOperation(operationId: "CreateUsersWithListInput", tags: new[] { "user" }, Summary = "Creates list of users with given input array")]
        [OpenApiRequestBody(contentType: "application/json", bodyType: typeof(List<Org.OpenAPITools.Models.User>), Required = true)]
        public async Task<IActionResult> _CreateUsersWithListInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/user/createWithList")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUsersWithListInput");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_DeleteUser")]
        [OpenApiOperation(operationId: "DeleteUser", tags: new[] { "user" }, Summary = "Delete user")]
        [OpenApiParameter(name: "username", In = ParameterLocation.Path, Required = true, Type = typeof(string))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)404)]
        public async Task<IActionResult> _DeleteUser([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2/user/{username}")] HttpRequest req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("DeleteUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_GetUserByName")]
        [OpenApiOperation(operationId: "GetUserByName", tags: new[] { "user" }, Summary = "Get user by user name")]
        [OpenApiParameter(name: "username", In = ParameterLocation.Path, Required = true, Type = typeof(string))]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.User))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)404)]
        public async Task<IActionResult> _GetUserByName([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/user/{username}")] HttpRequest req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("GetUserByName");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_LoginUser")]
        [OpenApiOperation(operationId: "LoginUser", tags: new[] { "user" }, Summary = "Logs user into the system")]
        [OpenApiParameter(name: "username", In = ParameterLocation.Query, Required = true, Type = typeof(string))]
        [OpenApiParameter(name: "password", In = ParameterLocation.Query, Required = true, Type = typeof(string))]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(string))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        public async Task<IActionResult> _LoginUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/user/login")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("LoginUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_LogoutUser")]
        [OpenApiOperation(operationId: "LogoutUser", tags: new[] { "user" }, Summary = "Logs out current logged in user session")]
        public async Task<IActionResult> _LogoutUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/user/logout")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("LogoutUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_UpdateUser")]
        [OpenApiOperation(operationId: "UpdateUser", tags: new[] { "user" }, Summary = "Updated user")]
        [OpenApiParameter(name: "username", In = ParameterLocation.Path, Required = true, Type = typeof(string))]
        [OpenApiRequestBody(contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.User), Required = true)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)404)]
        public async Task<IActionResult> _UpdateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2/user/{username}")] HttpRequest req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("UpdateUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }
    }
}
