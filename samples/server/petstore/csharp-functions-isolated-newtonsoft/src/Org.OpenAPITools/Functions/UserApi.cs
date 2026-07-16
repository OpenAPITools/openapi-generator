using System.Net;
using System.Threading.Tasks;
using System.ComponentModel.DataAnnotations;
using Microsoft.Azure.Functions.Worker;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Functions
{ 
    public partial class UserApi
    { 
        [Function("UserApi_CreateUser")]
        public async Task<IActionResult> _CreateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/user")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_CreateUsersWithArrayInput")]
        public async Task<IActionResult> _CreateUsersWithArrayInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/user/createWithArray")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUsersWithArrayInput");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_CreateUsersWithListInput")]
        public async Task<IActionResult> _CreateUsersWithListInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/user/createWithList")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("CreateUsersWithListInput");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_DeleteUser")]
        public async Task<IActionResult> _DeleteUser([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2/user/{username}")] HttpRequest req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("DeleteUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_GetUserByName")]
        public async Task<IActionResult> _GetUserByName([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/user/{username}")] HttpRequest req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("GetUserByName");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_LoginUser")]
        public async Task<IActionResult> _LoginUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/user/login")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("LoginUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_LogoutUser")]
        public async Task<IActionResult> _LogoutUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/user/logout")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("LogoutUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("UserApi_UpdateUser")]
        public async Task<IActionResult> _UpdateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2/user/{username}")] HttpRequest req, FunctionContext context, string username)
        {
            var method = this.GetType().GetMethod("UpdateUser");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }
    }
}
