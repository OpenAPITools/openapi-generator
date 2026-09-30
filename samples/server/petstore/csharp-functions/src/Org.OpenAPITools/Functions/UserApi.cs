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
    public partial class UserApi
    { 
        [FunctionName("UserApi_CreateUser")]
        public async Task<ActionResult<>> _CreateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("CreateUser");
            return method != null
                ? (await ((Task<>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("UserApi_CreateUsersWithArrayInput")]
        public async Task<ActionResult<>> _CreateUsersWithArrayInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user/createWithArray")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("CreateUsersWithArrayInput");
            return method != null
                ? (await ((Task<>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("UserApi_CreateUsersWithListInput")]
        public async Task<ActionResult<>> _CreateUsersWithListInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user/createWithList")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("CreateUsersWithListInput");
            return method != null
                ? (await ((Task<>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("UserApi_DeleteUser")]
        public async Task<ActionResult<>> _DeleteUser([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2user/{username}")]HttpRequest req, ExecutionContext context, string username)
        {
            var method = this.GetType().GetMethod("DeleteUser");
            return method != null
                ? (await ((Task<>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("UserApi_GetUserByName")]
        public async Task<ActionResult<User>> _GetUserByName([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/{username}")]HttpRequest req, ExecutionContext context, string username)
        {
            var method = this.GetType().GetMethod("GetUserByName");
            return method != null
                ? (await ((Task<User>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("UserApi_LoginUser")]
        public async Task<ActionResult<string>> _LoginUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/login")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("LoginUser");
            return method != null
                ? (await ((Task<string>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("UserApi_LogoutUser")]
        public async Task<ActionResult<>> _LogoutUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/logout")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("LogoutUser");
            return method != null
                ? (await ((Task<>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("UserApi_UpdateUser")]
        public async Task<ActionResult<>> _UpdateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2user/{username}")]HttpRequest req, ExecutionContext context, string username)
        {
            var method = this.GetType().GetMethod("UpdateUser");
            return method != null
                ? (await ((Task<>)method.Invoke(this, new object[] { req, context, username })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }
    }
}
