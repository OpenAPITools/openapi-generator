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
    public partial abstract class UserApi
    { 
        [FunctionName("UserApi_CreateUser")]
        public async Task<ActionResult<>> _CreateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user")]HttpRequest req, ExecutionContext context);

        [FunctionName("UserApi_CreateUsersWithArrayInput")]
        public async Task<ActionResult<>> _CreateUsersWithArrayInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user/createWithArray")]HttpRequest req, ExecutionContext context);

        [FunctionName("UserApi_CreateUsersWithListInput")]
        public async Task<ActionResult<>> _CreateUsersWithListInput([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2user/createWithList")]HttpRequest req, ExecutionContext context);

        [FunctionName("UserApi_DeleteUser")]
        public async Task<ActionResult<>> _DeleteUser([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2user/{username}")]HttpRequest req, ExecutionContext context, string username);

        [FunctionName("UserApi_GetUserByName")]
        public async Task<ActionResult<User>> _GetUserByName([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/{username}")]HttpRequest req, ExecutionContext context, string username);

        [FunctionName("UserApi_LoginUser")]
        public async Task<ActionResult<string>> _LoginUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/login")]HttpRequest req, ExecutionContext context);

        [FunctionName("UserApi_LogoutUser")]
        public async Task<ActionResult<>> _LogoutUser([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2user/logout")]HttpRequest req, ExecutionContext context);

        [FunctionName("UserApi_UpdateUser")]
        public async Task<ActionResult<>> _UpdateUser([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2user/{username}")]HttpRequest req, ExecutionContext context, string username);
    }
}
