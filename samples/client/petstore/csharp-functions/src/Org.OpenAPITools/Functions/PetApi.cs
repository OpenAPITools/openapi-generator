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
    public partial class PetApi
    { 
        [FunctionName("PetApi_AddPet")]
        public async Task<ActionResult<Pet>> _AddPet([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("AddPet");
            return method != null
                ? (await ((Task<Pet>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("PetApi_DeletePet")]
        public async Task<ActionResult<>> _DeletePet([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2pet/{petId}")]HttpRequest req, ExecutionContext context, long petId)
        {
            var method = this.GetType().GetMethod("DeletePet");
            return method != null
                ? (await ((Task<>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("PetApi_FindPetsByStatus")]
        public async Task<ActionResult<List<Pet>>> _FindPetsByStatus([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/findByStatus")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("FindPetsByStatus");
            return method != null
                ? (await ((Task<List<Pet>>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("PetApi_FindPetsByTags")]
        public async Task<ActionResult<List<Pet>>> _FindPetsByTags([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/findByTags")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("FindPetsByTags");
            return method != null
                ? (await ((Task<List<Pet>>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("PetApi_GetPetById")]
        public async Task<ActionResult<Pet>> _GetPetById([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/{petId}")]HttpRequest req, ExecutionContext context, long petId)
        {
            var method = this.GetType().GetMethod("GetPetById");
            return method != null
                ? (await ((Task<Pet>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("PetApi_UpdatePet")]
        public async Task<ActionResult<Pet>> _UpdatePet([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2pet")]HttpRequest req, ExecutionContext context)
        {
            var method = this.GetType().GetMethod("UpdatePet");
            return method != null
                ? (await ((Task<Pet>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("PetApi_UpdatePetWithForm")]
        public async Task<ActionResult<>> _UpdatePetWithForm([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet/{petId}")]HttpRequest req, ExecutionContext context, long petId)
        {
            var method = this.GetType().GetMethod("UpdatePetWithForm");
            return method != null
                ? (await ((Task<>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [FunctionName("PetApi_UploadFile")]
        public async Task<ActionResult<ApiResponse>> _UploadFile([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet/{petId}/uploadImage")]HttpRequest req, ExecutionContext context, long petId)
        {
            var method = this.GetType().GetMethod("UploadFile");
            return method != null
                ? (await ((Task<ApiResponse>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }
    }
}
