using System.Net;
using System.Threading.Tasks;
using System.ComponentModel.DataAnnotations;
using Microsoft.Azure.Functions.Worker;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Functions
{ 
    public partial class PetApi
    { 
        [Function("PetApi_AddPet")]
        public async Task<IActionResult> _AddPet([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("AddPet");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_DeletePet")]
        public async Task<IActionResult> _DeletePet([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2pet/{petId}")] HttpRequest req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("DeletePet");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_FindPetsByStatus")]
        public async Task<IActionResult> _FindPetsByStatus([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/findByStatus")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("FindPetsByStatus");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_FindPetsByTags")]
        public async Task<IActionResult> _FindPetsByTags([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/findByTags")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("FindPetsByTags");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_GetPetById")]
        public async Task<IActionResult> _GetPetById([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/{petId}")] HttpRequest req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("GetPetById");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UpdatePet")]
        public async Task<IActionResult> _UpdatePet([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2pet")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("UpdatePet");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UpdatePetWithForm")]
        public async Task<IActionResult> _UpdatePetWithForm([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet/{petId}")] HttpRequest req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("UpdatePetWithForm");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UploadFile")]
        public async Task<IActionResult> _UploadFile([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet/{petId}/uploadImage")] HttpRequest req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("UploadFile");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }
    }
}
