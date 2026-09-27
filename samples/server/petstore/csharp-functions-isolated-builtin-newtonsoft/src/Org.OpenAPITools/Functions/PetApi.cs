using System.Net;
using System.Threading.Tasks;
using System.ComponentModel.DataAnnotations;
using Microsoft.Azure.Functions.Worker;
using Microsoft.Azure.Functions.Worker.Http;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Functions
{ 
    public partial class PetApi
    { 
        [Function("PetApi_AddPet")]
        public async Task<HttpResponseData> _AddPet([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/pet")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("AddPet");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_DeletePet")]
        public async Task<HttpResponseData> _DeletePet([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2/pet/{petId}")] HttpRequestData req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("DeletePet");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_FindPetsByStatus")]
        public async Task<HttpResponseData> _FindPetsByStatus([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/pet/findByStatus")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("FindPetsByStatus");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_FindPetsByTags")]
        public async Task<HttpResponseData> _FindPetsByTags([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/pet/findByTags")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("FindPetsByTags");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_GetPetById")]
        public async Task<HttpResponseData> _GetPetById([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2/pet/{petId}")] HttpRequestData req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("GetPetById");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UpdatePet")]
        public async Task<HttpResponseData> _UpdatePet([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2/pet")] HttpRequestData req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("UpdatePet");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UpdatePetWithForm")]
        public async Task<HttpResponseData> _UpdatePetWithForm([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/pet/{petId}")] HttpRequestData req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("UpdatePetWithForm");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UploadFile")]
        public async Task<HttpResponseData> _UploadFile([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2/pet/{petId}/uploadImage")] HttpRequestData req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("UploadFile");
            return method != null
                ? (await ((Task<HttpResponseData>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : req.CreateResponse(HttpStatusCode.NotImplemented);
        }
    }
}
