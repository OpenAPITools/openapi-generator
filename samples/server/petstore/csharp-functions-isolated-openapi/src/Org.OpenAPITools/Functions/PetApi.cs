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
    public partial class PetApi
    { 
        [Function("PetApi_AddPet")]
        [OpenApiOperation(operationId: "AddPet", tags: new[] { "pet" }, Summary = "Add a new pet to the store")]
        [OpenApiRequestBody(contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.Pet), Required = true)]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.Pet))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)405)]
        public async Task<IActionResult> _AddPet([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("AddPet");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_DeletePet")]
        [OpenApiOperation(operationId: "DeletePet", tags: new[] { "pet" }, Summary = "Deletes a pet")]
        [OpenApiParameter(name: "petId", In = ParameterLocation.Path, Required = true, Type = typeof(long))]
        [OpenApiParameter(name: "api_key", In = ParameterLocation.Header, Required = false, Type = typeof(string))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        public async Task<IActionResult> _DeletePet([HttpTrigger(AuthorizationLevel.Anonymous, "Delete", Route = "v2pet/{petId}")] HttpRequest req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("DeletePet");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_FindPetsByStatus")]
        [OpenApiOperation(operationId: "FindPetsByStatus", tags: new[] { "pet" }, Summary = "Finds Pets by status")]
        [OpenApiParameter(name: "status", In = ParameterLocation.Query, Required = true, Type = typeof(List<string>))]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(List<Org.OpenAPITools.Models.Pet>))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        public async Task<IActionResult> _FindPetsByStatus([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/findByStatus")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("FindPetsByStatus");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_FindPetsByTags")]
        [OpenApiOperation(operationId: "FindPetsByTags", tags: new[] { "pet" }, Summary = "Finds Pets by tags")]
        [OpenApiParameter(name: "tags", In = ParameterLocation.Query, Required = true, Type = typeof(List<string>))]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(List<Org.OpenAPITools.Models.Pet>))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        public async Task<IActionResult> _FindPetsByTags([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/findByTags")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("FindPetsByTags");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_GetPetById")]
        [OpenApiOperation(operationId: "GetPetById", tags: new[] { "pet" }, Summary = "Find pet by ID")]
        [OpenApiParameter(name: "petId", In = ParameterLocation.Path, Required = true, Type = typeof(long))]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.Pet))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)404)]
        public async Task<IActionResult> _GetPetById([HttpTrigger(AuthorizationLevel.Anonymous, "Get", Route = "v2pet/{petId}")] HttpRequest req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("GetPetById");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UpdatePet")]
        [OpenApiOperation(operationId: "UpdatePet", tags: new[] { "pet" }, Summary = "Update an existing pet")]
        [OpenApiRequestBody(contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.Pet), Required = true)]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.Pet))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)400)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)404)]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)405)]
        public async Task<IActionResult> _UpdatePet([HttpTrigger(AuthorizationLevel.Anonymous, "Put", Route = "v2pet")] HttpRequest req, FunctionContext context)
        {
            var method = this.GetType().GetMethod("UpdatePet");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UpdatePetWithForm")]
        [OpenApiOperation(operationId: "UpdatePetWithForm", tags: new[] { "pet" }, Summary = "Updates a pet in the store with form data")]
        [OpenApiParameter(name: "petId", In = ParameterLocation.Path, Required = true, Type = typeof(long))]
        [OpenApiResponseWithoutBody(statusCode: (HttpStatusCode)405)]
        public async Task<IActionResult> _UpdatePetWithForm([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet/{petId}")] HttpRequest req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("UpdatePetWithForm");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }

        [Function("PetApi_UploadFile")]
        [OpenApiOperation(operationId: "UploadFile", tags: new[] { "pet" }, Summary = "uploads an image")]
        [OpenApiParameter(name: "petId", In = ParameterLocation.Path, Required = true, Type = typeof(long))]
        [OpenApiResponseWithBody(statusCode: (HttpStatusCode)200, contentType: "application/json", bodyType: typeof(Org.OpenAPITools.Models.ApiResponse))]
        public async Task<IActionResult> _UploadFile([HttpTrigger(AuthorizationLevel.Anonymous, "Post", Route = "v2pet/{petId}/uploadImage")] HttpRequest req, FunctionContext context, long petId)
        {
            var method = this.GetType().GetMethod("UploadFile");
            return method != null
                ? (await ((Task<IActionResult>)method.Invoke(this, new object[] { req, context, petId })).ConfigureAwait(false))
                : new StatusCodeResult((int)HttpStatusCode.NotImplemented);
        }
    }
}
