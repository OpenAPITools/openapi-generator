using System;
using Nancy;
using Nancy.ModelBinding;
using System.Collections.Generic;
using Sharpility.Base;
using IO.Swagger.v2.Models;
using IO.Swagger.v2.Utils;
using NodaTime;
using System.Threading.Tasks;

namespace IO.Swagger.v2.Modules
{ 
    /// <summary>
    /// Status values that need to be considered for filter
    /// </summary>
    public enum FindPetsByStatusStatusEnum
    {
        available = 1, 
        pending = 2, 
        sold = 3
    };


    /// <summary>
    /// Module processing requests of Pet domain.
    /// </summary>
    public sealed class PetModule : NancyModule
    {
        /// <summary>
        /// Sets up HTTP methods mappings.
        /// </summary>
        /// <param name="service">Service handling requests</param>
        public PetModule(PetService service) : base("/v2")
        { 
            Post["/pet", true] = async (parameters, ct) =>
            {
                var body = this.Bind<Pet>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'AddPet'");
                
                await service.AddPet(Context, body);
                return new Response { ContentType = "application/xml"};
            };

            Delete["/pet/{petId}", true] = async (parameters, ct) =>
            {
                var petId = Parameters.ValueOf<long?>(parameters, Context.Request, "petId", ParameterType.Path);
                var apiKey = Parameters.ValueOf<string>(parameters, Context.Request, "apiKey", ParameterType.Header);
                Preconditions.IsNotNull(petId, "Required parameter: 'petId' is missing at 'DeletePet'");
                
                await service.DeletePet(Context, petId, apiKey);
                return new Response { ContentType = "application/xml"};
            };

            Get["/pet/findByStatus", true] = async (parameters, ct) =>
            {
                var status = Parameters.ValueOf<FindPetsByStatusStatusEnum?>(parameters, Context.Request, "status", ParameterType.Query);
                Preconditions.IsNotNull(status, "Required parameter: 'status' is missing at 'FindPetsByStatus'");
                
                return await service.FindPetsByStatus(Context, status).ToArray();
            };

            Get["/pet/findByTags", true] = async (parameters, ct) =>
            {
                var tags = Parameters.ValueOf<List<string>>(parameters, Context.Request, "tags", ParameterType.Query);
                Preconditions.IsNotNull(tags, "Required parameter: 'tags' is missing at 'FindPetsByTags'");
                
                return await service.FindPetsByTags(Context, tags).ToArray();
            };

            Get["/pet/{petId}", true] = async (parameters, ct) =>
            {
                var petId = Parameters.ValueOf<long?>(parameters, Context.Request, "petId", ParameterType.Path);
                Preconditions.IsNotNull(petId, "Required parameter: 'petId' is missing at 'GetPetById'");
                
                return await service.GetPetById(Context, petId);
            };

            Put["/pet", true] = async (parameters, ct) =>
            {
                var body = this.Bind<Pet>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'UpdatePet'");
                
                await service.UpdatePet(Context, body);
                return new Response { ContentType = "application/xml"};
            };

            Post["/pet/{petId}", true] = async (parameters, ct) =>
            {
                var petId = Parameters.ValueOf<long?>(parameters, Context.Request, "petId", ParameterType.Path);
                var name = Parameters.ValueOf<string>(parameters, Context.Request, "name", ParameterType.Undefined);
                var status = Parameters.ValueOf<string>(parameters, Context.Request, "status", ParameterType.Undefined);
                Preconditions.IsNotNull(petId, "Required parameter: 'petId' is missing at 'UpdatePetWithForm'");
                
                await service.UpdatePetWithForm(Context, petId, name, status);
                return new Response { ContentType = "application/xml"};
            };

            Post["/pet/{petId}/uploadImage", true] = async (parameters, ct) =>
            {
                var petId = Parameters.ValueOf<long?>(parameters, Context.Request, "petId", ParameterType.Path);
                var additionalMetadata = Parameters.ValueOf<string>(parameters, Context.Request, "additionalMetadata", ParameterType.Undefined);
                var file = Parameters.ValueOf<System.IO.Stream>(parameters, Context.Request, "file", ParameterType.Undefined);
                Preconditions.IsNotNull(petId, "Required parameter: 'petId' is missing at 'UploadFile'");
                
                return await service.UploadFile(Context, petId, additionalMetadata, file);
            };
        }
    }

    /// <summary>
    /// Service handling Pet requests.
    /// </summary>
    public interface PetService
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        Task AddPet(NancyContext context, Pet body);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"> (optional)</param>
        /// <returns></returns>
        Task DeletePet(NancyContext context, long? petId, string apiKey);

        /// <summary>
        /// Multiple status values can be provided with comma separated strings
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <returns>List&lt;Pet&gt;</returns>
        Task<List<Pet>> FindPetsByStatus(NancyContext context, FindPetsByStatusStatusEnum? status);

        /// <summary>
        /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="tags">Tags to filter by</param>
        /// <returns>List&lt;Pet&gt;</returns>
        Task<List<Pet>> FindPetsByTags(NancyContext context, List<string> tags);

        /// <summary>
        /// Returns a single pet
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="petId">ID of pet to return</param>
        /// <returns>Pet</returns>
        Task<Pet> GetPetById(NancyContext context, long? petId);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <returns></returns>
        Task UpdatePet(NancyContext context, Pet body);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet (optional)</param>
        /// <param name="status">Updated status of the pet (optional)</param>
        /// <returns></returns>
        Task UpdatePetWithForm(NancyContext context, long? petId, string name, string status);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server (optional)</param>
        /// <param name="file">file to upload (optional)</param>
        /// <returns>ApiResponse</returns>
        Task<ApiResponse> UploadFile(NancyContext context, long? petId, string additionalMetadata, System.IO.Stream file);
    }

    /// <summary>
    /// Abstraction of PetService.
    /// </summary>
    public abstract class AbstractPetService: PetService
    {
        public virtual Task AddPet(NancyContext context, Pet body)
        {
            return AddPet(body);
        }

        public virtual Task DeletePet(NancyContext context, long? petId, string apiKey)
        {
            return DeletePet(petId, apiKey);
        }

        public virtual Task<List<Pet>> FindPetsByStatus(NancyContext context, FindPetsByStatusStatusEnum? status)
        {
            return FindPetsByStatus(status);
        }

        public virtual Task<List<Pet>> FindPetsByTags(NancyContext context, List<string> tags)
        {
            return FindPetsByTags(tags);
        }

        public virtual Task<Pet> GetPetById(NancyContext context, long? petId)
        {
            return GetPetById(petId);
        }

        public virtual Task UpdatePet(NancyContext context, Pet body)
        {
            return UpdatePet(body);
        }

        public virtual Task UpdatePetWithForm(NancyContext context, long? petId, string name, string status)
        {
            return UpdatePetWithForm(petId, name, status);
        }

        public virtual Task<ApiResponse> UploadFile(NancyContext context, long? petId, string additionalMetadata, System.IO.Stream file)
        {
            return UploadFile(petId, additionalMetadata, file);
        }

        protected abstract Task AddPet(Pet body);

        protected abstract Task DeletePet(long? petId, string apiKey);

        protected abstract Task<List<Pet>> FindPetsByStatus(FindPetsByStatusStatusEnum? status);

        protected abstract Task<List<Pet>> FindPetsByTags(List<string> tags);

        protected abstract Task<Pet> GetPetById(long? petId);

        protected abstract Task UpdatePet(Pet body);

        protected abstract Task UpdatePetWithForm(long? petId, string name, string status);

        protected abstract Task<ApiResponse> UploadFile(long? petId, string additionalMetadata, System.IO.Stream file);
    }

}
