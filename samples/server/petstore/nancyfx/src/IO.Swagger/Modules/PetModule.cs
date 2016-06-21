using System;
using Nancy;
using Nancy.ModelBinding;
using System.Collections.Generic;
using Sharpility.Base;
using IO.Swagger.v2.Models;
using IO.Swagger.v2.Utils;
using NodaTime;

namespace IO.Swagger.v2.Modules
{ 

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
            Post["/pet"] = parameters =>
            {
                var body = this.Bind<Pet>();
                service.AddPet(Context, body);
                return new Response { ContentType = "application/json"};
            };

            Delete["/pet/{petId}"] = parameters =>
            {
                var petId = Parameters.ValueOf<long?>(parameters, Context.Request, "petId", ParameterType.Path);
                var apiKey = Parameters.ValueOf<string>(parameters, Context.Request, "apiKey", ParameterType.Header);
                Preconditions.IsNotNull(petId, "Required parameter: 'petId' is missing at 'DeletePet'");
                
                service.DeletePet(Context, petId, apiKey);
                return new Response { ContentType = "application/json"};
            };

            Get["/pet/findByStatus"] = parameters =>
            {
                var status = Parameters.ValueOf<List<string>>(parameters, Context.Request, "status", ParameterType.Query);
                return service.FindPetsByStatus(Context, status);
            };

            Get["/pet/findByTags"] = parameters =>
            {
                var tags = Parameters.ValueOf<List<string>>(parameters, Context.Request, "tags", ParameterType.Query);
                return service.FindPetsByTags(Context, tags);
            };

            Get["/pet/{petId}"] = parameters =>
            {
                var petId = Parameters.ValueOf<long?>(parameters, Context.Request, "petId", ParameterType.Path);
                Preconditions.IsNotNull(petId, "Required parameter: 'petId' is missing at 'GetPetById'");
                
                return service.GetPetById(Context, petId);
            };

            Put["/pet"] = parameters =>
            {
                var body = this.Bind<Pet>();
                service.UpdatePet(Context, body);
                return new Response { ContentType = "application/json"};
            };

            Post["/pet/{petId}"] = parameters =>
            {
                var petId = Parameters.ValueOf<string>(parameters, Context.Request, "petId", ParameterType.Path);
                var name = Parameters.ValueOf<string>(parameters, Context.Request, "name", ParameterType.Undefined);
                var status = Parameters.ValueOf<string>(parameters, Context.Request, "status", ParameterType.Undefined);
                Preconditions.IsNotNull(petId, "Required parameter: 'petId' is missing at 'UpdatePetWithForm'");
                
                service.UpdatePetWithForm(Context, petId, name, status);
                return new Response { ContentType = "application/json"};
            };

            Post["/pet/{petId}/uploadImage"] = parameters =>
            {
                var petId = Parameters.ValueOf<long?>(parameters, Context.Request, "petId", ParameterType.Path);
                var additionalMetadata = Parameters.ValueOf<string>(parameters, Context.Request, "additionalMetadata", ParameterType.Undefined);
                var file = Parameters.ValueOf<System.IO.Stream>(parameters, Context.Request, "file", ParameterType.Undefined);
                Preconditions.IsNotNull(petId, "Required parameter: 'petId' is missing at 'UploadFile'");
                
                service.UploadFile(Context, petId, additionalMetadata, file);
                return new Response { ContentType = "application/json"};
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
        /// <param name="body">Pet object that needs to be added to the store (optional)</param>
        /// <returns></returns>
        void AddPet(NancyContext context, Pet body);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"> (optional)</param>
        /// <returns></returns>
        void DeletePet(NancyContext context, long? petId, string apiKey);

        /// <summary>
        /// Multiple status values can be provided with comma seperated strings
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="status">Status values that need to be considered for filter (optional, default to available)</param>
        /// <returns>List&lt;Pet&gt;</returns>
        List<Pet> FindPetsByStatus(NancyContext context, List<string> status);

        /// <summary>
        /// Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="tags">Tags to filter by (optional)</param>
        /// <returns>List&lt;Pet&gt;</returns>
        List<Pet> FindPetsByTags(NancyContext context, List<string> tags);

        /// <summary>
        /// Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <returns>Pet</returns>
        Pet GetPetById(NancyContext context, long? petId);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">Pet object that needs to be added to the store (optional)</param>
        /// <returns></returns>
        void UpdatePet(NancyContext context, Pet body);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet (optional)</param>
        /// <param name="status">Updated status of the pet (optional)</param>
        /// <returns></returns>
        void UpdatePetWithForm(NancyContext context, string petId, string name, string status);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server (optional)</param>
        /// <param name="file">file to upload (optional)</param>
        /// <returns></returns>
        void UploadFile(NancyContext context, long? petId, string additionalMetadata, System.IO.Stream file);
    }

    /// <summary>
    /// Abstraction of PetService.
    /// </summary>
    public abstract class AbstractPetService: PetService
    {
        public virtual void AddPet(NancyContext context, Pet body)
        {
            AddPet(body);
        }

        public virtual void DeletePet(NancyContext context, long? petId, string apiKey)
        {
            DeletePet(petId, apiKey);
        }

        public virtual List<Pet> FindPetsByStatus(NancyContext context, List<string> status)
        {
            return FindPetsByStatus(status);
        }

        public virtual List<Pet> FindPetsByTags(NancyContext context, List<string> tags)
        {
            return FindPetsByTags(tags);
        }

        public virtual Pet GetPetById(NancyContext context, long? petId)
        {
            return GetPetById(petId);
        }

        public virtual void UpdatePet(NancyContext context, Pet body)
        {
            UpdatePet(body);
        }

        public virtual void UpdatePetWithForm(NancyContext context, string petId, string name, string status)
        {
            UpdatePetWithForm(petId, name, status);
        }

        public virtual void UploadFile(NancyContext context, long? petId, string additionalMetadata, System.IO.Stream file)
        {
            UploadFile(petId, additionalMetadata, file);
        }

        protected abstract void AddPet(Pet body);

        protected abstract void DeletePet(long? petId, string apiKey);

        protected abstract List<Pet> FindPetsByStatus(List<string> status);

        protected abstract List<Pet> FindPetsByTags(List<string> tags);

        protected abstract Pet GetPetById(long? petId);

        protected abstract void UpdatePet(Pet body);

        protected abstract void UpdatePetWithForm(string petId, string name, string status);

        protected abstract void UploadFile(long? petId, string additionalMetadata, System.IO.Stream file);
    }

}
