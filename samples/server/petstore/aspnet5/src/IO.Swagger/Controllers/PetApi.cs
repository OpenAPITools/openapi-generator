using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using Microsoft.AspNet.Mvc;
using Newtonsoft.Json;
using Swashbuckle.SwaggerGen.Annotations;
using IO.Swagger.Models;

namespace IO.Swagger.Controllers
{ 
    /// <summary>
    /// 
    /// </summary>
    public class PetApiController : Controller
    { 

        /// <summary>
        /// Update an existing pet
        /// </summary>
        /// <remarks></remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <response code="400">Invalid ID supplied</response>
        /// <response code="404">Pet not found</response>
        /// <response code="405">Validation exception</response>
        [HttpPut]
        [Route("/pet")]
        [SwaggerOperation("UpdatePet")]
        public void UpdatePet([FromBody]Pet body)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Add a new pet to the store
        /// </summary>
        /// <remarks></remarks>
        /// <param name="body">Pet object that needs to be added to the store</param>
        /// <response code="405">Invalid input</response>
        [HttpPost]
        [Route("/pet")]
        [SwaggerOperation("AddPet")]
        public void AddPet([FromBody]Pet body)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Finds Pets by status
        /// </summary>
        /// <remarks>Multiple status values can be provided with comma seperated strings</remarks>
        /// <param name="status">Status values that need to be considered for filter</param>
        /// <response code="200">successful operation</response>
        /// <response code="400">Invalid status value</response>
        [HttpGet]
        [Route("/pet/findByStatus")]
        [SwaggerOperation("FindPetsByStatus")]
        [SwaggerResponse(200, type: typeof(List<Pet>))]
        public IActionResult FindPetsByStatus([FromQuery]List<string> status)
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<List<Pet>>(exampleJson)
            : default(List<Pet>);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Finds Pets by tags
        /// </summary>
        /// <remarks>Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.</remarks>
        /// <param name="tags">Tags to filter by</param>
        /// <response code="200">successful operation</response>
        /// <response code="400">Invalid tag value</response>
        [HttpGet]
        [Route("/pet/findByTags")]
        [SwaggerOperation("FindPetsByTags")]
        [SwaggerResponse(200, type: typeof(List<Pet>))]
        public IActionResult FindPetsByTags([FromQuery]List<string> tags)
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<List<Pet>>(exampleJson)
            : default(List<Pet>);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Find pet by ID
        /// </summary>
        /// <remarks>Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions</remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <response code="200">successful operation</response>
        /// <response code="400">Invalid ID supplied</response>
        /// <response code="404">Pet not found</response>
        [HttpGet]
        [Route("/pet/{petId}")]
        [SwaggerOperation("GetPetById")]
        [SwaggerResponse(200, type: typeof(Pet))]
        public IActionResult GetPetById([FromRoute]long? petId)
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<Pet>(exampleJson)
            : default(Pet);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Updates a pet in the store with form data
        /// </summary>
        /// <remarks></remarks>
        /// <param name="petId">ID of pet that needs to be updated</param>
        /// <param name="name">Updated name of the pet</param>
        /// <param name="status">Updated status of the pet</param>
        /// <response code="405">Invalid input</response>
        [HttpPost]
        [Route("/pet/{petId}")]
        [SwaggerOperation("UpdatePetWithForm")]
        public void UpdatePetWithForm([FromRoute]string petId, [FromForm]string name, [FromForm]string status)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Deletes a pet
        /// </summary>
        /// <remarks></remarks>
        /// <param name="petId">Pet id to delete</param>
        /// <param name="apiKey"></param>
        /// <response code="400">Invalid pet value</response>
        [HttpDelete]
        [Route("/pet/{petId}")]
        [SwaggerOperation("DeletePet")]
        public void DeletePet([FromRoute]long? petId, [FromHeader]string apiKey)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// uploads an image
        /// </summary>
        /// <remarks></remarks>
        /// <param name="petId">ID of pet to update</param>
        /// <param name="additionalMetadata">Additional data to pass to server</param>
        /// <param name="file">file to upload</param>
        /// <response code="0">successful operation</response>
        [HttpPost]
        [Route("/pet/{petId}/uploadImage")]
        [SwaggerOperation("UploadFile")]
        public void UploadFile([FromRoute]long? petId, [FromForm]string additionalMetadata, [FromForm]Stream file)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
        /// </summary>
        /// <remarks>Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions</remarks>
        /// <param name="petId">ID of pet that needs to be fetched</param>
        /// <response code="200">successful operation</response>
        /// <response code="400">Invalid ID supplied</response>
        /// <response code="404">Pet not found</response>
        [HttpGet]
        [Route("/pet/{petId}/testing_byte_array=true")]
        [SwaggerOperation("GetPetByIdWithByteArray")]
        [SwaggerResponse(200, type: typeof(byte[]))]
        public IActionResult GetPetByIdWithByteArray([FromRoute]long? petId)
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<byte[]>(exampleJson)
            : default(byte[]);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Fake endpoint to test byte array in body parameter for adding a new pet to the store
        /// </summary>
        /// <remarks></remarks>
        /// <param name="body">Pet object in the form of byte array</param>
        /// <response code="405">Invalid input</response>
        [HttpPost]
        [Route("/pet/testing_byte_array=true")]
        [SwaggerOperation("AddPetUsingByteArray")]
        public void AddPetUsingByteArray([FromBody]byte[] body)
        { 
            throw new NotImplementedException();
        }
    }
}
