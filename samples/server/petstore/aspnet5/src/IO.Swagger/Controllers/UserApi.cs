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
    public class UserApiController : Controller
    { 

        /// <summary>
        /// Create user
        /// </summary>
        /// <remarks>This can only be done by the logged in user.</remarks>
        /// <param name="body">Created user object</param>
        /// <response code="0">successful operation</response>
        [HttpPost]
        [Route("/user")]
        [SwaggerOperation("CreateUser")]
        public void CreateUser([FromBody]User body)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Creates list of users with given input array
        /// </summary>
        /// <remarks></remarks>
        /// <param name="body">List of user object</param>
        /// <response code="0">successful operation</response>
        [HttpPost]
        [Route("/user/createWithArray")]
        [SwaggerOperation("CreateUsersWithArrayInput")]
        public void CreateUsersWithArrayInput([FromBody]List<User> body)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Creates list of users with given input array
        /// </summary>
        /// <remarks></remarks>
        /// <param name="body">List of user object</param>
        /// <response code="0">successful operation</response>
        [HttpPost]
        [Route("/user/createWithList")]
        [SwaggerOperation("CreateUsersWithListInput")]
        public void CreateUsersWithListInput([FromBody]List<User> body)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Logs user into the system
        /// </summary>
        /// <remarks></remarks>
        /// <param name="username">The user name for login</param>
        /// <param name="password">The password for login in clear text</param>
        /// <response code="200">successful operation</response>
        /// <response code="400">Invalid username/password supplied</response>
        [HttpGet]
        [Route("/user/login")]
        [SwaggerOperation("LoginUser")]
        [SwaggerResponse(200, type: typeof(string))]
        public IActionResult LoginUser([FromQuery]string username, [FromQuery]string password)
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<string>(exampleJson)
            : default(string);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Logs out current logged in user session
        /// </summary>
        /// <remarks></remarks>
        /// <response code="0">successful operation</response>
        [HttpGet]
        [Route("/user/logout")]
        [SwaggerOperation("LogoutUser")]
        public void LogoutUser()
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Get user by user name
        /// </summary>
        /// <remarks></remarks>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing.</param>
        /// <response code="200">successful operation</response>
        /// <response code="400">Invalid username supplied</response>
        /// <response code="404">User not found</response>
        [HttpGet]
        [Route("/user/{username}")]
        [SwaggerOperation("GetUserByName")]
        [SwaggerResponse(200, type: typeof(User))]
        public IActionResult GetUserByName([FromRoute]string username)
        { 
            string exampleJson = null;
            
            var example = exampleJson != null
            ? JsonConvert.DeserializeObject<User>(exampleJson)
            : default(User);
            
            return new ObjectResult(example);
        }


        /// <summary>
        /// Updated user
        /// </summary>
        /// <remarks>This can only be done by the logged in user.</remarks>
        /// <param name="username">name that need to be deleted</param>
        /// <param name="body">Updated user object</param>
        /// <response code="400">Invalid user supplied</response>
        /// <response code="404">User not found</response>
        [HttpPut]
        [Route("/user/{username}")]
        [SwaggerOperation("UpdateUser")]
        public void UpdateUser([FromRoute]string username, [FromBody]User body)
        { 
            throw new NotImplementedException();
        }


        /// <summary>
        /// Delete user
        /// </summary>
        /// <remarks>This can only be done by the logged in user.</remarks>
        /// <param name="username">The name that needs to be deleted</param>
        /// <response code="400">Invalid username supplied</response>
        /// <response code="404">User not found</response>
        [HttpDelete]
        [Route("/user/{username}")]
        [SwaggerOperation("DeleteUser")]
        public void DeleteUser([FromRoute]string username)
        { 
            throw new NotImplementedException();
        }
    }
}
