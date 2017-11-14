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
    /// Module processing requests of User domain.
    /// </summary>
    public sealed class UserModule : NancyModule
    {
        /// <summary>
        /// Sets up HTTP methods mappings.
        /// </summary>
        /// <param name="service">Service handling requests</param>
        public UserModule(UserService service) : base("/v2")
        { 
            Post["/user", true] = async (parameters, ct) =>
            {
                var body = this.Bind<User>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'CreateUser'");
                
                await service.CreateUser(Context, body);
                return new Response { ContentType = "application/xml"};
            };

            Post["/user/createWithArray", true] = async (parameters, ct) =>
            {
                var body = this.Bind<List<User>>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'CreateUsersWithArrayInput'");
                
                await service.CreateUsersWithArrayInput(Context, body);
                return new Response { ContentType = "application/xml"};
            };

            Post["/user/createWithList", true] = async (parameters, ct) =>
            {
                var body = this.Bind<List<User>>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'CreateUsersWithListInput'");
                
                await service.CreateUsersWithListInput(Context, body);
                return new Response { ContentType = "application/xml"};
            };

            Delete["/user/{username}", true] = async (parameters, ct) =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Path);
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'DeleteUser'");
                
                await service.DeleteUser(Context, username);
                return new Response { ContentType = "application/xml"};
            };

            Get["/user/{username}", true] = async (parameters, ct) =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Path);
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'GetUserByName'");
                
                return await service.GetUserByName(Context, username);
            };

            Get["/user/login", true] = async (parameters, ct) =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Query);
                var password = Parameters.ValueOf<string>(parameters, Context.Request, "password", ParameterType.Query);
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'LoginUser'");
                
                Preconditions.IsNotNull(password, "Required parameter: 'password' is missing at 'LoginUser'");
                
                return await service.LoginUser(Context, username, password);
            };

            Get["/user/logout", true] = async (parameters, ct) =>
            {
                
                await service.LogoutUser(Context);
                return new Response { ContentType = "application/xml"};
            };

            Put["/user/{username}", true] = async (parameters, ct) =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Path);
                var body = this.Bind<User>();
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'UpdateUser'");
                
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'UpdateUser'");
                
                await service.UpdateUser(Context, username, body);
                return new Response { ContentType = "application/xml"};
            };
        }
    }

    /// <summary>
    /// Service handling User requests.
    /// </summary>
    public interface UserService
    {
        /// <summary>
        /// This can only be done by the logged in user.
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">Created user object</param>
        /// <returns></returns>
        Task CreateUser(NancyContext context, User body);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        Task CreateUsersWithArrayInput(NancyContext context, List<User> body);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        Task CreateUsersWithListInput(NancyContext context, List<User> body);

        /// <summary>
        /// This can only be done by the logged in user.
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="username">The name that needs to be deleted</param>
        /// <returns></returns>
        Task DeleteUser(NancyContext context, string username);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing. </param>
        /// <returns>User</returns>
        Task<User> GetUserByName(NancyContext context, string username);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="username">The user name for login</param>
        /// <param name="password">The password for login in clear text</param>
        /// <returns>string</returns>
        Task<string> LoginUser(NancyContext context, string username, string password);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <returns></returns>
        Task LogoutUser(NancyContext context);

        /// <summary>
        /// This can only be done by the logged in user.
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="username">name that need to be deleted</param>
        /// <param name="body">Updated user object</param>
        /// <returns></returns>
        Task UpdateUser(NancyContext context, string username, User body);
    }

    /// <summary>
    /// Abstraction of UserService.
    /// </summary>
    public abstract class AbstractUserService: UserService
    {
        public virtual Task CreateUser(NancyContext context, User body)
        {
            return CreateUser(body);
        }

        public virtual Task CreateUsersWithArrayInput(NancyContext context, List<User> body)
        {
            return CreateUsersWithArrayInput(body);
        }

        public virtual Task CreateUsersWithListInput(NancyContext context, List<User> body)
        {
            return CreateUsersWithListInput(body);
        }

        public virtual Task DeleteUser(NancyContext context, string username)
        {
            return DeleteUser(username);
        }

        public virtual Task<User> GetUserByName(NancyContext context, string username)
        {
            return GetUserByName(username);
        }

        public virtual Task<string> LoginUser(NancyContext context, string username, string password)
        {
            return LoginUser(username, password);
        }

        public virtual Task LogoutUser(NancyContext context)
        {
            return LogoutUser();
        }

        public virtual Task UpdateUser(NancyContext context, string username, User body)
        {
            return UpdateUser(username, body);
        }

        protected abstract Task CreateUser(User body);

        protected abstract Task CreateUsersWithArrayInput(List<User> body);

        protected abstract Task CreateUsersWithListInput(List<User> body);

        protected abstract Task DeleteUser(string username);

        protected abstract Task<User> GetUserByName(string username);

        protected abstract Task<string> LoginUser(string username, string password);

        protected abstract Task LogoutUser();

        protected abstract Task UpdateUser(string username, User body);
    }

}
