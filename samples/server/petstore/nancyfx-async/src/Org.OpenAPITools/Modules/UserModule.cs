using System;
using Nancy;
using Nancy.ModelBinding;
using System.Collections.Generic;
using Sharpility.Base;
using Org.OpenAPITools.v2.Models;
using Org.OpenAPITools.v2.Utils;
using NodaTime;
using System.Threading.Tasks;

namespace Org.OpenAPITools.v2.Modules
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
                var user = this.Bind<User>();
                Preconditions.IsNotNull(user, "Required parameter: 'user' is missing at 'CreateUser'");
                
                await service.CreateUser(Context, user);
                return new Response { ContentType = ""};
            };

            Post["/user/createWithArray", true] = async (parameters, ct) =>
            {
                var user = this.Bind<List<User>>();
                Preconditions.IsNotNull(user, "Required parameter: 'user' is missing at 'CreateUsersWithArrayInput'");
                
                await service.CreateUsersWithArrayInput(Context, user);
                return new Response { ContentType = ""};
            };

            Post["/user/createWithList", true] = async (parameters, ct) =>
            {
                var user = this.Bind<List<User>>();
                Preconditions.IsNotNull(user, "Required parameter: 'user' is missing at 'CreateUsersWithListInput'");
                
                await service.CreateUsersWithListInput(Context, user);
                return new Response { ContentType = ""};
            };

            Delete["/user/{username}", true] = async (parameters, ct) =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Path);
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'DeleteUser'");
                
                await service.DeleteUser(Context, username);
                return new Response { ContentType = ""};
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
                return new Response { ContentType = ""};
            };

            Put["/user/{username}", true] = async (parameters, ct) =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Path);
                var user = this.Bind<User>();
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'UpdateUser'");
                
                Preconditions.IsNotNull(user, "Required parameter: 'user' is missing at 'UpdateUser'");
                
                await service.UpdateUser(Context, username, user);
                return new Response { ContentType = ""};
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
        /// <param name="user">Created user object</param>
        /// <returns></returns>
        Task CreateUser(NancyContext context, User user);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="user">List of user object</param>
        /// <returns></returns>
        Task CreateUsersWithArrayInput(NancyContext context, List<User> user);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="user">List of user object</param>
        /// <returns></returns>
        Task CreateUsersWithListInput(NancyContext context, List<User> user);

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
        /// <param name="username">The name that needs to be fetched. Use user1 for testing.</param>
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
        /// <param name="user">Updated user object</param>
        /// <returns></returns>
        Task UpdateUser(NancyContext context, string username, User user);
    }

    /// <summary>
    /// Abstraction of UserService.
    /// </summary>
    public abstract class AbstractUserService: UserService
    {
        public virtual Task CreateUser(NancyContext context, User user)
        {
            return CreateUser(user);
        }

        public virtual Task CreateUsersWithArrayInput(NancyContext context, List<User> user)
        {
            return CreateUsersWithArrayInput(user);
        }

        public virtual Task CreateUsersWithListInput(NancyContext context, List<User> user)
        {
            return CreateUsersWithListInput(user);
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

        public virtual Task UpdateUser(NancyContext context, string username, User user)
        {
            return UpdateUser(username, user);
        }

        protected abstract Task CreateUser(User user);

        protected abstract Task CreateUsersWithArrayInput(List<User> user);

        protected abstract Task CreateUsersWithListInput(List<User> user);

        protected abstract Task DeleteUser(string username);

        protected abstract Task<User> GetUserByName(string username);

        protected abstract Task<string> LoginUser(string username, string password);

        protected abstract Task LogoutUser();

        protected abstract Task UpdateUser(string username, User user);
    }

}
