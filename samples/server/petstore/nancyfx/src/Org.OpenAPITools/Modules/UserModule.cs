using System;
using Nancy;
using Nancy.ModelBinding;
using System.Collections.Generic;
using Sharpility.Base;
using Org.OpenAPITools._v2.Models;
using Org.OpenAPITools._v2.Utils;
using NodaTime;

namespace Org.OpenAPITools._v2.Modules
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
            Post["/user"] = parameters =>
            {
                var body = this.Bind<User>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'CreateUser'");
                
                service.CreateUser(Context, body);
                return new Response { ContentType = ""};
            };

            Post["/user/createWithArray"] = parameters =>
            {
                var body = this.Bind<List<User>>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'CreateUsersWithArrayInput'");
                
                service.CreateUsersWithArrayInput(Context, body);
                return new Response { ContentType = ""};
            };

            Post["/user/createWithList"] = parameters =>
            {
                var body = this.Bind<List<User>>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'CreateUsersWithListInput'");
                
                service.CreateUsersWithListInput(Context, body);
                return new Response { ContentType = ""};
            };

            Delete["/user/{username}"] = parameters =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Path);
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'DeleteUser'");
                
                service.DeleteUser(Context, username);
                return new Response { ContentType = ""};
            };

            Get["/user/{username}"] = parameters =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Path);
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'GetUserByName'");
                
                return service.GetUserByName(Context, username);
            };

            Get["/user/login"] = parameters =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Query);
                var password = Parameters.ValueOf<string>(parameters, Context.Request, "password", ParameterType.Query);
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'LoginUser'");
                
                Preconditions.IsNotNull(password, "Required parameter: 'password' is missing at 'LoginUser'");
                
                return service.LoginUser(Context, username, password);
            };

            Get["/user/logout"] = parameters =>
            {
                
                service.LogoutUser(Context);
                return new Response { ContentType = ""};
            };

            Put["/user/{username}"] = parameters =>
            {
                var username = Parameters.ValueOf<string>(parameters, Context.Request, "username", ParameterType.Path);
                var body = this.Bind<User>();
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'UpdateUser'");
                
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'UpdateUser'");
                
                service.UpdateUser(Context, username, body);
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
        /// <param name="body">Created user object</param>
        /// <returns></returns>
        void CreateUser(NancyContext context, User body);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        void CreateUsersWithArrayInput(NancyContext context, List<User> body);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">List of user object</param>
        /// <returns></returns>
        void CreateUsersWithListInput(NancyContext context, List<User> body);

        /// <summary>
        /// This can only be done by the logged in user.
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="username">The name that needs to be deleted</param>
        /// <returns></returns>
        void DeleteUser(NancyContext context, string username);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="username">The name that needs to be fetched. Use user1 for testing.</param>
        /// <returns>User</returns>
        User GetUserByName(NancyContext context, string username);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="username">The user name for login</param>
        /// <param name="password">The password for login in clear text</param>
        /// <returns>string</returns>
        string LoginUser(NancyContext context, string username, string password);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <returns></returns>
        void LogoutUser(NancyContext context);

        /// <summary>
        /// This can only be done by the logged in user.
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="username">name that need to be deleted</param>
        /// <param name="body">Updated user object</param>
        /// <returns></returns>
        void UpdateUser(NancyContext context, string username, User body);
    }

    /// <summary>
    /// Abstraction of UserService.
    /// </summary>
    public abstract class AbstractUserService: UserService
    {
        public virtual void CreateUser(NancyContext context, User body)
        {
            CreateUser(body);
        }

        public virtual void CreateUsersWithArrayInput(NancyContext context, List<User> body)
        {
            CreateUsersWithArrayInput(body);
        }

        public virtual void CreateUsersWithListInput(NancyContext context, List<User> body)
        {
            CreateUsersWithListInput(body);
        }

        public virtual void DeleteUser(NancyContext context, string username)
        {
            DeleteUser(username);
        }

        public virtual User GetUserByName(NancyContext context, string username)
        {
            return GetUserByName(username);
        }

        public virtual string LoginUser(NancyContext context, string username, string password)
        {
            return LoginUser(username, password);
        }

        public virtual void LogoutUser(NancyContext context)
        {
            LogoutUser();
        }

        public virtual void UpdateUser(NancyContext context, string username, User body)
        {
            UpdateUser(username, body);
        }

        protected abstract void CreateUser(User body);

        protected abstract void CreateUsersWithArrayInput(List<User> body);

        protected abstract void CreateUsersWithListInput(List<User> body);

        protected abstract void DeleteUser(string username);

        protected abstract User GetUserByName(string username);

        protected abstract string LoginUser(string username, string password);

        protected abstract void LogoutUser();

        protected abstract void UpdateUser(string username, User body);
    }

}
