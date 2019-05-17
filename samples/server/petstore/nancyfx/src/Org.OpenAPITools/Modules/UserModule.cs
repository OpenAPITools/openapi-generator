using System;
using Nancy;
using Nancy.ModelBinding;
using System.Collections.Generic;
using Sharpility.Base;
using Org.OpenAPITools.v2.Models;
using Org.OpenAPITools.v2.Utils;
using NodaTime;

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
            Post["/user"] = parameters =>
            {
                var user = this.Bind<User>();
                Preconditions.IsNotNull(user, "Required parameter: 'user' is missing at 'CreateUser'");
                
                service.CreateUser(Context, user);
                return new Response { ContentType = ""};
            };

            Post["/user/createWithArray"] = parameters =>
            {
                var user = this.Bind<List<User>>();
                Preconditions.IsNotNull(user, "Required parameter: 'user' is missing at 'CreateUsersWithArrayInput'");
                
                service.CreateUsersWithArrayInput(Context, user);
                return new Response { ContentType = ""};
            };

            Post["/user/createWithList"] = parameters =>
            {
                var user = this.Bind<List<User>>();
                Preconditions.IsNotNull(user, "Required parameter: 'user' is missing at 'CreateUsersWithListInput'");
                
                service.CreateUsersWithListInput(Context, user);
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
                var user = this.Bind<User>();
                Preconditions.IsNotNull(username, "Required parameter: 'username' is missing at 'UpdateUser'");
                
                Preconditions.IsNotNull(user, "Required parameter: 'user' is missing at 'UpdateUser'");
                
                service.UpdateUser(Context, username, user);
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
        void CreateUser(NancyContext context, User user);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="user">List of user object</param>
        /// <returns></returns>
        void CreateUsersWithArrayInput(NancyContext context, List<User> user);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="user">List of user object</param>
        /// <returns></returns>
        void CreateUsersWithListInput(NancyContext context, List<User> user);

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
        /// <param name="user">Updated user object</param>
        /// <returns></returns>
        void UpdateUser(NancyContext context, string username, User user);
    }

    /// <summary>
    /// Abstraction of UserService.
    /// </summary>
    public abstract class AbstractUserService: UserService
    {
        public virtual void CreateUser(NancyContext context, User user)
        {
            CreateUser(user);
        }

        public virtual void CreateUsersWithArrayInput(NancyContext context, List<User> user)
        {
            CreateUsersWithArrayInput(user);
        }

        public virtual void CreateUsersWithListInput(NancyContext context, List<User> user)
        {
            CreateUsersWithListInput(user);
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

        public virtual void UpdateUser(NancyContext context, string username, User user)
        {
            UpdateUser(username, user);
        }

        protected abstract void CreateUser(User user);

        protected abstract void CreateUsersWithArrayInput(List<User> user);

        protected abstract void CreateUsersWithListInput(List<User> user);

        protected abstract void DeleteUser(string username);

        protected abstract User GetUserByName(string username);

        protected abstract string LoginUser(string username, string password);

        protected abstract void LogoutUser();

        protected abstract void UpdateUser(string username, User user);
    }

}
