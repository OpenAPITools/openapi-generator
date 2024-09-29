using FastEndpoints;

using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;


public class CreateUserRequest
{
    /// <summary>
    /// Created user object
    /// </summary>
    [FromBody]
    public User user { get; set; }
}
public class CreateUsersWithArrayInputRequest
{
    /// <summary>
    /// List of user object
    /// </summary>
    [FromBody]
    public List<User> user { get; set; }
}
public class CreateUsersWithListInputRequest
{
    /// <summary>
    /// List of user object
    /// </summary>
    [FromBody]
    public List<User> user { get; set; }
}
public class DeleteUserRequest
{
    /// <summary>
    /// The name that needs to be deleted
    /// </summary>
    public string Username { get; set; }
}
public class GetUserByNameRequest
{
    /// <summary>
    /// The name that needs to be fetched. Use user1 for testing.
    /// </summary>
    public string Username { get; set; }
}
public class LoginUserRequest
{
    /// <summary>
    /// The user name for login
    /// </summary>
    [QueryParam]
    public string Username { get; set; }
    /// <summary>
    /// The password for login in clear text
    /// </summary>
    [QueryParam]
    public string Password { get; set; }
}
public class LogoutUserRequest
{
}
public class UpdateUserRequest
{
    /// <summary>
    /// name that need to be deleted
    /// </summary>
    public string Username { get; set; }
    /// <summary>
    /// Updated user object
    /// </summary>
    [FromBody]
    public User user { get; set; }
}


