using FluentValidation;
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;


public class CreateUserRequest
{
    /// <summary>
    /// Created user object
    /// </summary>
    [FastEndpoints.FromBody]
    public User user { get; set; }
}
public class CreateUsersWithArrayInputRequest
{
    /// <summary>
    /// List of user object
    /// </summary>
    [FastEndpoints.FromBody]
    public List<User> user { get; set; }
}
public class CreateUsersWithListInputRequest
{
    /// <summary>
    /// List of user object
    /// </summary>
    [FastEndpoints.FromBody]
    public List<User> user { get; set; }
}
public class DeleteUserRequest
{
    /// <summary>
    /// The name that needs to be deleted
    /// </summary>
    [FastEndpoints.BindFrom("username")]
    public string Username { get; set; }
}
public class GetUserByNameRequest
{
    /// <summary>
    /// The name that needs to be fetched. Use user1 for testing.
    /// </summary>
    [FastEndpoints.BindFrom("username")]
    public string Username { get; set; }
}
public class LoginUserRequest
{
    /// <summary>
    /// The user name for login
    /// </summary>
    [FastEndpoints.QueryParam, FastEndpoints.BindFrom("username")]
    public string Username { get; set; }
    /// <summary>
    /// The password for login in clear text
    /// </summary>
    [FastEndpoints.QueryParam, FastEndpoints.BindFrom("password")]
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
    [FastEndpoints.BindFrom("username")]
    public string Username { get; set; }
    /// <summary>
    /// Updated user object
    /// </summary>
    [FastEndpoints.FromBody]
    public User user { get; set; }
}



public class CreateUserRequestValidator : FastEndpoints.Validator<CreateUserRequest>
{
    public CreateUserRequestValidator()
    {
        RuleFor(x => x.user).NotEmpty();
    }
}


public class CreateUsersWithArrayInputRequestValidator : FastEndpoints.Validator<CreateUsersWithArrayInputRequest>
{
    public CreateUsersWithArrayInputRequestValidator()
    {
        RuleFor(x => x.user).NotEmpty();
    }
}


public class CreateUsersWithListInputRequestValidator : FastEndpoints.Validator<CreateUsersWithListInputRequest>
{
    public CreateUsersWithListInputRequestValidator()
    {
        RuleFor(x => x.user).NotEmpty();
    }
}


public class DeleteUserRequestValidator : FastEndpoints.Validator<DeleteUserRequest>
{
    public DeleteUserRequestValidator()
    {
        RuleFor(x => x.Username).NotEmpty();
    }
}


public class GetUserByNameRequestValidator : FastEndpoints.Validator<GetUserByNameRequest>
{
    public GetUserByNameRequestValidator()
    {
        RuleFor(x => x.Username).NotEmpty();
    }
}


public class LoginUserRequestValidator : FastEndpoints.Validator<LoginUserRequest>
{
    public LoginUserRequestValidator()
    {
        RuleFor(x => x.Username).NotEmpty();
        RuleFor(x => x.Password).NotEmpty();
    }
}


public class LogoutUserRequestValidator : FastEndpoints.Validator<LogoutUserRequest>
{
    public LogoutUserRequestValidator()
    {
    }
}


public class UpdateUserRequestValidator : FastEndpoints.Validator<UpdateUserRequest>
{
    public UpdateUserRequestValidator()
    {
        RuleFor(x => x.Username).NotEmpty();
        RuleFor(x => x.user).NotEmpty();
    }
}


