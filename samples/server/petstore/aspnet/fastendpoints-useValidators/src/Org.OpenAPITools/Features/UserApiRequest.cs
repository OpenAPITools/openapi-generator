using FastEndpoints;
using FluentValidation;
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
    [BindFrom("username")]
    public string Username { get; set; }
}
public class GetUserByNameRequest
{
    /// <summary>
    /// The name that needs to be fetched. Use user1 for testing.
    /// </summary>
    [BindFrom("username")]
    public string Username { get; set; }
}
public class LoginUserRequest
{
    /// <summary>
    /// The user name for login
    /// </summary>
    [QueryParam, BindFrom("username")]
    public string Username { get; set; }
    /// <summary>
    /// The password for login in clear text
    /// </summary>
    [QueryParam, BindFrom("password")]
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
    [BindFrom("username")]
    public string Username { get; set; }
    /// <summary>
    /// Updated user object
    /// </summary>
    [FromBody]
    public User user { get; set; }
}



public class CreateUserRequestValidator : Validator<CreateUserRequest>
{
    public CreateUserRequestValidator()
    {
        RuleFor(x => x.user).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class CreateUsersWithArrayInputRequestValidator : Validator<CreateUsersWithArrayInputRequest>
{
    public CreateUsersWithArrayInputRequestValidator()
    {
        RuleFor(x => x.user).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class CreateUsersWithListInputRequestValidator : Validator<CreateUsersWithListInputRequest>
{
    public CreateUsersWithListInputRequestValidator()
    {
        RuleFor(x => x.user).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class DeleteUserRequestValidator : Validator<DeleteUserRequest>
{
    public DeleteUserRequestValidator()
    {
        RuleFor(x => x.Username).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class GetUserByNameRequestValidator : Validator<GetUserByNameRequest>
{
    public GetUserByNameRequestValidator()
    {
        RuleFor(x => x.Username).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class LoginUserRequestValidator : Validator<LoginUserRequest>
{
    public LoginUserRequestValidator()
    {
        RuleFor(x => x.Username).NotEmpty();
        RuleFor(x => x.Password).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class LogoutUserRequestValidator : Validator<LogoutUserRequest>
{
    public LogoutUserRequestValidator()
    {
        //TODO Add any additional validation rules here
    }
}


public class UpdateUserRequestValidator : Validator<UpdateUserRequest>
{
    public UpdateUserRequestValidator()
    {
        RuleFor(x => x.Username).NotEmpty();
        RuleFor(x => x.user).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


