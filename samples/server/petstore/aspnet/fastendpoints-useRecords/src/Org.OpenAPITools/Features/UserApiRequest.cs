
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;

public record CreateUserRequest([property: FastEndpoints.FromBody] User user)
{
}

public record CreateUsersWithArrayInputRequest([property: FastEndpoints.FromBody] List<User> user)
{
}

public record CreateUsersWithListInputRequest([property: FastEndpoints.FromBody] List<User> user)
{
}

public record DeleteUserRequest([property: FastEndpoints.BindFrom("username")] string Username)
{
}

public record GetUserByNameRequest([property: FastEndpoints.BindFrom("username")] string Username)
{
}

public record LoginUserRequest([property: FastEndpoints.QueryParam, FastEndpoints.BindFrom("username")] string Username, [property: FastEndpoints.QueryParam, FastEndpoints.BindFrom("password")] string Password)
{
}

public record LogoutUserRequest()
{
}

public record UpdateUserRequest([property: FastEndpoints.BindFrom("username")] string Username, [property: FastEndpoints.FromBody] User user)
{
}




