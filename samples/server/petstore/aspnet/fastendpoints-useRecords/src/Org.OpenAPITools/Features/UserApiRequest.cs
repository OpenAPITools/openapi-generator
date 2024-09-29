using FastEndpoints;

using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;

public record CreateUserRequest([property: FromBody] User user)
{
}

public record CreateUsersWithArrayInputRequest([property: FromBody] List<User> user)
{
}

public record CreateUsersWithListInputRequest([property: FromBody] List<User> user)
{
}

public record DeleteUserRequest([property: BindFrom("username")] string Username)
{
}

public record GetUserByNameRequest([property: BindFrom("username")] string Username)
{
}

public record LoginUserRequest([property: QueryParam, BindFrom("username")] string Username, [property: QueryParam, BindFrom("password")] string Password)
{
}

public record LogoutUserRequest()
{
}

public record UpdateUserRequest([property: BindFrom("username")] string Username, [property: FromBody] User user)
{
}




