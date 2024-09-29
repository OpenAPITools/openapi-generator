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

public record DeleteUserRequest(string Username)
{
}

public record GetUserByNameRequest(string Username)
{
}

public record LoginUserRequest([property: QueryParam] string Username, [property: QueryParam] string Password)
{
}

public record LogoutUserRequest()
{
}

public record UpdateUserRequest(string Username, [property: FromBody] User user)
{
}




