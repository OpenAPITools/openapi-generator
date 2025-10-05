using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;



/// <summary>
/// Create user
/// </summary>

public class CreateUserEndpoint : FastEndpoints.Endpoint<CreateUserRequest>
{
    public override void Configure()
    {
        Post("/v2/user");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("user");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 0);
        });

        Summary(s => {
            s.Summary = "Create user";
            s.RequestParam(r => r.user, "Created user object");
            s.Responses[0] = "successful operation";
        });
    }

    public override async Task HandleAsync(CreateUserRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Creates list of users with given input array
/// </summary>

public class CreateUsersWithArrayInputEndpoint : FastEndpoints.Endpoint<CreateUsersWithArrayInputRequest>
{
    public override void Configure()
    {
        Post("/v2/user/createWithArray");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("user");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 0);
        });

        Summary(s => {
            s.Summary = "Creates list of users with given input array";
            s.RequestParam(r => r.user, "List of user object");
            s.Responses[0] = "successful operation";
        });
    }

    public override async Task HandleAsync(CreateUsersWithArrayInputRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Creates list of users with given input array
/// </summary>

public class CreateUsersWithListInputEndpoint : FastEndpoints.Endpoint<CreateUsersWithListInputRequest>
{
    public override void Configure()
    {
        Post("/v2/user/createWithList");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("user");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 0);
        });

        Summary(s => {
            s.Summary = "Creates list of users with given input array";
            s.RequestParam(r => r.user, "List of user object");
            s.Responses[0] = "successful operation";
        });
    }

    public override async Task HandleAsync(CreateUsersWithListInputRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Delete user
/// </summary>

public class DeleteUserEndpoint : FastEndpoints.Endpoint<DeleteUserRequest>
{
    public override void Configure()
    {
        Delete("/v2/user/{username}");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("user");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 400);
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 404);
        });

        Summary(s => {
            s.Summary = "Delete user";
            s.RequestParam(r => r.Username, "The name that needs to be deleted");
            s.Responses[400] = "Invalid username supplied";
            s.Responses[404] = "User not found";
        });
    }

    public override async Task HandleAsync(DeleteUserRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Get user by user name
/// </summary>

public class GetUserByNameEndpoint : FastEndpoints.Endpoint<GetUserByNameRequest, User>
{
    public override void Configure()
    {
        Get("/v2/user/{username}");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("user");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 400);
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 404);
        });

        Summary(s => {
            s.Summary = "Get user by user name";
            s.RequestParam(r => r.Username, "The name that needs to be fetched. Use user1 for testing.");
            s.Responses[200] = "successful operation";
            s.Responses[400] = "Invalid username supplied";
            s.Responses[404] = "User not found";
        });
    }

    public override async Task HandleAsync(GetUserByNameRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Logs user into the system
/// </summary>

public class LoginUserEndpoint : FastEndpoints.Endpoint<LoginUserRequest, string>
{
    public override void Configure()
    {
        Get("/v2/user/login");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("user");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 400);
        });

        Summary(s => {
            s.Summary = "Logs user into the system";
            s.RequestParam(r => r.Username, "The user name for login");
            s.RequestParam(r => r.Password, "The password for login in clear text");
            s.Responses[200] = "successful operation";
            s.Responses[400] = "Invalid username/password supplied";
        });
    }

    public override async Task HandleAsync(LoginUserRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Logs out current logged in user session
/// </summary>

public class LogoutUserEndpoint : FastEndpoints.EndpointWithoutRequest
{
    public override void Configure()
    {
        Get("/v2/user/logout");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("user");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 0);
        });

        Summary(s => {
            s.Summary = "Logs out current logged in user session";
            s.Responses[0] = "successful operation";
        });
    }

    public override async Task HandleAsync(CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Updated user
/// </summary>

public class UpdateUserEndpoint : FastEndpoints.Endpoint<UpdateUserRequest>
{
    public override void Configure()
    {
        Put("/v2/user/{username}");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("user");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 400);
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemDetails(x, 404);
        });

        Summary(s => {
            s.Summary = "Updated user";
            s.RequestParam(r => r.Username, "name that need to be deleted");
            s.RequestParam(r => r.user, "Updated user object");
            s.Responses[400] = "Invalid user supplied";
            s.Responses[404] = "User not found";
        });
    }

    public override async Task HandleAsync(UpdateUserRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}

