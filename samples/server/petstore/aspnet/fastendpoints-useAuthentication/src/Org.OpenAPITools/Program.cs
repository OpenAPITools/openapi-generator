using FastEndpoints;
using FastEndpoints.Security;
using FastEndpoints.Swagger;

var builder = WebApplication.CreateBuilder(args);

builder.Services
    .AddAuthenticationJwtBearer(s => s.SigningKey = "The secret used to sign tokens") //TODO set the signing key
    .AddAuthorization()
    .AddFastEndpoints()
    .SwaggerDocument(o =>
    {
        o.DocumentSettings = s =>
        {
            s.DocumentName = "OpenAPI Petstore";
            s.Title = "OpenAPI Petstore";
            s.Description = """
This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
""";
            s.Version = "1.0.0";
        };
        o.AutoTagPathSegmentIndex = 0;
    })
    
;

var app = builder.Build();

app
    .UseAuthentication()
    .UseAuthorization()
    .UseFastEndpoints(x =>
    {
        
    })
    .UseSwaggerGen();

app.UseHttpsRedirection();

app.Run();