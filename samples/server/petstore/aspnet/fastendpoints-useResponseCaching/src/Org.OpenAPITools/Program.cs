using FastEndpoints;
using FastEndpoints.Swagger;

var builder = WebApplication.CreateBuilder(args);

builder.Services
    
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
    .AddResponseCaching()
;

var app = builder.Build();

app
    
    .UseResponseCaching()
    .UseFastEndpoints(x =>
    {
        
    })
    .UseSwaggerGen();

app.UseHttpsRedirection();

app.Run();