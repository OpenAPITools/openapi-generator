using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.DependencyInjection;

namespace Org.OpenAPITools
{
    /// <summary>
    /// Host entry point for the Azure Functions .NET isolated worker.
    /// The OpenAPI/Swagger endpoints are contributed by the
    /// Microsoft.Azure.Functions.Worker.Extensions.OpenApi extension.
    /// </summary>
    public class Program
    {
        public static void Main(string[] args)
        {
            var host = new HostBuilder()
                .ConfigureFunctionsWebApplication()
                .ConfigureServices(services =>
                {
                    // Serialize IActionResult results with Newtonsoft.Json.
                    services.AddMvc().AddNewtonsoftJson();
                })
                .Build();

            host.Run();
        }
    }
}
