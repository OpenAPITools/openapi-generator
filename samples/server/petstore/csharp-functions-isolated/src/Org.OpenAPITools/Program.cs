using Microsoft.Extensions.Hosting;

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
                .Build();

            host.Run();
        }
    }
}
