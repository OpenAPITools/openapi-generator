using Microsoft.Extensions.Hosting;
using Microsoft.Azure.Functions.Worker;
using Azure.Core.Serialization;

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
                .ConfigureFunctionsWorkerDefaults(options =>
                {
                    // Serialize with Newtonsoft.Json so the model attributes are honored.
                    options.Serializer = new NewtonsoftJsonObjectSerializer();
                })
                .Build();

            host.Run();
        }
    }
}
