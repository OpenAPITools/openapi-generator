using System.Net;
using System.Net.Http;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Xunit;

namespace Org.OpenAPITools.Test.Client
{
    public sealed class ThrowOnAnyErrorTests
    {
        [Fact]
        public void TransportFailure_ThrowsApiExceptionWithStatusZero()
        {
            var api = new PetApi("http://127.0.0.1:" + FreePort());

            var ex = Assert.Throws<ApiException>(() => api.GetPetById(1));

            Assert.Equal(0, ex.ErrorCode);
            Assert.IsType<HttpRequestException>(ex.InnerException);
        }

        [Fact]
        public void MalformedBody_ThrowsApiExceptionWithHttpStatus()
        {
            var port = FreePort();
            using (var server = new HttpListener())
            {
                server.Prefixes.Add("http://127.0.0.1:" + port + "/");
                server.Start();
                Task.Run(() =>
                {
                    var ctx = server.GetContext();
                    var body = Encoding.UTF8.GetBytes("not json");
                    ctx.Response.ContentType = "application/json";
                    ctx.Response.OutputStream.Write(body, 0, body.Length);
                    ctx.Response.Close();
                });
                var api = new PetApi("http://127.0.0.1:" + port);

                var ex = Assert.Throws<ApiException>(() => api.GetPetById(1));

                Assert.Equal(200, ex.ErrorCode);
                Assert.Equal("not json", ex.ErrorContent);
            }
        }

        private static int FreePort()
        {
            var listener = new TcpListener(IPAddress.Loopback, 0);
            listener.Start();
            var port = ((IPEndPoint)listener.LocalEndpoint).Port;
            listener.Stop();
            return port;
        }
    }
}
