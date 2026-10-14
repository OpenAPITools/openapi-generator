using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using Org.OpenAPITools.Api;
using Xunit;

namespace Org.OpenAPITools.Test.Client
{
    public sealed class AcceptHeaderTests
    {
        [Fact]
        public void MixedResponseTypes_SendsAllAcceptTypesJsonFirst()
        {
            var port = FreePort();
            using (var server = new HttpListener())
            {
                server.Prefixes.Add("http://127.0.0.1:" + port + "/");
                server.Start();
                var accept = Task.Run(() =>
                {
                    var ctx = server.GetContext();
                    var sent = ctx.Request.Headers["Accept"];
                    var body = Encoding.UTF8.GetBytes("{\"name\":\"doggie\",\"photoUrls\":[]}");
                    ctx.Response.ContentType = "application/json";
                    ctx.Response.OutputStream.Write(body, 0, body.Length);
                    ctx.Response.Close();
                    return sent;
                });

                new PetApi("http://127.0.0.1:" + port).GetPetById(1);

                Assert.Equal("application/json,application/xml", accept.Result);
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
