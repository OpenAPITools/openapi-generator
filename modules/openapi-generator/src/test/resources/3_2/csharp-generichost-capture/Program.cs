// Wire-level verification harness for OpenAPI 3.2 support in the generated
// `csharp` (generichost) client. CSharpClientCodegenTest copies this directory
// into the generated client directory and runs it with `dotnet run`.
//
// A raw TcpListener records the HTTP request line for every generated call:
// standard methods keep HttpMethod.* dispatch, while query/additionalOperations
// methods and `in: querystring` parameters must reach the wire verbatim
// (no normalization, no re-encoding).
using System.Collections.Concurrent;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Text.Json;
using Microsoft.Extensions.Logging.Abstractions;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;

var listener = new TcpListener(IPAddress.Loopback, 0);
listener.Start();
int port = ((IPEndPoint)listener.LocalEndpoint).Port;
var lines = new ConcurrentQueue<string>();
var cts = new CancellationTokenSource(TimeSpan.FromSeconds(60));

var server = Task.Run(async () =>
{
    while (!cts.IsCancellationRequested)
    {
        TcpClient conn;
        try { conn = await listener.AcceptTcpClientAsync(cts.Token); }
        catch (OperationCanceledException) { break; }
        try
        {
            using var stream = conn.GetStream();
            using var reader = new StreamReader(stream, Encoding.ASCII, false, 1024, true);
            var requestLine = await reader.ReadLineAsync(cts.Token);
            if (requestLine != null) lines.Enqueue(requestLine);
            // consume headers (no body in this fixture)
            while (await reader.ReadLineAsync(cts.Token) is { } h && h.Length > 0) { }
            var resp = Encoding.ASCII.GetBytes("HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n");
            await stream.WriteAsync(resp, cts.Token);
        }
        catch (OperationCanceledException) { break; }
        finally { conn.Close(); }
    }
});

using var handler = new SocketsHttpHandler { UseProxy = false, AllowAutoRedirect = false };
using var httpClient = new HttpClient(handler)
{
    BaseAddress = new Uri($"http://127.0.0.1:{port}"),
    DefaultRequestVersion = HttpVersion.Version11
};
var api = new DefaultApi(
    NullLogger<DefaultApi>.Instance,
    httpClient,
    new JsonSerializerOptionsProvider(new JsonSerializerOptions()),
    new DefaultApiEvents());

// `in: querystring` callers pass the query component without the leading `?`
await api.QueryPetsAsync("a=1&b=%20x", cts.Token);
await api.CustomPetsAsync(cts.Token);
await api.CheckFetchPetsAsync(cts.Token);
await api.PurgePetsAsync(cts.Token);
await api.HashPetsAsync(cts.Token);
await api.ListPetsAsync(cts.Token);

var expected = new[]
{
    "QUERY /pets?a=1&b=%20x HTTP/1.1",
    "customMethod /pets HTTP/1.1",
    "CHECK&FETCH /pets HTTP/1.1",
    "PURGE /pets HTTP/1.1",
    "X#Y /pets HTTP/1.1",
    "GET /pets HTTP/1.1"
};
cts.Cancel();
try { await server; } catch (OperationCanceledException) { }

var got = lines.ToArray();
var ok = got.SequenceEqual(expected);
for (int i = 0; i < expected.Length; i++)
{
    var actual = i < got.Length ? got[i] : "<none>";
    if (actual != expected[i])
        Console.Error.WriteLine($"FAIL: got {actual}, want {expected[i]}");
}
Console.WriteLine(ok ? "CAPTURE-PASS" : "CAPTURE-FAIL");
return ok ? 0 : 1;
