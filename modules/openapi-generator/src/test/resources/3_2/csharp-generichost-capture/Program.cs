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
            var cl = 0;
            while (await reader.ReadLineAsync(cts.Token) is { } h && h.Length > 0)
            {
                if (h.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase))
                    cl = int.Parse(h.Substring(15).Trim());
            }
            var bodyBuf = new char[cl];
            var read = 0;
            while (read < cl) read += await reader.ReadBlockAsync(bodyBuf.AsMemory(read), cts.Token);
            var body = new string(bodyBuf);
            if (requestLine != null)
                lines.Enqueue($"{requestLine} [CL={cl}]" + (body.Length > 0 ? $" BODY={body}" : ""));
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

// `in: querystring` callers pass the query component without the leading `?`;
// embedded `//` must reach the wire intact.
await api.QueryPetsAsync("a=1&u=http://h//p", cts.Token);
await api.CustomPetsAsync(cts.Token);
await api.CheckFetchPetsAsync(cts.Token);
await api.PurgePetsAsync(cts.Token);
await api.HashPetsAsync(cts.Token);
await api.ListPetsAsync(cts.Token);
// GET with an `in: querystring` parameter
await api.FindPetsAsync("z=9", cts.Token);
// QUERY carrying a request body; its querystring param is named `uri`
await api.SearchItemsAsync("k=v", new Dictionary<string, object> { ["a"] = 1 }, cts.Token);
// additionalOperations: REPORT with querystring, PROPPATCH with a body
await api.ReportItemsAsync("r=1", cts.Token);
await api.PropPatchAsync("<x/>", cts.Token);

var expected = new[]
{
    "QUERY /pets?a=1&u=http://h//p HTTP/1.1 [CL=0]",
    "customMethod /pets HTTP/1.1 [CL=0]",
    "CHECK&FETCH /pets HTTP/1.1 [CL=0]",
    "PURGE /pets HTTP/1.1 [CL=0]",
    "X#Y /pets HTTP/1.1 [CL=0]",
    "GET /pets HTTP/1.1 [CL=0]",
    "GET /find?z=9 HTTP/1.1 [CL=0]",
    "QUERY /items?k=v HTTP/1.1 [CL=7] BODY={\"a\":1}",
    "REPORT /report?r=1 HTTP/1.1 [CL=0]",
    "PROPPATCH /report HTTP/1.1 [CL=16] BODY=\"\\u003Cx/\\u003E\""
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
