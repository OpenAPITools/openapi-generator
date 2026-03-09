using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Extensions;
using System.Net;
using System.Net.Http.Headers;

namespace OpenAPIClient_generichost_manual_tests
{
    /// <summary>
    /// Tests that multipart/form-data requests preserve the boundary in their Content-Type header.
    /// Regression test for https://github.com/OpenAPITools/openapi-generator/issues/21384
    /// </summary>
    [TestClass]
    public sealed class MultipartContentTypeTests
    {
        /// <summary>
        /// Captures the outgoing HttpRequestMessage and returns a minimal fake response,
        /// so tests can inspect what the generated client actually sends without a real server.
        /// </summary>
        private sealed class CapturingHandler : HttpMessageHandler
        {
            public HttpRequestMessage? CapturedRequest { get; private set; }
            public string? CapturedContentType { get; private set; }
            public string? CapturedBody { get; private set; }

            protected override async Task<HttpResponseMessage> SendAsync(
                HttpRequestMessage request, CancellationToken cancellationToken)
            {
                CapturedRequest = request;
                CapturedContentType = request.Content?.Headers.ContentType?.ToString();
                CapturedBody = request.Content is not null
                    ? await request.Content.ReadAsStringAsync(cancellationToken)
                    : null;

                var response = new HttpResponseMessage(HttpStatusCode.OK)
                {
                    Content = new StringContent(
                        """{"code":200,"type":"ok","message":"ok"}""",
                        System.Text.Encoding.UTF8,
                        "application/json")
                };

                return response;
            }
        }

        private IHost _host = null!;
        private CapturingHandler _capturingHandler = null!;

        [TestInitialize]
        public void Setup()
        {
            _capturingHandler = new CapturingHandler();

            IHostBuilder hostBuilder = Host.CreateDefaultBuilder(Array.Empty<string>())
                .ConfigureApi((context, services, options) =>
                {
                    options.AddTokens(new ApiKeyToken("test-key", ClientUtils.ApiKeyHeader.Api_key, timeout: TimeSpan.FromSeconds(1)));
                    options.AddTokens(new BearerToken("test-bearer", timeout: TimeSpan.FromSeconds(1)));
                    options.AddTokens(new BasicToken("user", "pass", timeout: TimeSpan.FromSeconds(1)));
                    options.AddTokens(new OAuthToken("test-oauth", timeout: TimeSpan.FromSeconds(1)));

                    HttpSigningConfiguration signingConfig = new("<keyId>", "<keyFilePath>", null,
                        new List<string>(), System.Security.Cryptography.HashAlgorithmName.SHA256, "<algo>", 0);
                    options.AddTokens(new HttpSignatureToken(signingConfig, timeout: TimeSpan.FromSeconds(1)));

                    options.AddApiHttpClients(
                        client: c => c.BaseAddress = new Uri("http://localhost/"),
                        builder: b => b.ConfigurePrimaryHttpMessageHandler(() => _capturingHandler));
                });

            _host = hostBuilder.Build();
        }

        [TestCleanup]
        public void Cleanup()
        {
            _host.Dispose();
            _capturingHandler.Dispose();
        }

        [TestMethod]
        public async Task UploadFile_SingleOptionalFile_ContentTypeHasBoundary()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream = new MemoryStream(new byte[] { 1, 2, 3 });

            await petApi.UploadFileAsync(petId: 1, file: new Option<FileParameter>(new FileParameter(stream)));

            Assert.IsNotNull(_capturingHandler.CapturedContentType,
                "Content-Type header should not be null for a multipart/form-data request.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "multipart/form-data",
                "Content-Type should be multipart/form-data.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "boundary=",
                "Content-Type must include a boundary parameter so the server can parse the body.");
        }

        [TestMethod]
        public async Task UploadFile_SingleOptionalFile_BodyContainsFileWithCorrectFieldName()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream = new MemoryStream(new byte[] { 1, 2, 3 });

            await petApi.UploadFileAsync(petId: 1, file: new Option<FileParameter>(new FileParameter(stream)));

            Assert.IsNotNull(_capturingHandler.CapturedBody, "Body should not be null.");
            // .NET's MultipartFormDataContent writes unquoted names: name=file (valid per RFC 7578)
            StringAssert.Contains(_capturingHandler.CapturedBody, "name=file",
                "Multipart part should use the field name 'file' as defined in the spec.");
        }

        [TestMethod]
        public async Task UploadFiles_MultipleRequiredFiles_ContentTypeHasBoundary()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream1 = new MemoryStream(new byte[] { 1, 2, 3 });
            using var stream2 = new MemoryStream(new byte[] { 4, 5, 6 });

            await petApi.UploadFilesAsync(
                files: new List<FileParameter> { new FileParameter(stream1), new FileParameter(stream2) },
                petId: 1);

            Assert.IsNotNull(_capturingHandler.CapturedContentType,
                "Content-Type header should not be null for a multipart/form-data request.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "multipart/form-data",
                "Content-Type should be multipart/form-data.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "boundary=",
                "Content-Type must include a boundary parameter so the server can parse the body.");
        }

        [TestMethod]
        public async Task UploadFiles_MultipleRequiredFiles_BodyContainsBothFilesWithCorrectFieldName()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream1 = new MemoryStream(new byte[] { 10, 20, 30 });
            using var stream2 = new MemoryStream(new byte[] { 40, 50, 60 });

            await petApi.UploadFilesAsync(
                files: new List<FileParameter> { new FileParameter(stream1), new FileParameter(stream2) },
                petId: 1);

            Assert.IsNotNull(_capturingHandler.CapturedBody, "Body should not be null.");

            // Count Content-Disposition headers for the "files" field. Each stream must appear as
            // its own named part. Matching the full header prefix avoids false positives from
            // "filename=files" which also contains "name=files" as a substring.
            var parts = _capturingHandler.CapturedBody!.Split("Content-Disposition: form-data; name=files");
            Assert.AreEqual(3, parts.Length,
                "Body should contain exactly two parts named 'files' (one per stream), " +
                $"but found {parts.Length - 1}. Body:\n{_capturingHandler.CapturedBody}");
        }

        /// <summary>
        /// Covers issue #21384: multipart/form-data with only text form fields (no file)
        /// must still include the boundary in Content-Type so the server can parse the body.
        /// Uses UploadFileAsync with only the optional additionalMetadata text field, no file.
        /// </summary>
        [TestMethod]
        public async Task UploadFile_TextFieldOnly_NoFile_ContentTypeHasBoundary()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();

            await petApi.UploadFileAsync(
                petId: 1,
                additionalMetadata: new Option<string>("some metadata"));

            Assert.IsNotNull(_capturingHandler.CapturedContentType,
                "Content-Type header should not be null for a multipart/form-data request.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "multipart/form-data",
                "Content-Type should be multipart/form-data.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "boundary=",
                "Content-Type must include a boundary parameter so the server can parse the body.");
        }

        [TestMethod]
        public async Task UploadFile_TextFieldOnly_NoFile_BodyContainsTextValue()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();

            await petApi.UploadFileAsync(
                petId: 1,
                additionalMetadata: new Option<string>("testmetadata"));

            Assert.IsNotNull(_capturingHandler.CapturedBody, "Body should not be null.");
            StringAssert.Contains(_capturingHandler.CapturedBody, "testmetadata",
                "Body should contain the additionalMetadata value.");
        }

        /// <summary>
        /// Covers the case where an optional file and an optional text field are both supplied.
        /// Ensures that adding text alongside a file does not strip the boundary.
        /// </summary>
        [TestMethod]
        public async Task UploadFile_WithBothFileAndText_ContentTypeHasBoundary()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream = new MemoryStream(new byte[] { 1, 2, 3 });

            await petApi.UploadFileAsync(
                petId: 1,
                file: new Option<FileParameter>(new FileParameter(stream)),
                additionalMetadata: new Option<string>("testmetadata"));

            Assert.IsNotNull(_capturingHandler.CapturedContentType,
                "Content-Type header should not be null.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "multipart/form-data",
                "Content-Type should be multipart/form-data.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "boundary=",
                "Content-Type must include a boundary parameter.");
        }

        [TestMethod]
        public async Task UploadFile_WithBothFileAndText_BodyContainsBothFields()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream = new MemoryStream(new byte[] { 1, 2, 3 });

            await petApi.UploadFileAsync(
                petId: 1,
                file: new Option<FileParameter>(new FileParameter(stream)),
                additionalMetadata: new Option<string>("testmetadata"));

            Assert.IsNotNull(_capturingHandler.CapturedBody, "Body should not be null.");
            StringAssert.Contains(_capturingHandler.CapturedBody, "name=file",
                "Body should contain a part named 'file'.");
            StringAssert.Contains(_capturingHandler.CapturedBody, "testmetadata",
                "Body should contain the additionalMetadata value.");
        }

        /// <summary>
        /// Covers UploadFileWithRequiredFile: a required (non-optional) binary file param.
        /// Tests the required-file code path in the template, which differs from optional files.
        /// </summary>
        [TestMethod]
        public async Task UploadFileWithRequiredFile_RequiredFileOnly_ContentTypeHasBoundary()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream = new MemoryStream(new byte[] { 7, 8, 9 });

            await petApi.UploadFileWithRequiredFileAsync(petId: 1, requiredFile: new FileParameter(stream));

            Assert.IsNotNull(_capturingHandler.CapturedContentType,
                "Content-Type header should not be null.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "multipart/form-data",
                "Content-Type should be multipart/form-data.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "boundary=",
                "Content-Type must include a boundary parameter.");
        }

        [TestMethod]
        public async Task UploadFileWithRequiredFile_RequiredFileOnly_BodyContainsCorrectFieldName()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream = new MemoryStream(new byte[] { 7, 8, 9 });

            await petApi.UploadFileWithRequiredFileAsync(petId: 1, requiredFile: new FileParameter(stream));

            Assert.IsNotNull(_capturingHandler.CapturedBody, "Body should not be null.");
            StringAssert.Contains(_capturingHandler.CapturedBody, "name=requiredFile",
                "Multipart part should use the field name 'requiredFile' as defined in the spec.");
        }

        [TestMethod]
        public async Task UploadFileWithRequiredFile_WithTextAndFile_ContentTypeHasBoundary()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream = new MemoryStream(new byte[] { 7, 8, 9 });

            await petApi.UploadFileWithRequiredFileAsync(
                petId: 1,
                requiredFile: new FileParameter(stream),
                additionalMetadata: new Option<string>("testmetadata"));

            Assert.IsNotNull(_capturingHandler.CapturedContentType,
                "Content-Type header should not be null.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "multipart/form-data",
                "Content-Type should be multipart/form-data.");
            StringAssert.Contains(_capturingHandler.CapturedContentType, "boundary=",
                "Adding text fields alongside binary fields must not strip the boundary.");
        }

        [TestMethod]
        public async Task UploadFileWithRequiredFile_WithTextAndFile_BodyContainsBothFields()
        {
            var petApi = _host.Services.GetRequiredService<IPetApi>();
            using var stream = new MemoryStream(new byte[] { 7, 8, 9 });

            await petApi.UploadFileWithRequiredFileAsync(
                petId: 1,
                requiredFile: new FileParameter(stream),
                additionalMetadata: new Option<string>("testmetadata"));

            Assert.IsNotNull(_capturingHandler.CapturedBody, "Body should not be null.");
            StringAssert.Contains(_capturingHandler.CapturedBody, "name=requiredFile",
                "Body should contain a part named 'requiredFile'.");
            StringAssert.Contains(_capturingHandler.CapturedBody, "testmetadata",
                "Body should contain the additionalMetadata value.");
        }
    }
}
