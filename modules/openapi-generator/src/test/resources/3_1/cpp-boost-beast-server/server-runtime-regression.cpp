// ============================================================================
// server-runtime-regression.cpp - end-to-end loopback driver for the
// cpp-boost-beast-server runtime test. Implements every service method,
// serves on 127.0.0.1:0, then asserts wire behavior with raw sockets.
// ============================================================================
#include "HttpServer.h"
#include "ParamCodecs.h"
#include "Problem.h"
#include "Responder.h"
#include "Router.h"
#include "DefaultApi.h"

#include <boost/asio.hpp>

#include <chrono>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

namespace api = org::openapitools::server::api;
namespace model = org::openapitools::server::model;

static int failures = 0;

static void expect(bool condition, std::string const& what) {
    if (!condition) {
        ++failures;
        std::cerr << "FAIL: " << what << "\n";
    }
}

// ---------------------------------------------------------------------------
// Service implementation with deterministic echo behavior.
// ---------------------------------------------------------------------------
class RegressionApi : public api::DefaultApi {
public:
    void getPetById(GetPetByIdRequest request,
                    std::shared_ptr<api::RequestContext> context,
                    GetPetByIdResponder responder) override {
        model::Pet pet;
        pet.setId(request.petId);
        pet.setName("pet-" + std::to_string(request.petId));
        pet.setStatus(std::string("available"));
        // The context is heap-owned; reading it here proves the handler kept
        // it alive for the service.
        expect(!context->operationId.empty(), "context should carry operationId");
        // Complete twice: the single-completion guard must ignore the
        // second call, so the client still sees exactly one clean 200.
        responder.send200(pet);
        responder.send200(std::move(pet));
    }

    void updatePet(UpdatePetRequest request,
                   std::shared_ptr<api::RequestContext>,
                   UpdatePetResponder responder) override {
        responder.send200(request.body);
    }

    void deletePet(DeletePetRequest,
                   std::shared_ptr<api::RequestContext>,
                   DeletePetResponder responder) override {
        responder.send204();
    }

    void createPet(CreatePetRequest request,
                   std::shared_ptr<api::RequestContext>,
                   CreatePetResponder responder) override {
        responder.send201(request.body);
    }

    void listPets(ListPetsRequest request,
                  std::shared_ptr<api::RequestContext>,
                  ListPetsResponder responder) override {
        std::vector<std::shared_ptr<model::Pet>> pets;
        int count = request.limit > 0 ? static_cast<int>(request.limit) : 2;
        for (int i = 1; i <= count; ++i) {
            auto pet = std::make_shared<model::Pet>();
            pet->setId(i);
            pet->setName("pet-" + std::to_string(i));
            if (!request.status.empty()) {
                pet->setStatus(request.status);
            }
            pets.push_back(std::move(pet));
        }
        responder.send200(std::move(pets));
    }

    void getReport(GetReportRequest request,
                   std::shared_ptr<api::RequestContext>,
                   GetReportResponder responder) override {
        model::Report report;
        // Echo the decoded cookie so the test can prove cookie parsing
        // (including optional whitespace after ';') end to end.
        report.setTitle(request.lang.empty() ? "report" : request.lang);
        responder.send200(std::move(report));
    }

    void getBatch(GetBatchRequest request,
                  std::shared_ptr<api::RequestContext>,
                  GetBatchResponder responder) override {
        std::string joined;
        for (std::size_t i = 0; i < request.ids.size(); ++i) {
            if (i != 0) {
                joined += ",";
            }
            joined += request.ids[i];
        }
        model::Report report;
        report.setTitle(joined);
        responder.send200(std::move(report));
    }

    void getBulkPets(GetBulkPetsRequest,
                     std::shared_ptr<api::RequestContext>,
                     GetBulkPetsResponder responder) override {
        model::Pet pet;
        pet.setId(999);
        pet.setName("bulk");
        responder.send200(std::move(pet));
    }

    void search(SearchRequest request,
                std::shared_ptr<api::RequestContext>,
                SearchResponder responder) override {
        // Deferred completion: the responder outlives the read deadline
        // (readTimeoutSeconds is 1s), proving send_response re-arms the
        // stream timer before writing. Responder is a movable, thread-safe
        // value type whose sink posts onto the connection strand.
        std::size_t filterSize = request.filter.size();
        std::size_t sortSize = request.sort.size();
        std::thread([responder = std::move(responder),
                     filterSize, sortSize]() mutable {
            std::this_thread::sleep_for(std::chrono::seconds(2));
            model::Report report;
            report.setTitle(std::to_string(filterSize) + "-"
                + std::to_string(sortSize));
            responder.send200(std::move(report));
        }).detach();
    }

    void getDefaults(GetDefaultsRequest request,
                     std::shared_ptr<api::RequestContext>,
                     GetDefaultsResponder responder) override {
        // Echoes the values the wire produced: declared defaults for absent
        // optional parameters, decoded values for present ones.
        model::Report report;
        report.setTitle(request.name + "|" + std::to_string(request.limit)
            + "|" + std::to_string(request.ids.size()));
        responder.send200(std::move(report));
    }

    void getPeriod(GetPeriodRequest request,
                   std::shared_ptr<api::RequestContext>,
                   GetPeriodResponder responder) override {
        model::Report report;
        report.setTitle(std::to_string(request.year) + "-"
            + std::to_string(request.month));
        responder.send200(std::move(report));
    }

    void postEcho(PostEchoRequest request,
                  std::shared_ptr<api::RequestContext>,
                  PostEchoResponder responder) override {
        // An absent optional body keeps the default-constructed Pet (id 0,
        // no name); a present one echoes the decoded model.
        responder.send200(request.body);
    }

    void codec(CodecRequest request,
               std::shared_ptr<api::RequestContext>,
               CodecResponder responder) override {
        // Echo the decoded float so the driver can prove the wire grammar
        // gates: hex forms and float overflow are rejected pre-handler,
        // underflow arrives as zero (ordinary IEEE rounding).
        model::Report report;
        report.setTitle(std::to_string(request.w));
        responder.send200(std::move(report));
    }

    void getPick(GetPickRequest,
                 std::shared_ptr<api::RequestContext>,
                 GetPickResponder responder) override {
        // oneOf of two string branches shares one C++ type, so the generated
        // variant holds tagged CompositionBranchValue members. Serializing it
        // exercises the response-side unwrap (bodyLeaf overload).
        responder.send200(model::Pick{
            model::CompositionBranchValue<0, std::string>(std::string("Alpha"))});
    }
};

class RegressionAuthorizer : public api::Authorizer {
public:
    bool authorize(std::string const& operationId,
                   api::AuthCredentials const& credentials) override {
        // Deny the anonymous op outright: it must still succeed because
        // `security: []` bypasses the gate entirely.
        if (operationId == "getPetById") {
            return false;
        }
        return credentials.httpAuthorization == "Bearer ok"
            || credentials.apiKeyValues.count("header:X-API-KEY") != 0;
    }
};

struct RawResponse {
    unsigned status = 0;
    std::string versionLine;
    std::string allow;
    std::string wwwAuthenticate;
    std::string contentType;
    std::string body;
};

static RawResponse roundtrip(
        boost::asio::io_context& ioc,
        unsigned port,
        std::string const& request,
        bool closeConnection = true,
        boost::asio::ip::tcp::socket* persistent = nullptr) {
    RawResponse result;
    boost::asio::ip::tcp::socket owned(ioc);
    boost::asio::ip::tcp::socket& target =
        persistent != nullptr ? *persistent : owned;
    if (persistent == nullptr) {
        target.connect(boost::asio::ip::tcp::endpoint{
            boost::asio::ip::make_address("127.0.0.1"),
            static_cast<unsigned short>(port)});
    }
    boost::asio::write(target, boost::asio::buffer(request));

    std::string accumulated;
    char buffer[4096];
    boost::system::error_code error;
    for (;;) {
        std::size_t received = target.read_some(
            boost::asio::buffer(buffer), error);
        if (error) {
            break;
        }
        accumulated.append(buffer, received);
        std::size_t headerEnd = accumulated.find("\r\n\r\n");
        if (headerEnd == std::string::npos) {
            continue;
        }
        std::size_t contentLength = 0;
        std::istringstream headerStream(accumulated.substr(0, headerEnd));
        std::string line;
        bool firstLine = true;
        while (std::getline(headerStream, line)) {
            // CRLF lines keep a trailing '\r'; strip it so exact header
            // comparisons behave.
            if (!line.empty() && line.back() == '\r') {
                line.pop_back();
            }
            if (firstLine) {
                result.versionLine = line;
                firstLine = false;
                continue;
            }
            std::string lower;
            lower.reserve(line.size());
            for (char c : line) {
                lower.push_back(static_cast<char>(std::tolower(
                    static_cast<unsigned char>(c))));
            }
            std::string const contentLengthPrefix = "content-length: ";
            std::string const allowPrefix = "allow: ";
            std::string const wwwAuthenticatePrefix = "www-authenticate: ";
            std::string const contentTypePrefix = "content-type: ";
            if (lower.size() > contentLengthPrefix.size()
                    && lower.compare(0, contentLengthPrefix.size(),
                                     contentLengthPrefix) == 0) {
                contentLength = static_cast<std::size_t>(
                    std::stoul(lower.substr(contentLengthPrefix.size())));
            }
            if (lower.size() > allowPrefix.size()
                    && lower.compare(0, allowPrefix.size(), allowPrefix) == 0) {
                result.allow = line.substr(allowPrefix.size());
            }
            if (lower.size() > wwwAuthenticatePrefix.size()
                    && lower.compare(0, wwwAuthenticatePrefix.size(),
                                     wwwAuthenticatePrefix) == 0) {
                result.wwwAuthenticate =
                    line.substr(wwwAuthenticatePrefix.size());
            }
            if (lower.size() > contentTypePrefix.size()
                    && lower.compare(0, contentTypePrefix.size(),
                                     contentTypePrefix) == 0) {
                result.contentType = line.substr(contentTypePrefix.size());
            }
        }
        if (accumulated.size() - headerEnd - 4 >= contentLength) {
            std::string headerBlock = accumulated.substr(0, headerEnd);
            std::size_t statusStart = headerBlock.find(' ');
            result.status = static_cast<unsigned>(std::stoul(
                headerBlock.substr(statusStart + 1, 3)));
            result.body = accumulated.substr(headerEnd + 4, contentLength);
            break;
        }
    }
    if (closeConnection) {
        boost::system::error_code ignored;
        target.shutdown(boost::asio::ip::tcp::socket::shutdown_both, ignored);
    }
    return result;
}

/// Builds a request string, computing Content-Length from the actual body.
static std::string request(
        std::string const& methodAndPath,
        std::string const& headers,
        std::string const& body) {
    std::string fixedHeaders = headers;
    if (!body.empty() && fixedHeaders.find("Content-Length") == std::string::npos) {
        fixedHeaders += "Content-Length: " + std::to_string(body.size()) + "\r\n";
    }
    return methodAndPath + " HTTP/1.1\r\nHost: t\r\n" + fixedHeaders
        + "\r\n" + body;
}

/// Same as request() but for a caller-chosen HTTP version (version tests).
static std::string requestVersioned(
        std::string const& version,
        std::string const& methodAndPath,
        std::string const& headers,
        std::string const& body) {
    std::string fixedHeaders = headers;
    if (!body.empty() && fixedHeaders.find("Content-Length") == std::string::npos) {
        fixedHeaders += "Content-Length: " + std::to_string(body.size()) + "\r\n";
    }
    return methodAndPath + " " + version + "\r\nHost: t\r\n" + fixedHeaders
        + "\r\n" + body;
}

int main() {
    boost::asio::io_context ioc;
    auto router = std::make_shared<api::Router>();
    api::ServerOptions options;
    // 1-second deadline makes the deferred search completion (2s worker
    // thread) meaningful: the write must survive an already-expired timer.
    options.readTimeoutSeconds = 1;
    options.bodyLimitBytes = 1024;
    options.authorizer = std::make_shared<RegressionAuthorizer>();
    auto server = api::HttpServer::create(ioc, router, options);
    api::DefaultApi::attach(*server, std::make_shared<RegressionApi>());
    server->listen(boost::asio::ip::tcp::endpoint{
        boost::asio::ip::make_address("127.0.0.1"),
        static_cast<unsigned short>(0)});
    unsigned port = server->localEndpoint().port();

    std::thread serverThread([&ioc] { ioc.run(); });

    // 200 + JSON body on a valid GET.
    RawResponse ok = roundtrip(ioc, port,
        "GET /pets/42 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(ok.status == 200, "valid pet GET should be 200");
    expect(ok.body.find("\"id\":42") != std::string::npos,
        "pet body should carry id 42");

    // 400 problem on int64 path failure and minimum violation.
    RawResponse badId = roundtrip(ioc, port,
        "GET /pets/abc HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(badId.status == 400, "non-numeric petId should be 400");
    expect(badId.contentType.find("application/problem+json") != std::string::npos,
        "problem content type on bad petId");
    expect(badId.body.find("\"errors\"") != std::string::npos,
        "problem errors array on bad petId");

    RawResponse belowMin = roundtrip(ioc, port,
        "GET /pets/0 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(belowMin.status == 400, "petId below minimum should be 400");

    // 400 on enum failure and limit bound failure.
    RawResponse badEnum = roundtrip(ioc, port,
        "GET /pets?status=unknown HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(badEnum.status == 400, "invalid status enum should be 400");

    RawResponse badLimit = roundtrip(ioc, port,
        "GET /pets?limit=0 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(badLimit.status == 400, "limit below minimum should be 400");

    // 400 on a numeric enum member outside the allowed set; 200 inside it.
    RawResponse badTier = roundtrip(ioc, port,
        "GET /pets?tier=15 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(badTier.status == 400, "tier outside the integer enum should be 400");
    RawResponse goodTier = roundtrip(ioc, port,
        "GET /pets?tier=20 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(goodTier.status == 200, "tier inside the integer enum should be 200");

    // 404 on unknown path.
    RawResponse missing = roundtrip(ioc, port,
        "GET /nope HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(missing.status == 404, "unknown path should be 404");

    // 405 with Allow on wrong method.
    RawResponse wrongMethod = roundtrip(ioc, port,
        "PATCH /pets/42 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(wrongMethod.status == 405, "wrong method should be 405");
    expect(wrongMethod.allow.find("GET") != std::string::npos
            && wrongMethod.allow.find("PUT") != std::string::npos
            && wrongMethod.allow.find("DELETE") != std::string::npos,
        "Allow should list GET, PUT, DELETE");

    // 400 on malformed JSON body.
    RawResponse badJson = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", "{oops}}"));
    expect(badJson.status == 400, "malformed JSON body should be 400");

    // 400 with errors[] on schema-invalid body (missing required name).
    RawResponse missingName = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", "{\"id\":7}"));
    expect(missingName.status == 400, "missing required name should be 400");
    expect(missingName.body.find("\"errors\"") != std::string::npos,
        "missing-name problem should carry errors");

    // 201 on valid body.
    RawResponse created = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", "{\"id\":9,\"name\":\"rex\"}"));
    expect(created.status == 201, "valid create should be 201");
    expect(created.body.find("rex") != std::string::npos,
        "created body should echo name");
    // 400 on a body whose enum member is not declared (schema validation
    // runs before the handler sees the value).
    RawResponse badEnumBody = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n",
        "{\"id\":9,\"name\":\"rex\",\"status\":\"invalid\"}"));
    expect(badEnumBody.status == 400,
        "undeclared enum member in body should be 400");
    // 400 on an explicit null for a required non-nullable field (no silent
    // default substitution). Only the validation-enabled build enforces
    // this at the request boundary; the compileWithValidation=false leg
    // keeps the documented tolerate-null compatibility default.
#ifdef CPPBB_EXPECT_SCHEMA_VALIDATION
    RawResponse nullRequired = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", "{\"id\":null,\"name\":\"rex\"}"));
    expect(nullRequired.status == 400,
        "null for a required non-nullable body field should be 400");
#endif

    // 415 on text/plain.
    RawResponse wrongType = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: text/plain\r\n"
        "Connection: close\r\n", "hi"));
    expect(wrongType.status == 415, "text/plain body should be 415");
    // 404 must not echo the query: a credential sent as ?api_key= would
    // otherwise leak through detail/instance.
    RawResponse missingKey = roundtrip(ioc, port,
        "GET /nope?api_key=SUPERSECRET HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(missingKey.status == 404, "unknown path with query should be 404");
    expect(missingKey.body.find("SUPERSECRET") == std::string::npos,
        "404 problem must not echo the query string");

    // 401 without credentials on POST.
    RawResponse noAuth = roundtrip(ioc, port, request("POST /pets",
        "Content-Type: application/json\r\nConnection: close\r\n",
        "{\"id\":9,\"name\":\"rex\"}"));
    expect(noAuth.status == 401, "POST without bearer should be 401");
    // RFC 9110 11.6.1: http-scheme 401 carries a challenge. The scheme token
    // mirrors the declared casing (spec uses lowercase "bearer"; RFC 7235
    // treats auth scheme tokens case-insensitively).
    expect(noAuth.wwwAuthenticate == "bearer realm=\"api\"",
        "bearer 401 should carry WWW-Authenticate challenge");

    // 401 on explicitly denied credentials.
    RawResponse denied = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer deny\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", "{\"id\":9,\"name\":\"rex\"}"));
    expect(denied.status == 401, "denied bearer should be 401");
    expect(denied.wwwAuthenticate == "bearer realm=\"api\"",
        "denied bearer 401 should carry WWW-Authenticate challenge");

    // 204 with valid API key (inherited global security) on DELETE.
    RawResponse deleted = roundtrip(ioc, port,
        "DELETE /pets/42 HTTP/1.1\r\nHost: t\r\nX-API-KEY: k1,k2\r\n"
        "Connection: close\r\n\r\n");
    expect(deleted.status == 204, "DELETE with api_key should be 204");

    // 401 without the API key on DELETE.
    RawResponse noKey = roundtrip(ioc, port,
        "DELETE /pets/42 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(noKey.status == 401, "DELETE without api_key should be 401");
    // API keys have no standardized challenge form: none must be emitted.
    expect(noKey.wwwAuthenticate.empty(),
        "apiKey 401 should not carry WWW-Authenticate");

    // 200 round-trip through the typed request-body decoder + echo.
    RawResponse updated = roundtrip(ioc, port, request("PUT /pets/42",
        "X-API-KEY: k1\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", "{\"id\":42,\"name\":\"put-dog\"}"));
    expect(updated.status == 200, "PUT with api_key should be 200");
    expect(updated.body.find("put-dog") != std::string::npos,
        "PUT response should echo the decoded body");
    // Constraint inheritance from the PATH ITEM: `version` is declared on
    // /pets/{petId}.parameters with minimum:5, so the raw-schema lookup
    // must fall back beyond the operation list; violating values answer 400.
    RawResponse versionLow = roundtrip(ioc, port,
        "GET /pets/3?version=2 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(versionLow.status == 400,
        "path-item parameter constraint must be enforced (below minimum)");
    expect(versionLow.body.find("below the minimum") != std::string::npos,
        "path-item constraint failure should report the minimum");
    RawResponse versionOk = roundtrip(ioc, port,
        "GET /pets/3?version=9 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(versionOk.status == 200,
        "path-item parameter satisfying its constraint should be 200");

    // 413 on a body over the configured limit.
    std::string big(2048, 'x');
    RawResponse tooBig = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", big));
    expect(tooBig.status == 413, "oversized body should be 413");

    // 413 on HTTP/1.0 must still mirror the request version (parser head
    // is available when body_limit fires).
    RawResponse tooBig10 = roundtrip(ioc, port, requestVersioned(
        "HTTP/1.0", "POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n",
        big));
    expect(tooBig10.status == 413, "oversized HTTP/1.0 body should be 413");
    expect(tooBig10.versionLine.rfind("HTTP/1.0", 0) == 0,
        "413 response should mirror HTTP/1.0 (no stale version on error path)");

    // HTTP/1.0 response version mirroring on the success path too.
    RawResponse plain10 = roundtrip(ioc, port, requestVersioned(
        "HTTP/1.0", "GET /pets/42", "", ""));
    expect(plain10.status == 200, "plain HTTP/1.0 GET should be 200");
    expect(plain10.versionLine.rfind("HTTP/1.0", 0) == 0,
        "HTTP/1.0 request should get an HTTP/1.0 response (RFC 9110 6.7)");

    // Keep-alive: two requests on one connection.
    {
        boost::asio::ip::tcp::socket persistent(ioc);
        persistent.connect(boost::asio::ip::tcp::endpoint{
            boost::asio::ip::make_address("127.0.0.1"),
            static_cast<unsigned short>(port)});
        RawResponse first = roundtrip(ioc, port,
            "GET /pets/7 HTTP/1.1\r\nHost: t\r\n\r\n",
            false, &persistent);
        RawResponse second = roundtrip(ioc, port,
            "GET /pets/8 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n",
            true, &persistent);
        expect(first.status == 200 && second.status == 200,
            "keep-alive should serve two requests on one connection");
        expect(second.body.find("\"id\":8") != std::string::npos,
            "second keep-alive response should be for pet 8");
    }

    // Expect: 100-continue must be answered BEFORE the body is released.
    // A client that holds its body back would deadlock against the read
    // timeout on a server that reads head+body in one step, so the probe
    // writes only the head and asserts the interim response arrives first.
    {
        boost::asio::ip::tcp::socket expectSocket(ioc);
        expectSocket.connect(boost::asio::ip::tcp::endpoint{
            boost::asio::ip::make_address("127.0.0.1"),
            static_cast<unsigned short>(port)});
        const std::string body = "{\"id\":5,\"name\":\"cont\"}";
        const std::string head =
            "POST /echo HTTP/1.1\r\nHost: t\r\nExpect: 100-continue\r\n"
            "Content-Type: application/json\r\nConnection: close\r\n"
            "Content-Length: " + std::to_string(body.size()) + "\r\n\r\n";
        boost::asio::write(expectSocket, boost::asio::buffer(head));
        expectSocket.non_blocking(true);
        std::string transcript;
        char probe[64];
        for (int attempt = 0; attempt < 300; ++attempt) {
            boost::system::error_code probeEc;
            std::size_t got = 0;
            try {
                got = expectSocket.read_some(
                    boost::asio::buffer(probe), probeEc);
            } catch (boost::system::system_error const&) {
                got = 0;
                probeEc = boost::asio::error::would_block;
            }
            if (got > 0) {
                transcript.append(probe, got);
                break;   // first bytes after the head = the interim response
            }
            if (probeEc != boost::asio::error::would_block) {
                break;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        expectSocket.non_blocking(false);
        expect(transcript.rfind("HTTP/1.1 100", 0) == 0,
            "Expect: 100-continue should receive an interim response first");
        boost::asio::write(expectSocket, boost::asio::buffer(body));
        char buffer[4096];
        boost::system::error_code readEc;
        for (;;) {
            std::size_t got = expectSocket.read_some(
                boost::asio::buffer(buffer), readEc);
            if (readEc) {
                break;
            }
            transcript.append(buffer, got);
        }
        expect(transcript.find("HTTP/1.1 200") != std::string::npos,
            "100-continue request should complete with the final response");
        expect(transcript.find("cont") != std::string::npos,
            "final response should echo the released body");
    }

    // Label-style pattern path parameter: valid then invalid.
    RawResponse labelOk = roundtrip(ioc, port,
        "GET /reports/.AB-12 HTTP/1.1\r\nHost: t\r\n"
        "Authorization: Bearer ok\r\nCookie: session=x; lang=en\r\n"
        "Connection: close\r\n\r\n");
    expect(labelOk.status == 200, "label path with valid pattern should be 200");
    // Cookie value decoded, OWS after ';' skipped, echoed via title.
    expect(labelOk.body.find("\"title\":\"en\"") != std::string::npos,
        "cookie lang should decode to en and echo in title");

    RawResponse labelBad = roundtrip(ioc, port,
        "GET /reports/.ab12 HTTP/1.1\r\nHost: t\r\n"
        "Authorization: Bearer ok\r\nConnection: close\r\n\r\n");
    expect(labelBad.status == 400, "label path violating pattern should be 400");

    // Pipe-delimited array query parameter.
    RawResponse piped = roundtrip(ioc, port,
        "GET /pets?tags=red%7Cblue HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(piped.status == 200, "pipe-delimited tags should parse");

    // Matrix-exploded array path parameter: ";ids=3;ids=4;ids=5" -> 3,4,5.
    RawResponse batch = roundtrip(ioc, port,
        "GET /batch/;ids=3;ids=4;ids=5 HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(batch.status == 200, "matrix-exploded ids should parse");
    expect(batch.body.find("\"title\":\"3,4,5\"") != std::string::npos,
        "matrix explode should strip repeated name= per element");

    // Literal route registered AFTER /pets/{petId} must still win.
    RawResponse bulk = roundtrip(ioc, port,
        "GET /pets/bulk HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(bulk.status == 200, "/pets/bulk should hit the literal route");
    expect(bulk.body.find("\"id\":999") != std::string::npos,
        "literal-over-param ranking should select getBulkPets");

    // Deep-object required map + space-delimited array; completion is
    // deferred 2s, surviving the expired 1s read timer via the re-arm.
    RawResponse searchOk = roundtrip(ioc, port,
        "GET /search?filter%5Bcolor%5D=blue HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(searchOk.status == 200,
        "deepObject filter should parse and deferred response should arrive");
    expect(searchOk.body.find("\"title\":\"1-0\"") != std::string::npos,
        "deferred completion should echo 1 filter, 0 sort fields");

    RawResponse searchSorted = roundtrip(ioc, port,
        "GET /search?filter%5Bcolor%5D=blue&sort=a%20b HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(searchSorted.status == 200, "space-delimited sort should parse");
    expect(searchSorted.body.find("\"title\":\"1-2\"") != std::string::npos,
        "spaceDelimited sort should split into two elements");

    RawResponse searchMissing = roundtrip(ioc, port,
        "GET /search HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(searchMissing.status == 400,
        "required deepObject filter missing should be 400");

    // ---- declared defaults + presence ----
    // Absent optional parameters keep their OpenAPI defaults, not zero values.
    RawResponse defaultsAbsent = roundtrip(ioc, port,
        "GET /defaults?ids=aa,bb HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(defaultsAbsent.status == 200, "defaults with valid ids should be 200");
    expect(defaultsAbsent.body.find("\"title\":\"five|7|2\"") != std::string::npos,
        "absent name/limit should carry declared defaults (five, 7)");

    RawResponse defaultsPresent = roundtrip(ioc, port,
        "GET /defaults?name=hi&limit=3&ids=aa,bb HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(defaultsPresent.status == 200, "explicit values should be 200");
    expect(defaultsPresent.body.find("\"title\":\"hi|3|2\"") != std::string::npos,
        "present name/limit should override declared defaults");

    // Exclusive bounds: score must be >1.5 and <3 (JSON Schema semantics).
    RawResponse scoreLow = roundtrip(ioc, port,
        "GET /defaults?ids=aa,bb&score=1.5 HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(scoreLow.status == 400, "score == exclusiveMinimum should be 400");
    RawResponse scoreOk = roundtrip(ioc, port,
        "GET /defaults?ids=aa,bb&score=1.75 HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(scoreOk.status == 200, "score above exclusiveMinimum should be 200");
    RawResponse scoreHigh = roundtrip(ioc, port,
        "GET /defaults?ids=aa,bb&score=3 HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(scoreHigh.status == 400, "score == exclusiveMaximum should be 400");

    // Collection + item constraints: minItems=2, uniqueItems, item minLength=2.
    RawResponse fewItems = roundtrip(ioc, port,
        "GET /defaults?ids=aa HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(fewItems.status == 400, "ids below minItems should be 400");
    RawResponse dupItems = roundtrip(ioc, port,
        "GET /defaults?ids=aa,aa HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(dupItems.status == 400, "duplicate ids should violate uniqueItems");
    RawResponse shortItem = roundtrip(ioc, port,
        "GET /defaults?ids=aa,b HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(shortItem.status == 400, "item below item minLength should be 400");

    // ---- embedded path expressions ----
    RawResponse period = roundtrip(ioc, port,
        "GET /periods/2026-8/summary HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(period.status == 200, "embedded path expressions should route");
    expect(period.body.find("\"title\":\"2026-8\"") != std::string::npos,
        "embedded expressions should capture year and month separately");
    RawResponse periodBad = roundtrip(ioc, port,
        "GET /periods/20x6-8/summary HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(periodBad.status == 400, "non-numeric embedded capture should be 400");

    // ---- strict style codecs ----
    // Label without the leading dot is malformed (the dot is part of the
    // label serialization), even though the bare value matches the pattern.
    RawResponse labelNoDot = roundtrip(ioc, port,
        "GET /reports/AB-12 HTTP/1.1\r\nHost: t\r\n"
        "Authorization: Bearer ok\r\nConnection: close\r\n\r\n");
    expect(labelNoDot.status == 400, "label segment without dot should be 400");
    // Matrix segment with the wrong parameter name is malformed.
    RawResponse batchWrongName = roundtrip(ioc, port,
        "GET /batch/;x=3;x=4 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(batchWrongName.status == 400, "matrix segment with wrong name should be 400");
    // Matrix element missing the repeated name= prefix is malformed.
    RawResponse batchLoose = roundtrip(ioc, port,
        "GET /batch/;ids=3;4 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(batchLoose.status == 400, "matrix element without name= should be 400");

    // ---- declared JSON response media type ----
    expect(labelOk.contentType.find("application/vnd.report+json") != std::string::npos,
        "getReport should serve its declared +json media type");

    // ---- optional request body ----
    RawResponse echoEmpty = roundtrip(ioc, port,
        "POST /echo HTTP/1.1\r\nHost: t\r\nContent-Length: 0\r\n"
        "Connection: close\r\n\r\n");
    expect(echoEmpty.status == 200, "optional body may be absent");
    expect(echoEmpty.body.find("\"id\":0") != std::string::npos,
        "absent optional body should decode to the default model");
    RawResponse echoBad = roundtrip(ioc, port, request("POST /echo",
        "Content-Type: application/json\r\nConnection: close\r\n", "{oops}}"));
    expect(echoBad.status == 400, "present-but-malformed optional body should be 400");
    RawResponse echoGood = roundtrip(ioc, port, request("POST /echo",
        "Content-Type: application/json\r\nConnection: close\r\n",
        "{\"id\":5,\"name\":\"echoed\"}"));
    expect(echoGood.status == 200 && echoGood.body.find("echoed") != std::string::npos,
        "present optional body should decode and echo");
    // ---- float wire grammar (parseScalar gates) ----
    // Plain decimal parses and reaches the handler.
    RawResponse floatOk = roundtrip(ioc, port,
        "GET /codec?w=1.5 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(floatOk.status == 200, "decimal float query should be 200");
    expect(floatOk.body.find("\"title\":\"1.500000\"") != std::string::npos,
        "decimal float should decode to 1.5");
    // C99 hex floats are not JSON numbers even though strtod consumes them.
    RawResponse floatHex = roundtrip(ioc, port,
        "GET /codec?w=0x10 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(floatHex.status == 400, "hex float text should be 400");
    // Values outside float range must not reach the service as infinity.
    RawResponse floatBig = roundtrip(ioc, port,
        "GET /codec?w=1e40 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(floatBig.status == 400, "float overflow should be 400");
    // Underflow (ERANGE to zero) is ordinary IEEE rounding: accepted.
    RawResponse floatTiny = roundtrip(ioc, port,
        "GET /codec?w=1e-400 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(floatTiny.status == 200, "float underflow should be accepted");
    expect(floatTiny.body.find("\"title\":\"0.000000\"") != std::string::npos,
        "underflowed float should arrive as zero");
    // strtod's inf/nan spellings are not JSON numbers.
    RawResponse floatInf = roundtrip(ioc, port,
        "GET /codec?w=inf HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(floatInf.status == 400, "inf text should be 400");

    // ---- fail-closed regex patterns ----
    // '(?i)' is outside std::regex's ECMAScript subset: construction throws,
    // so the gate answers 400 for every present value instead of retry-
    // throwing 500 on each request.
    RawResponse patternHit = roundtrip(ioc, port,
        "GET /codec?code=abc HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(patternHit.status == 400, "uncompilable scalar pattern should fail closed 400");
    expect(patternHit.body.find("supported regex grammar") != std::string::npos,
        "fail-closed problem should explain the pattern grammar");
    // An absent optional parameter skips the (broken) pattern entirely.
    RawResponse patternAbsent = roundtrip(ioc, port,
        "GET /codec HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(patternAbsent.status == 200, "absent pattern parameter should be 200");
    // Item patterns fail closed the same way.
    RawResponse itemPatternHit = roundtrip(ioc, port,
        "GET /codec?codes=aa,bb HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(itemPatternHit.status == 400, "uncompilable item pattern should fail closed 400");
    // A Unicode property escape (\p{L}) is refused explicitly, not matched
    // approximately: some std::regex libraries compile \p as an identity
    // escape (literal "p"), which would silently mis-accept. The scanner
    // rejects it before construction, so any present value answers 400.
    RawResponse propertyEscape = roundtrip(ioc, port,
        "GET /codec?uni=abc HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(propertyEscape.status == 400,
        "unicode property-escape pattern should fail closed 400");
    expect(propertyEscape.body.find("supported regex grammar") != std::string::npos,
        "property-escape problem should explain the grammar");
    // The same policy applies to a pattern nested in a BODY property: Pet.note
    // declares '^\p{L}+$', which the model validator refuses as an unsupported
    // pattern expression, so any payload carrying the property answers 400
    // (the object-property fail-closed path). Only when the schema registry is
    // generated — the decode-shape-only config has no schema gate and accepts
    // the payload.
#ifdef CPPBB_EXPECT_SCHEMA_VALIDATION
    RawResponse propertyNote = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n",
        "{\"id\":9,\"name\":\"rex\",\"note\":\"abc\"}"));
    expect(propertyNote.status == 400,
        "body property with property-escape pattern should fail closed 400");
    expect(propertyNote.body.find("unsupported pattern expression") != std::string::npos,
        "body fail-closed problem should name the pattern grammar");
#endif

    // An unsupported Expectation token (anything but 100-continue) is refused
    // with 417 (RFC 9110 10.1.1) instead of being silently ignored: a client
    // that requested an assurance the server cannot give must never have its
    // body read as if the expectation held.
    RawResponse expectUnknown = roundtrip(ioc, port, request("POST /echo",
        "Content-Type: application/json\r\nExpect: confirm-10x\r\n"
        "Connection: close\r\n", "{\"id\":1,\"name\":\"x\"}"));
    expect(expectUnknown.status == 417,
        "unsupported Expectation token should answer 417");
    // A list containing 100-continue plus an unsupported token is also 417:
    // every expectation must be satisfiable before the interim response.
    RawResponse expectMixed = roundtrip(ioc, port, request("POST /echo",
        "Content-Type: application/json\r\nExpect: 100-continue, confirm-10x\r\n"
        "Connection: close\r\n", "{\"id\":1,\"name\":\"x\"}"));
    expect(expectMixed.status == 417,
        "mixed Expectation list with an unsupported token should answer 417");

    // ---- tagged variant (oneOf) response ----
    // Pick is a oneOf of two string branches sharing one C++ type, so the
    // generated model is std::variant<CompositionBranchValue<0,std::string>,
    // CompositionBranchValue<1,std::string>>. Serving it exercises the
    // response-side bodyLeaf unwrap.
    RawResponse picked = roundtrip(ioc, port,
        "GET /pick HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(picked.status == 200, "tagged variant response should be 200");
    expect(picked.body.find("\"Alpha\"") != std::string::npos,
        "variant response should serialize the selected branch as its value");

    ioc.stop();
    serverThread.join();

    if (failures != 0) {
        std::cerr << failures << " server runtime assertion(s) failed\n";
        return 1;
    }
    std::cout << "cpp-boost-beast-server runtime regressions passed\n";
    return 0;
}
