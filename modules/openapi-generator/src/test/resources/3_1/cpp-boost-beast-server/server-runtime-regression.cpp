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
    void getPetById(api::GetPetByIdRequest request,
                    api::RequestContext&,
                    api::GetPetByIdResponder responder) override {
        model::Pet pet;
        pet.setId(request.petId);
        pet.setName("pet-" + std::to_string(request.petId));
        pet.setStatus(std::string("available"));
        // Complete twice: the single-completion guard must ignore the
        // second call, so the client still sees exactly one clean 200.
        responder.send200(pet);
        responder.send200(std::move(pet));
    }

    void updatePet(api::UpdatePetRequest request,
                   api::RequestContext&,
                   api::UpdatePetResponder responder) override {
        responder.send200(request.body);
    }

    void deletePet(api::DeletePetRequest,
                   api::RequestContext&,
                   api::DeletePetResponder responder) override {
        responder.send204();
    }

    void createPet(api::CreatePetRequest request,
                   api::RequestContext&,
                   api::CreatePetResponder responder) override {
        responder.send201(request.body);
    }

    void listPets(api::ListPetsRequest request,
                  api::RequestContext&,
                  api::ListPetsResponder responder) override {
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

    void getReport(api::GetReportRequest request,
                   api::RequestContext&,
                   api::GetReportResponder responder) override {
        model::Report report;
        // Echo the decoded cookie so the test can prove cookie parsing
        // (including optional whitespace after ';') end to end.
        report.setTitle(request.lang.empty() ? "report" : request.lang);
        responder.send200(std::move(report));
    }

    void getBatch(api::GetBatchRequest request,
                  api::RequestContext&,
                  api::GetBatchResponder responder) override {
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

    void getBulkPets(api::GetBulkPetsRequest,
                     api::RequestContext&,
                     api::GetBulkPetsResponder responder) override {
        model::Pet pet;
        pet.setId(999);
        pet.setName("bulk");
        responder.send200(std::move(pet));
    }

    void search(api::SearchRequest request,
                api::RequestContext&,
                api::SearchResponder responder) override {
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
    auto server = std::make_shared<api::HttpServer>(ioc, router, options);
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

    // 415 on text/plain.
    RawResponse wrongType = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: text/plain\r\n"
        "Connection: close\r\n", "hi"));
    expect(wrongType.status == 415, "text/plain body should be 415");

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

    ioc.stop();
    serverThread.join();

    if (failures != 0) {
        std::cerr << failures << " server runtime assertion(s) failed\n";
        return 1;
    }
    std::cout << "cpp-boost-beast-server runtime regressions passed\n";
    return 0;
}
