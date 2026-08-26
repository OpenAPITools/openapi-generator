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

    void getReport(api::GetReportRequest,
                   api::RequestContext&,
                   api::GetReportResponder responder) override {
        model::Report report;
        report.setTitle("report");
        responder.send200(std::move(report));
    }
};

class RegressionAuthorizer : public api::Authorizer {
public:
    bool authorize(std::string const&, api::AuthCredentials const& credentials) override {
        return credentials.httpAuthorization != "Bearer deny";
    }
};

// ---------------------------------------------------------------------------
// Raw-socket HTTP client helpers.
// ---------------------------------------------------------------------------
struct RawResponse {
    unsigned status = 0;
    std::string allow;
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
        while (std::getline(headerStream, line)) {
            std::string lower;
            lower.reserve(line.size());
            for (char c : line) {
                lower.push_back(static_cast<char>(std::tolower(
                    static_cast<unsigned char>(c))));
            }
            std::string const contentLengthPrefix = "content-length: ";
            std::string const allowPrefix = "allow: ";
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

int main() {
    boost::asio::io_context ioc;
    auto router = std::make_shared<api::Router>();
    api::ServerOptions options;
    options.readTimeoutSeconds = 30;
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

    // 401 on explicitly denied credentials.
    RawResponse denied = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer deny\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", "{\"id\":9,\"name\":\"rex\"}"));
    expect(denied.status == 401, "denied bearer should be 401");

    // 204 with valid API key (inherited global security) on DELETE.
    RawResponse deleted = roundtrip(ioc, port,
        "DELETE /pets/42 HTTP/1.1\r\nHost: t\r\nX-API-KEY: k1,k2\r\n"
        "Connection: close\r\n\r\n");
    expect(deleted.status == 204, "DELETE with api_key should be 204");

    // 401 without the API key on DELETE.
    RawResponse noKey = roundtrip(ioc, port,
        "DELETE /pets/42 HTTP/1.1\r\nHost: t\r\nConnection: close\r\n\r\n");
    expect(noKey.status == 401, "DELETE without api_key should be 401");
    // 413 on a body over the configured limit.
    std::string big(2048, 'x');
    RawResponse tooBig = roundtrip(ioc, port, request("POST /pets",
        "Authorization: Bearer ok\r\nContent-Type: application/json\r\n"
        "Connection: close\r\n", big));
    expect(tooBig.status == 413, "oversized body should be 413");

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
        "Authorization: Bearer ok\r\nCookie: lang=en\r\n"
        "Connection: close\r\n\r\n");
    expect(labelOk.status == 200, "label path with valid pattern should be 200");

    RawResponse labelBad = roundtrip(ioc, port,
        "GET /reports/.ab12 HTTP/1.1\r\nHost: t\r\n"
        "Authorization: Bearer ok\r\nConnection: close\r\n\r\n");
    expect(labelBad.status == 400, "label path violating pattern should be 400");

    // Pipe-delimited array query parameter.
    RawResponse piped = roundtrip(ioc, port,
        "GET /pets?tags=red%7Cblue HTTP/1.1\r\nHost: t\r\n"
        "Connection: close\r\n\r\n");
    expect(piped.status == 200, "pipe-delimited tags should parse");

    ioc.stop();
    serverThread.join();

    if (failures != 0) {
        std::cerr << failures << " server runtime assertion(s) failed\n";
        return 1;
    }
    std::cout << "cpp-boost-beast-server runtime regressions passed\n";
    return 0;
}
