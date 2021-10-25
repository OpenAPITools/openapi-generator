//struct FeedbackData: Codable {
//}
//
//struct Feedback: Codable {
//    let user: String
//    let currentDate: Date
//    let param: String
//    let data: FeedbackData
//}
//
//struct FeedbackOutput: Codable {
//}
//
//struct SupportService {
//    let transport: Transport
//
//    init(_ transport: Transport) {
//        self.transport = transport
//    }
//
//    func sendFeedback(_ feedback: Feedback) -> AnyPublisher<Result<FeedbackOutput, Error>, Never> {
//
//        let url: URL = transport.baseURL.appending("/feedback")
//            + feedback.user.encode.urlEncode +
//            "&" + feedback.currentDate.encode.urlEncode
//
//        var request = URLRequest(url: url)
//
//        request.httpMethod = "POST"
//        request.setValue(feedback.param, forHTTPHeaderField: "param")
//        request.httpBody = try? JSONEncoder().encode(feedback.data)
//
//        return transport.send(request, auth: .httpBearer)
//            .map {
//                map($0)
//            }
//            .eraseToAnyPublisher()
//    }
//
//    private func map(_ response: TransportResponse) -> Result<FeedbackOutput, Error> {
//        // 200, 401, 403
//        switch response.response.statusCode {
//            case 200:
//                if let data = response.data, let value = try? JSONDecoder().decode(FeedbackOutput.self, from: data) {
//                    return .success(value)
//                }
//            default:
//                return .failure(response.error ?? .unknown)
//        }
//    }
//}
//
//class SwooAuthService {
//}

/// Runtime

import Foundation
import Combine

public enum SecurityScheme {
    case none
    // Other schemes not supported yet https://swagger.io/docs/specification/authentication/
}

public protocol HTTPAuth {
    func bearer() -> Future<String?, Never>
}

public enum TransportError: Error {
    case noInternetConnection
    case unknown
}

public struct TransportResponse {
    public let data: Data?
    public let response: HTTPURLResponse?
    public let error: TransportError?
}

public protocol Transport {
    var baseURL: URL { get }
    func send(request: URLRequest, securityScheme: SecurityScheme) -> Future<TransportResponse, Never>
}

public class URLSessionTransport: Transport {
    let session: URLSession
    let auth: HTTPAuth
    public var baseURL: URL
    public var commonHeaders: [String: String] = [:]

    public init(session: URLSession = .shared, baseURL: URL, auth: HTTPAuth) {
        self.session = session
        self.auth = auth
        self.baseURL = baseURL
    }

    public func send(request: URLRequest, securityScheme: SecurityScheme) -> Future<TransportResponse, Never> {
        Future { promise in
            // TODO: add headers to requests
            promise(.success(TransportResponse(data: nil, response: nil, error: nil)))
        }
    }
}

/// Client

//class SwooHTTPAuth: HTTPAuth {
//    private(set) var bearer: String?
//
//    func bearer() -> Future<String?, Never> {
//        Future { promise in promise(.success("")) }
//    }
//
////    func bearer() -> Future<String?, Never> {
////        guard let bearer = _bearer else {
////            return renew()
////        }
////        return Future<String?, Never> { promice in
////            ..
////        }
////    }
//
//    func renew() -> Future<String?, Never> {
//
//    }
//}
//
//enum Config {
//    static let authBaseURL = "https://swoo.auth.com"
//    static let feedbackBaseURL = "https://swoo.feedback.com"
//}
//
//let authTransport = DefaultTransport(Config.authBaseURL)
//let authService = SwooAuthService(authTransport)
//let swooHTTPAuth = SwooHTTPAuth(authService: authService)
//
//let transport = URLSessionTransport(baseURL: Config.feedbackBaseURL, auth: swooHTTPAuth)
//let service = SupportService(transport)
//
//service.sendFeedback(Feedback(user: "User", currentDate: .now(), param: "param", data: .init()))
//    .sink { result in
//        ..
//    }
//    .store(...)

// Open questions
// - cancellation - is it really needed?
// - httpauth - should the bearer be future or just for renew procedure?
