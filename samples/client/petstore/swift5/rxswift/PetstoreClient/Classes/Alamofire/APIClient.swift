//
//  APIClient.swift
//  PetstoreClient
//
//  Created by dmatsudate on 2019/07/23.
//

import Foundation
import Alamofire

public class APIClient<Response> {

    public let baseURL: URL
    public let headers: [AnyHashable: Any]

    private let sessionManager: SessionManager
    private let queue = DispatchQueue(
        label: "com.openapi-generator.api-client",
        qos: .userInitiated,
        attributes: [.concurrent],
        autoreleaseFrequency: .inherit)

    public init(baseURL: URL, headers: [AnyHashable: Any] = [:]) {
        self.baseURL = baseURL
        self.headers = headers

        let config: URLSessionConfiguration = .ephemeral
        config.httpAdditionalHeaders = headers
        self.sessionManager = SessionManager(configuration: config)
    }

    private func url(request: RequestBuilder<Response>) -> URL {
        return baseURL.appendingPathComponent(request.endpoint)
    }

    func perform(request: RequestBuilder<Response>, completion: (Result<Response, Error>) -> Void) {
        queue.async { [weak self] in
            guard let self = self else { return }

            let url = self.url(request: request)
            let method = Alamofire.HTTPMethod(rawValue: request.method)!
            let parameters: Alamofire.Parameters = {
                var parameters = Alamofire.Parameters()
                if let form = request.parameters?.form {
                    parameters.merge(form.compactMapValues { $0 }, uniquingKeysWith: { $1 })
                }

                if let query = request.parameters?.query {
                    parameters.merge(query.compactMapValues { $0 }, uniquingKeysWith: { $1 })
                }
                return parameters
            }()

            if let form = request.parameters?.form {
                self.sessionManager.upload(multipartFormData: { (formData) in
                    form.compactMapValues { $0 }.forEach { key, value in
                        if let file = URL(string: value) {
                            formData.append(file, withName: key)
                        } else {
                            formData.append(value.data(using: .utf8)!, withName: key)
                        }
                    }

                }, to: url, method: method, encodingCompletion: { encodingResult in
                    switch encodingResult {
                    case .success(let uploadRequest, _, _):

                    }
                })
            } else {
                let url =
                let alamofireRequest = self.sessionManager.request(url(request: request), method: method, parameters: <#T##Parameters?#>, encoding: <#T##ParameterEncoding#>, headers: <#T##HTTPHeaders?#>)
            }


        }

    }

}
