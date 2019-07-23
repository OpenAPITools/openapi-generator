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
    private let queue = DispatchQueue(label: "com.open-api-generator.api-client", qos: .userInitiated)

    public init(baseURL: URL, headers: [AnyHashable: Any] = [:]) {
        self.baseURL = baseURL
        self.headers = headers

        let config: URLSessionConfiguration = .ephemeral
        config.httpAdditionalHeaders = headers
        self.sessionManager = SessionManager(configuration: config)
    }

    func perform(request: RequestBuilder<Response>, completion: (Result<Response, Error>) -> Void) {
        queue.async { [weak self] in
            guard let self = self else { return }

            self?.session

        }

    }


}
