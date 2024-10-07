//
//  BearerTokenHandler.swift
//  SwaggerClient
//
//  Created by Bruno Coelho on 31/12/2020.
//  Copyright © 2020 Swagger. All rights reserved.
//

import Foundation
import PetstoreClient

public class BearerOpenAPIInterceptor: OpenAPIInterceptor {
    public init() {}
    
    public func intercept(urlRequest: URLRequest, urlSession: URLSessionProtocol, openAPIClient: OpenAPIClient, completion: @escaping (Result<URLRequest, any Error>) -> Void) {
        BearerTokenHandler.shared.refreshTokenIfDoesntExist { token in
            
            // Change the current url request
            var newUrlRequest = urlRequest
            newUrlRequest.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
            
            // Change the global headers
            openAPIClient.customHeaders["Authorization"] = "Bearer \(token)"
            
            completion(.success(newUrlRequest))
        }
    }
    
    public func retry(urlRequest: URLRequest, urlSession: URLSessionProtocol, openAPIClient: OpenAPIClient, data: Data?, response: URLResponse, error: Error, completion: @escaping (OpenAPIInterceptorRetry) -> Void) {
        // We will analyse the response to see if it's a 401, and if it's a 401, we will refresh the token and retry the request
        BearerTokenHandler.shared.refreshTokenIfUnauthorizedRequestResponse(
            data: data,
            response: response,
            error: error
        ) { (wasTokenRefreshed, newToken) in
            
            if wasTokenRefreshed, let newToken = newToken {
                
                // Change the global headers
                openAPIClient.customHeaders["Authorization"] = "Bearer \(newToken)"
                
                completion(.retry)
            } else {
                // If the token was not refreshed, it's because it was not a 401 error, so we send the response to the completion block
                completion(.dontRetry)
            }
        }
    }
}

class BearerTokenHandler: @unchecked Sendable {
    private init() {}
    static let shared = BearerTokenHandler()

    
    private var bearerToken: String? = nil
    
    func refreshTokenIfDoesntExist(completionHandler: @escaping (String) -> Void) {
        if let bearerToken = bearerToken {
            completionHandler(bearerToken)
        } else {
            startRefreshingToken { token in
                completionHandler(token)
            }
        }
    }
    
    func refreshTokenIfUnauthorizedRequestResponse(data: Data?, response: URLResponse, error: Error, completionHandler: @escaping (Bool, String?) -> Void) {
        if let response = response as? HTTPURLResponse, response.statusCode == 401 {
            startRefreshingToken { token in
                completionHandler(true, token)
            }
        } else {
            completionHandler(false, nil)
        }
    }
    
    private func startRefreshingToken(completionHandler: @escaping (String) -> Void) {
        // Get a bearer token
        let dummyBearerToken = "..."
        
        bearerToken = dummyBearerToken

        completionHandler(dummyBearerToken)
    }
}
