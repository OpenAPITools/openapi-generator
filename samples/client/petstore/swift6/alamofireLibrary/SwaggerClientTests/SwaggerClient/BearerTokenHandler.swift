//
//  BearerTokenHandler.swift
//  SwaggerClient
//
//  Created by Bruno Coelho on 31/12/2020.
//  Copyright © 2020 Swagger. All rights reserved.
//

import Foundation
import Alamofire
import PetstoreClient

class BearerTokenHandler: RequestInterceptor, @unchecked Sendable {
    private var bearerToken: String? = nil
    
    func adapt(_ urlRequest: URLRequest, for session: Session, completion: @escaping (Result<URLRequest, Error>) -> Void) {
        if let bearerToken = bearerToken {
            var urlRequest = urlRequest
            urlRequest.setValue("Bearer \(bearerToken)", forHTTPHeaderField: "Authorization")
            
            completion(.success(urlRequest))
            return
        }
        
        completion(.success(urlRequest))
    }
    
    func retry(_ request: Request, for session: Session, dueTo error: Error, completion: @escaping (RetryResult) -> Void) {
        if let response = request.task?.response as? HTTPURLResponse, response.statusCode == 401 {
            startRefreshingToken { isTokenRefreshed in
                completion(.retry)
            }
        } else {
            completion(.doNotRetryWithError(error))
        }
    }
    
    private func startRefreshingToken(completionHandler: @escaping (Bool) -> Void) {
        // Get a bearer token
        let dummyBearerToken = "..."
        
        bearerToken = dummyBearerToken
        PetstoreClientAPIConfiguration.shared.customHeaders["Authorization"] = "Bearer \(dummyBearerToken)"
        
        completionHandler(true)
    }
}
