//
//  BearerDecodableRequestBuilder.swift
//  SwaggerClient
//
//  Created by Bruno Coelho on 31/12/2020.
//  Copyright © 2020 Swagger. All rights reserved.
//

import Foundation
import Alamofire
import PetstoreClient

class BearerRequestBuilderFactory: RequestBuilderFactory {
    func getNonDecodableBuilder<T>() -> RequestBuilder<T>.Type {
        BearerRequestBuilder<T>.self
    }
    
    func getBuilder<T: Decodable>() -> RequestBuilder<T>.Type {
        BearerDecodableRequestBuilder<T>.self
    }
}

class BearerRequestBuilder<T>: AlamofireRequestBuilder<T> {
    override func createSessionManager() -> SessionManager {
        let sessionManager = super.createSessionManager()
        
        let bearerTokenHandler = BearerTokenHandler()
        sessionManager.adapter = bearerTokenHandler
        sessionManager.retrier = bearerTokenHandler
        
        return sessionManager
    }
}

class BearerDecodableRequestBuilder<T: Decodable>: AlamofireDecodableRequestBuilder<T> {
    override func createSessionManager() -> SessionManager {
        let sessionManager = super.createSessionManager()
        
        let bearerTokenHandler = BearerTokenHandler()
        sessionManager.adapter = bearerTokenHandler
        sessionManager.retrier = bearerTokenHandler
        
        return sessionManager
    }
}

class BearerTokenHandler: RequestAdapter, RequestRetrier {
    private static var bearerToken: String? = nil
    
    func adapt(_ urlRequest: URLRequest) throws -> URLRequest {
        if let bearerToken = Self.bearerToken {
            var urlRequest = urlRequest
            urlRequest.setValue("Bearer \(bearerToken)", forHTTPHeaderField: "Authorization")
            return urlRequest
        }
        
        return urlRequest
    }
    
    func should(_: SessionManager, retry request: Request, with _: Error, completion: @escaping RequestRetryCompletion) {
        if let response = request.task?.response as? HTTPURLResponse, response.statusCode == 401 {
            Self.startRefreshingToken { isTokenRefreshed in
                completion(isTokenRefreshed, 0.0)
            }
        } else {
            completion(false, 0.0)
        }
    }
    
    private static func startRefreshingToken(completionHandler: @escaping (Bool) -> Void) {
        // Get a bearer token
        let dummyBearerToken = "..."
        
        bearerToken = dummyBearerToken
        PetstoreClientAPI.customHeaders["Authorization"] = "Bearer \(dummyBearerToken)"
        
        completionHandler(true)
    }
}
