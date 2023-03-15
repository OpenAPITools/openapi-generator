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
    override func createAlamofireSession(interceptor: RequestInterceptor? = nil) -> Session {
        let bearerTokenHandler = BearerTokenHandler()

        let alamofireSession = super.createAlamofireSession(interceptor: bearerTokenHandler)
        
        return alamofireSession
    }
}

class BearerDecodableRequestBuilder<T: Decodable>: AlamofireDecodableRequestBuilder<T> {
    override func createAlamofireSession(interceptor: RequestInterceptor? = nil) -> Session {
        let bearerTokenHandler = BearerTokenHandler()
        
        let alamofireSession = super.createAlamofireSession(interceptor: bearerTokenHandler)
        
        return alamofireSession
    }
}

class BearerTokenHandler: RequestInterceptor {
    private static var bearerToken: String? = nil
    
    func adapt(_ urlRequest: URLRequest, for session: Session, completion: @escaping (Result<URLRequest, Error>) -> Void) {
        if let bearerToken = Self.bearerToken {
            var urlRequest = urlRequest
            urlRequest.setValue("Bearer \(bearerToken)", forHTTPHeaderField: "Authorization")
            
            completion(.success(urlRequest))
            return
        }
        
        completion(.success(urlRequest))
    }
    
    func retry(_ request: Request, for session: Session, dueTo error: Error, completion: @escaping (RetryResult) -> Void) {
        if let response = request.task?.response as? HTTPURLResponse, response.statusCode == 401 {
            Self.startRefreshingToken { isTokenRefreshed in
                completion(.retry)
            }
        } else {
            completion(.doNotRetryWithError(error))
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
