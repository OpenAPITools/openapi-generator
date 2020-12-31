//
//  BearerDecodableRequestBuilder.swift
//  SwaggerClient
//
//  Created by Bruno Coelho on 31/12/2020.
//  Copyright © 2020 Swagger. All rights reserved.
//

import Foundation
import PetstoreClient

class BearerRequestBuilderFactory: RequestBuilderFactory {
    func getNonDecodableBuilder<T>() -> RequestBuilder<T>.Type {
        BearerRequestBuilder<T>.self
    }

    func getBuilder<T: Decodable>() -> RequestBuilder<T>.Type {
        BearerDecodableRequestBuilder<T>.self
    }
}

class BearerRequestBuilder<T>: URLSessionRequestBuilder<T> {
    override func execute(_ apiResponseQueue: DispatchQueue = PetstoreClientAPI.apiResponseQueue, _ completion: @escaping (Result<Response<T>, Error>) -> Void) {

        BearerTokenHandler.refreshTokenIfDoesntExist {
            
            super.execute(apiResponseQueue) { result in
                
                switch result {
                case .success:
                    completion(result)
                    
                case let .failure(error):
                    
                    if case let ErrorResponse.error(_, data, response, error) = error {
                        BearerTokenHandler.refreshTokenIfUnauthorizedRequestResponse(
                            data: data,
                            response: response,
                            error: error
                        ) { isTokenRefreshed in
                            if isTokenRefreshed {
                                self.execute(apiResponseQueue, completion)
                            } else {
                                completion(result)
                            }
                        }
                    } else {
                        completion(result)
                    }
                    
                }
            }
        }
    }
}

class BearerDecodableRequestBuilder<T: Decodable>: URLSessionDecodableRequestBuilder<T> {
    override func execute(_ apiResponseQueue: DispatchQueue = PetstoreClientAPI.apiResponseQueue, _ completion: @escaping (Result<Response<T>, Error>) -> Void) {

        BearerTokenHandler.refreshTokenIfDoesntExist {
            
            super.execute(apiResponseQueue) { result in
                
                switch result {
                case .success:
                    completion(result)
                    
                case let .failure(error):
                    
                    if case let ErrorResponse.error(_, data, response, error) = error {
                        BearerTokenHandler.refreshTokenIfUnauthorizedRequestResponse(
                            data: data,
                            response: response,
                            error: error
                        ) { isTokenRefreshed in
                            if isTokenRefreshed {
                                self.execute(apiResponseQueue, completion)
                            } else {
                                completion(result)
                            }
                        }
                    } else {
                        completion(result)
                    }
                    
                }
            }
        }
    }
}

class BearerTokenHandler {
    private static var bearerToken: String? = nil
    
    static func refreshTokenIfDoesntExist(completionHandler: @escaping () -> Void) {
        
        if bearerToken != nil {
            completionHandler()
        } else {
            startRefreshingToken {
                completionHandler()
            }
        }
    }
    
    static func refreshTokenIfUnauthorizedRequestResponse(data: Data?, response: URLResponse?, error: Error?, completionHandler: @escaping (Bool) -> Void) {
        guard let response = response as? HTTPURLResponse else {
            completionHandler(false)
            return
        }

        if response.statusCode == 401 {
            startRefreshingToken {
                completionHandler(true)
            }
        } else {
            completionHandler(false)
        }
    }
    
    private static func startRefreshingToken(completionHandler: @escaping () -> Void) {
        // Get a bearer token
        let dummyBearerToken = "..."
        
        bearerToken = dummyBearerToken
        PetstoreClientAPI.customHeaders["Authorization"] = "Bearer \(dummyBearerToken)"

        completionHandler()
    }
}
