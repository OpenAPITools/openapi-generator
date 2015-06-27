//
//  JSONModelHTTPClient.h
//
//  @version 1.0.2
//  @author Marin Todorov, http://www.touch-code-magazine.com
//

// Copyright (c) 2012-2014 Marin Todorov, Underplot ltd.
// This code is distributed under the terms and conditions of the MIT license.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// The MIT License in plain English: http://www.touch-code-magazine.com/JSONModel/MITLicense

#import "JSONModel.h"

#pragma mark - definitions

/**
 * HTTP Request methods
 */
extern NSString* const kHTTPMethodGET;
extern NSString* const kHTTPMethodPOST;

/**
 * Content-type strings
 */
extern NSString* const kContentTypeAutomatic;
extern NSString* const kContentTypeJSON;
extern NSString* const kContentTypeWWWEncoded;

/**
 * A block type to handle incoming JSON object and an error. 
 * You pass it to methods which fetch JSON asynchroniously. When the operation is finished
 * you receive back the fetched JSON (or nil) and an error (or nil)
 *
 * @param json object derived from a JSON string
 * @param err JSONModelError or nil
 */
typedef void (^JSONObjectBlock)(id json, JSONModelError* err);

/////////////////////////////////////////////////////////////////////////////////////////////
#pragma mark - configuration methods

/**
 * @discussion A very thin HTTP client that can do GET and POST HTTP requests.
 * It fetches only JSON data and also deserializes it using NSJSONSerialization.
 */
@interface JSONHTTPClient : NSObject

/////////////////////////////////////////////////////////////////////////////////////////////


/** @name HTTP Client configuration */
/**
 * Returns a modifyable dictionary of the client's default HTTP headers.
 * @result A mutable dictionary of pairs - HTTP header names and values.
 * @discussion You can use the result to modify the http client headers like so:
 * <pre>
 * NSMutableDictionary* headers = [JSONHTTPClient requestHeaders];
 * headers[@"APIToken"] = @"MySecretTokenValue";
 * </pre>
 */
+(NSMutableDictionary*)requestHeaders;

/**
 * Sets the default encoding of the request body.
 * See NSStringEncoding for a list of supported encodings
 * @param encoding text encoding constant
 */
+(void)setDefaultTextEncoding:(NSStringEncoding)encoding;

/**
 * Sets the policies for caching HTTP data
 * See NSURLRequestCachePolicy for a list of the pre-defined policies
 * @param policy the caching policy
 */
+(void)setCachingPolicy:(NSURLRequestCachePolicy)policy;

/**
 * Sets the timeout for network calls
 * @param seconds the amount of seconds to wait before considering the call failed
 */
+(void)setTimeoutInSeconds:(int)seconds;

/**
 * A method to set the default conent type of the request body
 * By default the content type is set to kContentTypeAutomatic
 * which checks the body request and decides between "application/json"
 * and "application/x-www-form-urlencoded"
 */
+(void)setRequestContentType:(NSString*)contentTypeString;

/////////////////////////////////////////////////////////////////////////////////////////////
#pragma mark - GET asynchronious JSON calls

/** @name Making asynchronious HTTP requests */
/**
 * Makes GET request to the given URL address and fetches a JSON response.
 * @param urlString the URL as a string
 * @param completeBlock JSONObjectBlock to execute upon completion
 */
+(void)getJSONFromURLWithString:(NSString*)urlString completion:(JSONObjectBlock)completeBlock;

/**
 * Makes GET request to the given URL address and fetches a JSON response. Sends the params as a query string variables.
 * @param urlString the URL as a string
 * @param params a dictionary of key / value pairs to be send as variables to the request
 * @param completeBlock JSONObjectBlock to execute upon completion
 */
+(void)getJSONFromURLWithString:(NSString*)urlString params:(NSDictionary*)params completion:(JSONObjectBlock)completeBlock;

/**
 * Makes a request to the given URL address and fetches a JSON response.
 * @param urlString the URL as a string
 * @param method the method of the request as a string
 * @param params a dictionary of key / value pairs to be send as variables to the request
 * @param bodyString the body of the POST request as a string
 * @param completeBlock JSONObjectBlock to execute upon completion
 */
+(void)JSONFromURLWithString:(NSString*)urlString method:(NSString*)method params:(NSDictionary*)params orBodyString:(NSString*)bodyString completion:(JSONObjectBlock)completeBlock;

/**
 * Makes a request to the given URL address and fetches a JSON response.
 * @param urlString the URL as a string
 * @param method the method of the request as a string
 * @param params a dictionary of key / value pairs to be send as variables to the request
 * @param bodyString the body of the POST request as a string
 * @param headers the headers to set on the request - overrides those in +requestHeaders
 * @param completeBlock JSONObjectBlock to execute upon completion
 */
+(void)JSONFromURLWithString:(NSString*)urlString method:(NSString*)method params:(NSDictionary*)params orBodyString:(NSString*)bodyString headers:(NSDictionary*)headers completion:(JSONObjectBlock)completeBlock;

/**
 * Makes a request to the given URL address and fetches a JSON response.
 * @param urlString the URL as a string
 * @param method the method of the request as a string
 * @param params a dictionary of key / value pairs to be send as variables to the request
 * @param bodyData the body of the POST request as raw binary data
 * @param headers the headers to set on the request - overrides those in +requestHeaders
 * @param completeBlock JSONObjectBlock to execute upon completion
 */
+(void)JSONFromURLWithString:(NSString*)urlString method:(NSString*)method params:(NSDictionary *)params orBodyData:(NSData*)bodyData headers:(NSDictionary*)headers completion:(JSONObjectBlock)completeBlock;

/////////////////////////////////////////////////////////////////////////////////////////////
#pragma mark - POST synchronious JSON calls

/**
 * Makes POST request to the given URL address and fetches a JSON response. Sends the bodyString param as the POST request body.
 * @param urlString the URL as a string
 * @param params a dictionary of key / value pairs to be send as variables to the request
 * @param completeBlock JSONObjectBlock to execute upon completion
 */
+(void)postJSONFromURLWithString:(NSString*)urlString params:(NSDictionary*)params completion:(JSONObjectBlock)completeBlock;

/**
 * Makes POST request to the given URL address and fetches a JSON response. Sends the bodyString param as the POST request body.
 * @param urlString the URL as a string
 * @param bodyString the body of the POST request as a string
 * @param completeBlock JSONObjectBlock to execute upon completion
 */
+(void)postJSONFromURLWithString:(NSString*)urlString bodyString:(NSString*)bodyString completion:(JSONObjectBlock)completeBlock;

/**
 * Makes POST request to the given URL address and fetches a JSON response. Sends the bodyString param as the POST request body.
 * @param urlString the URL as a string
 * @param bodyData the body of the POST request as an NSData object
 * @param completeBlock JSONObjectBlock to execute upon completion
 */
+(void)postJSONFromURLWithString:(NSString*)urlString bodyData:(NSData*)bodyData completion:(JSONObjectBlock)completeBlock;


@end
