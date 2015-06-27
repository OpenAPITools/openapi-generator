//
//  JSONModel+networking.h
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
#import "JSONHTTPClient.h"

typedef void (^JSONModelBlock)(id model, JSONModelError* err);

/**
 * The JSONModel(networking) class category adds networking to JSONModel.
 * It adds initFromURLWithString: initializer, which makes a GET http request 
 * to the URL given and initializes the model with the returned JSON.
 * Use #import "JSONModel+networking.h" to import networking capabilities.
 */
@interface JSONModel(Networking)

@property (assign, nonatomic) BOOL isLoading;
/** @name Asynchroniously create a model over the network */
/**
 * Asynchroniously create a model over the network. Create a new model instance and initialize it with the JSON fetched from the given URL
 * @param urlString the absolute URL address of the JSON feed as a string
 * @param completeBlock JSONModelBlock executed upon completion. The JSONModelBlock type is defined as: void (^JSONModelBlock)(JSONModel* model, JSONModelError* e); the first parameter is the initialized model or nil, 
 * and second parameter holds the model initialization error, if any
 */
-(instancetype)initFromURLWithString:(NSString *)urlString completion:(JSONModelBlock)completeBlock;

/**
 * Asynchronously gets the contents of a URL and constructs a JSONModel object from the response.
 * The constructed JSONModel object passed as the first parameter to the completion block will be of the same
 * class as the receiver. So call this method on yourJSONModel sub-class rather than directly on JSONModel.
 * @param	urlString		The absolute URL of the JSON resource, as a string
 * @param	completeBlock	The block to be called upon completion.
 *							JSONModelBlock type is defined as: void (^JSONModelBlock)(JSONModel* model, JSONModelError* err);
 *							The first parameter is the initialized model (of the same JSONModel sub-class as the receiver) or nil if there was an error;
 * 							The second parameter is the initialization error, if any.
 */
+ (void)getModelFromURLWithString:(NSString*)urlString completion:(JSONModelBlock)completeBlock;

/**
 * Asynchronously posts a JSONModel object (as JSON) to a URL and constructs a JSONModel object from the response.
 * The constructed JSONModel object passed as the first parameter to the completion block will be of the same
 * class as the receiver. So call this method on yourJSONModel sub-class rather than directly on JSONModel.
 * @param	post			A JSONModel object that will be converted to JSON and sent as the POST data to the HTTP request.
 * @param	urlString		The absolute URL of the JSON resource, as a string
 * @param	completeBlock	The block to be called upon completion.
 *							JSONModelBlock type is defined as: void (^JSONModelBlock)(JSONModel* model, JSONModelError* err);
 *							The first parameter is the initialized model (of the same JSONModel sub-class as the receiver) or nil if there was an error;
 * 							The second parameter is the initialization error, if any.
 */
+ (void)postModel:(JSONModel*)post toURLWithString:(NSString*)urlString completion:(JSONModelBlock)completeBlock;


@end
