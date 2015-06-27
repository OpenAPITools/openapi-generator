//
//  JSONKeyMapper.h
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

#import <Foundation/Foundation.h>

typedef NSString* (^JSONModelKeyMapBlock)(NSString* keyName);

/**
 * **You won't need to create or store instances of this class yourself.** If you want your model
 * to have different property names than the JSON feed keys, look below on how to 
 * make your model use a key mapper.
 *
 * For example if you consume JSON from twitter
 * you get back underscore_case style key names. For example:
 *
 * <pre>"profile_sidebar_border_color": "0094C2",
 * "profile_background_tile": false,</pre>
 *
 * To comply with Obj-C accepted camelCase property naming for your classes,
 * you need to provide mapping between JSON keys and ObjC property names.
 * 
 * In your model overwrite the +(JSONKeyMapper*)keyMapper method and provide a JSONKeyMapper
 * instance to convert the key names for your model.
 * 
 * If you need custom mapping it's as easy as:
 * <pre>
 * +(JSONKeyMapper*)keyMapper {
 * &nbsp; return [[JSONKeyMapper&nbsp;alloc]&nbsp;initWithDictionary:@{@"crazy_JSON_name":@"myCamelCaseName"}];
 * }
 * </pre>
 * In case you want to handle underscore_case, **use the predefined key mapper**, like so:
 * <pre>
 * +(JSONKeyMapper*)keyMapper {
 * &nbsp; return [JSONKeyMapper&nbsp;mapperFromUnderscoreCaseToCamelCase];
 * }
 * </pre>
 */
@interface JSONKeyMapper : NSObject

/** @name Name convertors */
/** Block, which takes in a JSON key and converts it to the corresponding property name */
@property (readonly, nonatomic) JSONModelKeyMapBlock JSONToModelKeyBlock;

/** Block, which takes in a property name and converts it to the corresponding JSON key name */
@property (readonly, nonatomic) JSONModelKeyMapBlock modelToJSONKeyBlock;

/** Combined convertor method
* @param value the source name
* @param importing YES invokes JSONToModelKeyBlock, NO - modelToJSONKeyBlock
* @return JSONKeyMapper instance
*/
-(NSString*)convertValue:(NSString*)value isImportingToModel:(BOOL)importing;

/** @name Creating a key mapper */

/**
 * Creates a JSONKeyMapper instance, based on the two blocks you provide this initializer.
 * The two parameters take in a JSONModelKeyMapBlock block:
 * <pre>NSString* (^JSONModelKeyMapBlock)(NSString* keyName)</pre>
 * The block takes in a string and returns the transformed (if at all) string.
 * @param toModel transforms JSON key name to your model property name
 * @param toJSON transforms your model property name to a JSON key
 */
-(instancetype)initWithJSONToModelBlock:(JSONModelKeyMapBlock)toModel
                       modelToJSONBlock:(JSONModelKeyMapBlock)toJSON;

/**
 * Creates a JSONKeyMapper instance, based on the mapping you provide
 * in the map parameter. Use the JSON key names as keys, your JSONModel 
 * property names as values.
 * @param map map dictionary, in the format: <pre>@{@"crazy_JSON_name":@"myCamelCaseName"}</pre>
 * @return JSONKeyMapper instance
 */
-(instancetype)initWithDictionary:(NSDictionary*)map;

/**
 * Creates a JSONKeyMapper, which converts underscore_case to camelCase and vice versa.
 */
+(instancetype)mapperFromUnderscoreCaseToCamelCase;

+(instancetype)mapperFromUpperCaseToLowerCase;
@end
