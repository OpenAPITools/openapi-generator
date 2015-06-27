//
//  JSONValueTransformer.h
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
#import "JSONModelArray.h"

/////////////////////////////////////////////////////////////////////////////////////////////

#pragma mark - extern definitons
/**
 * Boolean function to check for null values. Handy when you need to both check
 * for nil and [NSNUll null]
 */
extern BOOL isNull(id value);

/////////////////////////////////////////////////////////////////////////////////////////////

#pragma mark - JSONValueTransformer interface
/**
 * **You don't need to call methods of this class manually.** 
 *
 * Class providing methods to transform values from one class to another.
 * You are given a number of built-in transformers, but you are encouraged to
 * extend this class with your own categories to add further value transformers.
 * Just few examples of what can you add to JSONValueTransformer: hex colors in JSON to UIColor,
 * hex numbers in JSON to NSNumber model properties, base64 encoded strings in JSON to UIImage properties, and more.
 *
 * The class is invoked by JSONModel while transforming incoming
 * JSON types into your target class property classes, and vice versa.
 * One static copy is create and store in the JSONModel class scope.
 */
@interface JSONValueTransformer : NSObject

@property (strong, nonatomic, readonly) NSDictionary* primitivesNames;

/** @name Resolving cluster class names */
/**
 * This method returns the ubmrella class for any standard class cluster members.
 * For example returns NSString when given as input NSString, NSMutableString, __CFString and __CFConstantString
 * The method currently looksup a pre-defined list.
 * @param sourceClass the class to get the umrella class for
 * @return Class
 */
+(Class)classByResolvingClusterClasses:(Class)sourceClass;

#pragma mark - NSMutableString <-> NSString
/** @name Transforming to Mutable copies */
/**
 * Trasnforms a string value to a mutable string value
 * @param string incoming string
 * @return mutable string
 */
-(NSMutableString*)NSMutableStringFromNSString:(NSString*)string;

#pragma mark - NSMutableArray <-> NSArray
/**
 * Trasnforms an array to a mutable array
 * @param array incoming array
 * @return mutable array
 */
-(NSMutableArray*)NSMutableArrayFromNSArray:(NSArray*)array;

#pragma mark - NS(Mutable)Array <- JSONModelArray
/**
 * Trasnforms an array to a JSONModelArray
 * @param array incoming array
 * @return JSONModelArray
 */
-(NSArray*)NSArrayFromJSONModelArray:(JSONModelArray*)array;
-(NSMutableArray*)NSMutableArrayFromJSONModelArray:(JSONModelArray*)array;

#pragma mark - NSMutableDictionary <-> NSDictionary
/**
 * Trasnforms a dictionary to a mutable dictionary
 * @param dict incoming dictionary
 * @return mutable dictionary
 */
-(NSMutableDictionary*)NSMutableDictionaryFromNSDictionary:(NSDictionary*)dict;

#pragma mark - NSSet <-> NSArray
/** @name Transforming Sets */
/**
 * Transforms an array to a set
 * @param array incoming array
 * @return set with the array's elements
 */
-(NSSet*)NSSetFromNSArray:(NSArray*)array;

/**
 * Transforms an array to a mutable set
 * @param array incoming array
 * @return mutable set with the array's elements
 */
-(NSMutableSet*)NSMutableSetFromNSArray:(NSArray*)array;

/**
 * Transforms a set to an array
 * @param set incoming set
 * @return an array with the set's elements
 */
-(NSArray*)JSONObjectFromNSSet:(NSSet*)set;

/**
 * Transforms a mutable set to an array
 * @param set incoming mutable set
 * @return an array with the set's elements
 */
-(NSArray*)JSONObjectFromNSMutableSet:(NSMutableSet*)set;

#pragma mark - BOOL <-> number/string
/** @name Transforming JSON types */
/**
 * Transforms a number object to a bool number object
 * @param number the number to convert
 * @return the resulting number
 */
-(NSNumber*)BOOLFromNSNumber:(NSNumber*)number;

/**
 * Transforms a number object to a bool number object
 * @param string the string value to convert, "0" converts to NO, everything else to YES
 * @return the resulting number
 */
-(NSNumber*)BOOLFromNSString:(NSString*)string;

/**
 * Transforms a BOOL value to a bool number object
 * @param number an NSNumber value coming from the model
 * @return the result number
 */
-(NSNumber*)JSONObjectFromBOOL:(NSNumber*)number;

#pragma mark - string <-> number
/**
 * Transforms a string object to a number object
 * @param string the string to convert
 * @return the resulting number
 */
-(NSNumber*)NSNumberFromNSString:(NSString*)string;

/**
 * Transforms a number object to a string object
 * @param number the number to convert
 * @return the resulting string
 */
-(NSString*)NSStringFromNSNumber:(NSNumber*)number;

/**
 * Transforms a string object to a nsdecimalnumber object
 * @param string the string to convert
 * @return the resulting number
 */
-(NSDecimalNumber*)NSDecimalNumberFromNSString:(NSString*)string;

/**
 * Transforms a nsdecimalnumber object to a string object
 * @param number the number to convert
 * @return the resulting string
 */
-(NSString*)NSStringFromNSDecimalNumber:(NSDecimalNumber*)number;


#pragma mark - string <-> url
/** @name Transforming URLs */
/**
 * Transforms a string object to an NSURL object
 * @param string the string to convert
 * @return the resulting url object
 */
-(NSURL*)NSURLFromNSString:(NSString*)string;

/**
 * Transforms an NSURL object to a string
 * @param url the url object to convert
 * @return the resulting string
 */
-(NSString*)JSONObjectFromNSURL:(NSURL*)url;

#pragma mark - string <-> time zone

/** @name Transforming NSTimeZone */
/**
 * Transforms a string object to an NSTimeZone object
 * @param string the string to convert
 * @return the resulting NSTimeZone object
 */
- (NSTimeZone *)NSTimeZoneFromNSString:(NSString*)string;

/**
 * Transforms an NSTimeZone object to a string
 * @param timeZone the time zone object to convert
 * @return the resulting string
 */
- (NSString *)JSONObjectFromNSTimeZone:(NSTimeZone *)timeZone;

#pragma mark - string <-> date
/** @name Transforming Dates */
/**
 * The following two methods are not public. This way if there is a category on converting 
 * dates it'll override them. If there isn't a category the default methods found in the .m
 * file will be invoked. If these are public a warning is produced at the point of overriding
 * them in a category, so they have to stay hidden here.
 */

//-(NSDate*)NSDateFromNSString:(NSString*)string;
//-(NSString*)JSONObjectFromNSDate:(NSDate*)date;

#pragma mark - number <-> date

/**
 * Transforms a number to an NSDate object
 * @param number the number to convert
 * @return the resulting date
 */
- (NSDate*)NSDateFromNSNumber:(NSNumber*)number;

@end
