//
//  JSONModelClassProperty.h
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

enum kCustomizationTypes {
    kNotInspected = 0,
    kCustom,
    kNo
    };

typedef enum kCustomizationTypes PropertySetterType;
typedef enum kCustomizationTypes PropertyGetterType;

/**
 * **You do not need to instantiate this class yourself.** This class is used internally by JSONModel
 * to inspect the declared properties of your model class.
 *
 * Class to contain the information, representing a class property
 * It features the property's name, type, whether it's a required property, 
 * and (optionally) the class protocol
 */
@interface JSONModelClassProperty : NSObject

/** The name of the declared property (not the ivar name) */
@property (copy, nonatomic) NSString* name;

/** A property class type  */
@property (assign, nonatomic) Class type;

/** Struct name if a struct */
@property (strong, nonatomic) NSString* structName;

/** The name of the protocol the property conforms to (or nil) */
@property (copy, nonatomic) NSString* protocol;

/** If YES, it can be missing in the input data, and the input would be still valid */
@property (assign, nonatomic) BOOL isOptional;

/** If YES - don't call any transformers on this property's value */
@property (assign, nonatomic) BOOL isStandardJSONType;

/** If YES - create a mutable object for the value of the property */
@property (assign, nonatomic) BOOL isMutable;

/** If YES - create models on demand for the array members */
@property (assign, nonatomic) BOOL convertsOnDemand;

/** If YES - the value of this property determines equality to other models */
@property (assign, nonatomic) BOOL isIndex;

/** The status of property getter introspection in a model */
@property (assign, nonatomic) PropertyGetterType getterType;

/** a custom getter for this property, found in the owning model */
@property (assign, nonatomic) SEL customGetter;

/** The status of property setter introspection in a model */
@property (assign, nonatomic) PropertySetterType setterType;

/** a custom setter for this property, found in the owning model */
@property (assign, nonatomic) SEL customSetter;

@end
