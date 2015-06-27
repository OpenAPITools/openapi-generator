//
//  JSONModelClassProperty.m
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

#import "JSONModelClassProperty.h"

@implementation JSONModelClassProperty

-(NSString*)description
{
    //build the properties string for the current class property
    NSMutableArray* properties = [NSMutableArray arrayWithCapacity:8];
    
    if (self.isIndex) [properties addObject:@"Index"];
    if (self.isOptional) [properties addObject:@"Optional"];
    if (self.isMutable) [properties addObject:@"Mutable"];
    if (self.convertsOnDemand) [properties addObject:@"ConvertOnDemand"];
    if (self.isStandardJSONType) [properties addObject:@"Standard JSON type"];
    if (self.customSetter) [properties addObject:[NSString stringWithFormat: @"Setter = %@", NSStringFromSelector(self.customSetter)]];
    if (self.customGetter) [properties addObject:[NSString stringWithFormat: @"Getter = %@", NSStringFromSelector(self.customGetter)]];
    
    NSString* propertiesString = @"";
    if (properties.count>0) {
        propertiesString = [NSString stringWithFormat:@"(%@)", [properties componentsJoinedByString:@", "]];
    }
    
    //return the name, type and additional properties
    return [NSString stringWithFormat:@"@property %@%@ %@ %@",
            self.type?[NSString stringWithFormat:@"%@*",self.type]:(self.structName?self.structName:@"primitive"),
            self.protocol?[NSString stringWithFormat:@"<%@>", self.protocol]:@"",
            self.name,
            propertiesString
            ];
}

@end
