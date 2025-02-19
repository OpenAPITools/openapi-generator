#import <UIKit/UIKit.h>
#import <XCTest/XCTest.h>
#import <SwaggerClient/SWGApiClient.h>
#import <SwaggerClient/SWGPet.h>

@interface DeserializationTest : XCTestCase {

@private SWGApiClient *apiClient;
    
}

@end

@implementation DeserializationTest

- (void)setUp {
    [super setUp];
    apiClient = [[SWGApiClient alloc] init];
}

- (void)testDeserializeDate {
    NSString *dateStr = @"2012-09-27";
    
    NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
    NSTimeZone *timezone = [NSTimeZone timeZoneWithName:@"UTC"];
    [formatter setTimeZone:timezone];
    [formatter setDateFormat:@"yyyy-MM-dd"];
    NSDate *date = [formatter dateFromString:dateStr];
    NSError* error;
    NSDate *deserializedDate = [apiClient.responseDeserializer deserialize:dateStr class:@"NSDate*" error:&error];
    XCTAssertNil(error);
    XCTAssertEqualWithAccuracy([date timeIntervalSinceReferenceDate], [deserializedDate timeIntervalSinceReferenceDate], 0.001);
}

- (void)testDeserializeInvalidDate {
    NSString *dateStr = @"random string";

    NSError* error;
    NSDate *deserializedDate = [apiClient.responseDeserializer deserialize:dateStr class:@"NSDate*" error:&error];
    XCTAssertNotNil(error);
    XCTAssertNil(deserializedDate);
}

- (void)testDeserializeEmptyDate {
    NSString *dateStr = @"";
    NSError* error;
    NSDate *deserializedDate = [apiClient.responseDeserializer deserialize:dateStr class:@"NSDate*" error:&error];
    XCTAssertNil(error);
    XCTAssertNil(deserializedDate);
}

- (void)testDeserializeDateTime {
    NSString *dateTimeStr = @"1997-07-16T19:20:30+00:00";
    
    NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
    [formatter setDateFormat:@"yyyy-MM-dd'T'HH:mm:ssZ"];
    NSDate *dateTime = [formatter dateFromString:dateTimeStr];
    NSError* error;
    NSDate *deserializedDateTime = [apiClient.responseDeserializer deserialize:dateTimeStr class:@"NSDate*" error:&error];
    XCTAssertNil(error);
    XCTAssertEqualWithAccuracy([dateTime timeIntervalSinceReferenceDate], [deserializedDateTime timeIntervalSinceReferenceDate], 0.001);
}

- (void)testDeserializeUnknownObject {
    NSString *data = @"random string";
    NSError* error;
    NSNumber *result = [apiClient.responseDeserializer deserialize:data class:@"DeserializationTest*" error:&error];
    XCTAssertNotNil(error);
    XCTAssertNil(result);
}

- (void)testDeserializeObject {
    NSNumber *data = @1;
    NSError* error;
    NSNumber *result = [apiClient.responseDeserializer deserialize:data class:@"NSObject*" error:&error];
    XCTAssertNil(error);
    XCTAssertEqualObjects(data, result);
}

- (void)testDeserializeString {
    NSString *data = @"test string";
    NSError* error;
    NSString *result = [apiClient.responseDeserializer deserialize:data class:@"NSString*" error:&error];
    XCTAssertNil(error);
    XCTAssertTrue([result isEqualToString:data]);
}

- (void)testDeserializeListOfString {
    NSArray *data = @[@"test string"];
    NSError* error;
    NSArray *result = [apiClient.responseDeserializer deserialize:data class:@"NSArray<NSString*>*" error:&error];
    XCTAssertNil(error);
    XCTAssertTrue([result isKindOfClass:[NSArray class]]);
    XCTAssertTrue([result[0] isKindOfClass:[NSString class]]);
}

- (void)testDeserializeInvalidListOfNumbers {
    NSArray *data = @[@"test string"];
    NSError* error;
    NSArray *result = [apiClient.responseDeserializer deserialize:data class:@"NSArray<NSNumber*>*" error:&error];
    XCTAssertNotNil(error);
    XCTAssertNil(result);
}

- (void)testDeserializeListOfNumbers {
    NSArray *data = @[@"1.0"];
    NSError* error;
    NSArray *result = [apiClient.responseDeserializer deserialize:data class:@"NSArray<NSNumber*>*" error:&error];
    XCTAssertNil(error);
    XCTAssertTrue([result isKindOfClass:[NSArray class]]);
    XCTAssertTrue([result[0] isKindOfClass:[NSNumber class]]);
}

- (void)testDeserializeListOfModels {
    NSArray *data =
    @[
      @{
          @"id": @119,
          @"category": @{
                  @"id": @0,
                  @"name": @"string"
                  },
          @"name": @"doggie",
          @"photoUrls": @[
                  @"string"
                  ],
          @"tags": @[
                  @{
                      @"id": @0,
                      @"name": @"string"
                      }
                  ],
          @"status": @"available"
          
          }];
    NSError* error;
    NSArray *result = [apiClient.responseDeserializer deserialize:data class:@"NSArray<SWGPet>*" error:&error];
    
    XCTAssertTrue([result isKindOfClass:[NSArray class]]);
    XCTAssertTrue([[result firstObject] isKindOfClass:[SWGPet class]]);
    SWGPet*pet = [result firstObject];
    XCTAssertEqualObjects([pet.photoUrls firstObject],@"string");
    XCTAssertTrue([[pet.tags firstObject] isKindOfClass:[SWGTag class]]);
    SWGTag* tag = [pet.tags firstObject];
    XCTAssertEqualObjects(tag._id, @0);
    XCTAssertEqualObjects(tag.name, @"string");
    XCTAssertEqualObjects(pet._id, @119);
}

- (void)testDeserializeMapOfModels {
    NSDictionary *data =
    @{
      @"pet": @{
              @"id": @119,
              @"category": @{
                      @"id": @0,
                      @"name": @"string"
                      },
              @"name": @"doggie",
              @"photoUrls": @[
                      @"string"
                      ],
              @"tags": @[
                      @{
                          @"id": @0,
                          @"name": @"string"
                          }
                      ],
              @"status": @"available"
              
              }
      };
    NSError* error;
    NSDictionary *result = [apiClient.responseDeserializer deserialize:data class:@"NSDictionary* /* NSString, SWGPet */" error:&error];
    
    XCTAssertTrue([result isKindOfClass:[NSDictionary class]]);
    XCTAssertTrue([result[@"pet"] isKindOfClass:[SWGPet class]]);
    XCTAssertEqualObjects([result[@"pet"] _id], @119);
}

- (void)testDeserializeNestedMap {
    NSDictionary *data =
    @{
      @"foo": @{
              @"bar": @1,
              @"bar2": [NSNull null]
              }
    };
    SWGResponseDeserializer* responseDeserializer = [[SWGResponseDeserializer alloc] init];
    NSError* error;
    NSDictionary *result = [responseDeserializer deserialize:data class:@"NSDictionary* /* NSString, NSDictionary* /* NSString, NSNumber */ */" error:&error];
    
    XCTAssertTrue([result isKindOfClass:[NSDictionary class]]);
    XCTAssertTrue([result[@"foo"] isKindOfClass:[NSDictionary class]]);
    XCTAssertTrue([result[@"foo"][@"bar"] isKindOfClass:[NSNumber class]]);
}


- (void)testDeserializeNestedMapWithNullValue {
    NSDictionary *data =
    @{
      @"foo": @{
              @"bar": @1,
              @"bar2": [NSNull null]
              }
      };
    SWGResponseDeserializer* responseDeserializer = [[SWGResponseDeserializer alloc] init];
    responseDeserializer.treatNullAsError = YES;
    NSError* error;
    NSDictionary *result = [responseDeserializer deserialize:data class:@"NSDictionary* /* NSString, NSDictionary* /* NSString, NSNumber */ */" error:&error];
    XCTAssertNil(result);
    XCTAssertNotNil(error);
}

- (void)testDeserializeNestedMap2 {
    NSDictionary *data = @{
      @"foo": @{
              @"bar": @1
              }
      };
    NSError* error;
    NSDictionary *result = [apiClient.responseDeserializer deserialize:data class:@"NSDictionary<NSString*, NSDictionary<NSString*, NSNumber*>*>*" error:&error];
    
    XCTAssertTrue([result isKindOfClass:[NSDictionary class]]);
    XCTAssertTrue([result[@"foo"] isKindOfClass:[NSDictionary class]]);
    XCTAssertTrue([result[@"foo"][@"bar"] isKindOfClass:[NSNumber class]]);
}

- (void)testDeserializeNestedList {
    NSArray *data = @[@[@"foo"]];
    NSError* error;
    NSArray *result = [apiClient.responseDeserializer deserialize:data class:@"NSArray* /* NSArray* /* NSString */ */" error:&error];
    
    XCTAssertTrue([result isKindOfClass:[NSArray class]]);
    XCTAssertTrue([result[0] isKindOfClass:[NSArray class]]);
    XCTAssertTrue([result[0][0] isKindOfClass:[NSString class]]);
}

- (void)testDeserializeBool {
    NSString *data;
    NSNumber *result;
    
    data = @"true";
    NSError* error;
    result = [apiClient.responseDeserializer deserialize:data class:@"NSNumber*" error:&error];
    XCTAssertTrue([result isEqual:@YES]);
    
    data = @"false";
    result = [apiClient.responseDeserializer deserialize:data class:@"NSNumber*" error:&error];
    XCTAssertTrue([result isEqual:@NO]);
}

- (void)testDeserializeStringData {
    NSString *data = @"1233";

    NSError* error;
    NSString * returnValue = [apiClient.responseDeserializer deserialize:[data dataUsingEncoding:NSUTF8StringEncoding] class:@"NSString*" error:&error];
    XCTAssertTrue([returnValue isEqual:data]);

    NSNumber *returnNumber = [apiClient.responseDeserializer deserialize:[data dataUsingEncoding:NSUTF8StringEncoding] class:@"NSNumber*" error:&error];
    XCTAssertTrue([returnNumber isEqual:@1233]);
}


@end
