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
    
    NSDate *deserializedDate = [apiClient deserialize:dateStr class:@"NSDate*"];
    
    XCTAssertEqualWithAccuracy([date timeIntervalSinceReferenceDate], [deserializedDate timeIntervalSinceReferenceDate], 0.001);
}

- (void)testDeserializeDateTime {
    NSString *dateTimeStr = @"1997-07-16T19:20:30+00:00";
    
    NSDateFormatter *formatter = [[NSDateFormatter alloc] init];
    [formatter setDateFormat:@"yyyy-MM-dd'T'HH:mm:ssZ"];
    NSDate *dateTime = [formatter dateFromString:dateTimeStr];
    
    NSDate *deserializedDateTime = [apiClient deserialize:dateTimeStr class:@"NSDate*"];
    
    XCTAssertEqualWithAccuracy([dateTime timeIntervalSinceReferenceDate], [deserializedDateTime timeIntervalSinceReferenceDate], 0.001);
}

- (void)testDeserializeObject {
    XCTAssertTrue([[apiClient deserialize:@"" class:@"NSObject*"] isKindOfClass:[NSObject class]]);
}

- (void)testDeserializeString {
    NSString *data = @"test string";
    NSString *result = [apiClient deserialize:data class:@"NSString*"];
    
    XCTAssertTrue([result isEqualToString:data]);
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
    
    NSArray *result = [apiClient deserialize:data class:@"NSArray<SWGPet>*"];
    
    XCTAssertTrue([result isKindOfClass:[NSArray class]]);
    XCTAssertTrue([[result firstObject] isKindOfClass:[SWGPet class]]);
    XCTAssertEqualObjects([[result firstObject] _id], @119);
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
    
    NSDictionary *result = [apiClient deserialize:data class:@"NSDictionary* /* NSString, SWGPet */"];
    
    XCTAssertTrue([result isKindOfClass:[NSDictionary class]]);
    XCTAssertTrue([result[@"pet"] isKindOfClass:[SWGPet class]]);
    XCTAssertEqualObjects([result[@"pet"] _id], @119);
}

@end
