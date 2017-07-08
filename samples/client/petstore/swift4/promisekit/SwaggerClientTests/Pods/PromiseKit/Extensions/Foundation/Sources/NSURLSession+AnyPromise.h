#import <Foundation/NSDictionary.h>
#import <Foundation/NSURLSession.h>
#import <Foundation/NSURLRequest.h>
#import <PromiseKit/AnyPromise.h>

#define PMKURLErrorFailingURLResponseKey @"PMKURLErrorFailingURLResponseKey"
#define PMKURLErrorFailingDataKey @"PMKURLErrorFailingDataKey"
#define PMKURLErrorFailingStringKey @"PMKURLErrorFailingStringKey"
#define PMKJSONErrorJSONObjectKey @"PMKJSONErrorJSONObjectKey"

/**
 To import the `NSURLSession` category:

    use_frameworks!
    pod "PromiseKit/Foundation"

 Or `NSURLConnection` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"
 
 And then in your sources:

    #import <PromiseKit/PromiseKit.h>
*/
@interface NSURLSession (PromiseKit)

/**
 Creates a task that retrieves the contents of a URL based on the
 specified URL request object.

 PromiseKit automatically deserializes the raw HTTP data response into the
 appropriate rich data type based on the mime type the server provides.
 Thus if the response is JSON you will get the deserialized JSON response.
 PromiseKit supports decoding into strings, JSON and UIImages.

 However if your server does not provide a rich content-type, you will
 just get `NSData`. This is rare, but a good example we came across was
 downloading files from Dropbox.

 PromiseKit goes to quite some lengths to provide good `NSError` objects
 for error conditions at all stages of the HTTP to rich-data type
 pipeline. We provide the following additional `userInfo` keys as
 appropriate:

 - `PMKURLErrorFailingDataKey`
 - `PMKURLErrorFailingStringKey`
 - `PMKURLErrorFailingURLResponseKey`

    [[NSURLConnection sharedSession] promiseDataTaskWithRequest:rq].then(^(id response){
        // response is probably an NSDictionary deserialized from JSON
    });

 @param request The URL request.

 @return A promise that fulfills with three parameters:

   1) The deserialized data response.
   2) The `NSHTTPURLResponse`.
   3) The raw `NSData` response.

 @see https://github.com/mxcl/OMGHTTPURLRQ
*/
- (AnyPromise *)promiseDataTaskWithRequest:(NSURLRequest *)request NS_REFINED_FOR_SWIFT;

@end
