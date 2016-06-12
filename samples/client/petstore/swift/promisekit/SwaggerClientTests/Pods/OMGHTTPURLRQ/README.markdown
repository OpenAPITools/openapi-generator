# OMGHTTPURLRQ

Vital extensions to `NSURLRequest` that Apple left out for some reason.

```objc
NSMutableURLRequest *rq = [OMGHTTPURLRQ GET:@"http://api.com":@{@"key": @"value"}];

// application/x-www-form-urlencoded
NSMutableURLRequest *rq = [OMGHTTPURLRQ POST:@"http://api.com":@{@"key": @"value"}];

// application/json
NSMutableURLRequest *rq = [OMGHTTPURLRQ POST:@"http://api.com" JSON:@{@"key": @"value"}];

// PUT
NSMutableURLRequest *rq = [OMGHTTPURLRQ PUT:@"http://api.com":@{@"key": @"value"}];

// DELETE
NSMutableURLRequest *rq = [OMGHTTPURLRQ DELETE:@"http://api.com":@{@"key": @"value"}];
```

You can then pass these to an `NSURLConnection` or `NSURLSession`.


## `multipart/form-data`

OMG! Constructing multipart/form-data for POST requests is complicated, let us do it for you:

```objc

OMGMultipartFormData *multipartFormData = [OMGMultipartFormData new];

NSData *data1 = [NSData dataWithContentsOfFile:@"myimage1.png"];
[multipartFormData addFile:data1 parameterName:@"file1" filename:@"myimage1.png" contentType:@"image/png"];

// Ideally you would not want to re-encode the PNG, but often it is
// tricky to avoid it.
UIImage *image2 = [UIImage imageNamed:@"image2"];
NSData *data2 = UIImagePNGRepresentation(image2);
[multipartFormData addFile:data2 parameterName:@"file2" filename:@"myimage2.png" contentType:@"image/png"];

// SUPER Ideally you would not want to re-encode the JPEG as the process
// is lossy. If your image comes from the AssetLibrary you *CAN* get the
// original `NSData`. See stackoverflow.com.
UIImage *image3 = [UIImage imageNamed:@"image3"];
NSData *data3 = UIImageJPEGRepresentation(image3);
[multipartFormData addFile:data3 parameterName:@"file2" filename:@"myimage3.jpeg" contentType:@"image/jpeg"];

NSMutableURLRequest *rq = [OMGHTTPURLRQ POST:url:multipartFormData];
```

Now feed `rq` to `[NSURLConnection sendSynchronousRequest:returningResponse:error:`.


## Configuring an `NSURLSessionUploadTask`

If you need to use `NSURLSession`’s `uploadTask:` but you have become frustrated  because your endpoint expects a multipart/form-data POST request and `NSURLSession` sends the data *raw*, use this:

```objc
id config = [NSURLSessionConfiguration backgroundSessionConfigurationWithIdentifier:someID];
id session = [NSURLSession sessionWithConfiguration:config delegate:someObject delegateQueue:[NSOperationQueue new]];

OMGMultipartFormData *multipartFormData = [OMGMultipartFormData new];
[multipartFormData addFile:data parameterName:@"file" filename:nil contentType:nil];

NSURLRequest *rq = [OMGHTTPURLRQ POST:urlString:multipartFormData];

id path = [[NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES) lastObject] stringByAppendingPathComponent:@"upload.NSData"];
[rq.HTTPBody writeToFile:path atomically:YES];

[[session uploadTaskWithRequest:rq fromFile:[NSURL fileURLWithPath:path]] resume];
```


## OMGUserAgent

If you just need a sensible UserAgent string for your application you can `pod OMGHTTPURLRQ/UserAgent` and then:

```objc
#import <OMGHTTPURLRQ/OMGUserAgent.h>

NSString *userAgent = OMGUserAgent();
```

OMGHTTPURLRQ adds this User-Agent to all requests it generates automatically.

So for URLRequests generated **other** than by OMGHTTPURLRQ you would do:

```objc
[someURLRequest addValue:OMGUserAgent() forHTTPHeaderField:@"User-Agent"];
```


# Twitter Reverse Auth

You need an OAuth library, here we use the [TDOAuth](https://github.com/tweetdeck/TDOAuth) pod. You also need
your API keys that registering at https://dev.twitter.com will provide
you.

```objc
NSMutableURLRequest *rq = [TDOAuth URLRequestForPath:@"/oauth/request_token" POSTParameters:@{@"x_auth_mode" : @"reverse_auth"} host:@"api.twitter.com" consumerKey:APIKey consumerSecret:APISecret accessToken:nil tokenSecret:nil];
[rq addValue:OMGUserAgent() forHTTPHeaderField:@"User-Agent"];

[NSURLConnection sendAsynchronousRequest:rq queue:nil completionHandler:^(NSURLResponse *response, NSData *data, NSError *connectionError) {
    id oauth = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
    SLRequest *reverseAuth = [SLRequest requestForServiceType:SLServiceTypeTwitter requestMethod:SLRequestMethodPOST URL:[NSURL URLWithString:@"https://api.twitter.com/oauth/access_token"] parameters:@{
        @"x_reverse_auth_target": APIKey,
        @"x_reverse_auth_parameters": oauth
    }];
    reverseAuth.account = account;
    [reverseAuth performRequestWithHandler:^(NSData *data, NSHTTPURLResponse *urlResponse, NSError *error) {
        id creds = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        id credsDict = [NSMutableDictionary new];
        for (__strong id pair in [creds componentsSeparatedByString:@"&"]) {
            pair = [pair componentsSeparatedByString:@"="];
            credsDict[pair[0]] = pair[1];
        }
        NSLog(@"%@", credsDict);
    }];
}];
```


# License

```
Copyright 2014 Max Howell <mxcl@me.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
```
