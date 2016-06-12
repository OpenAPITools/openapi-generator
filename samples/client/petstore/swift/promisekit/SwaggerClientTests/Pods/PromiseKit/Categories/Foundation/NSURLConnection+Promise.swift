import Foundation
import OMGHTTPURLRQ
#if !COCOAPODS
import PromiseKit
#endif

/**
 To import the `NSURLConnection` category:

    use_frameworks!
    pod "PromiseKit/Foundation"

 Or `NSURLConnection` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    import PromiseKit
*/
extension NSURLConnection {
    public class func GET(URL: String, query: [NSObject:AnyObject]? = nil) -> URLDataPromise {
        return go(try OMGHTTPURLRQ.GET(URL, query))
    }

    public class func POST(URL: String, formData: [NSObject:AnyObject]? = nil) -> URLDataPromise {
        return go(try OMGHTTPURLRQ.POST(URL, formData))
    }

    public class func POST(URL: String, JSON: [NSObject:AnyObject]) -> URLDataPromise {
        return go(try OMGHTTPURLRQ.POST(URL, JSON: JSON))
    }

    public class func POST(URL: String, multipartFormData: OMGMultipartFormData) -> URLDataPromise {
        return go(try OMGHTTPURLRQ.POST(URL, multipartFormData))
    }

    public class func PUT(URL: String, formData: [NSObject:AnyObject]? = nil) -> URLDataPromise {
        return go(try OMGHTTPURLRQ.PUT(URL, formData))
    }

    public class func PUT(URL: String, JSON: [NSObject:AnyObject]) -> URLDataPromise {
        return go(try OMGHTTPURLRQ.PUT(URL, JSON: JSON))
    }

    public class func DELETE(URL: String) -> URLDataPromise {
        return go(try OMGHTTPURLRQ.DELETE(URL, nil))
    }

    public class func promise(request: NSURLRequest) -> URLDataPromise {
        return go(request)
    }
}

private func go(@autoclosure body: () throws -> NSURLRequest) -> URLDataPromise {
    do {
        var request = try body()

        if request.valueForHTTPHeaderField("User-Agent") == nil {
            let rq = request.mutableCopy() as! NSMutableURLRequest
            rq.setValue(OMGUserAgent(), forHTTPHeaderField: "User-Agent")
            request = rq
        }

        return URLDataPromise.go(request) { completionHandler in
            NSURLConnection.sendAsynchronousRequest(request, queue: Q, completionHandler: { completionHandler($1, $0, $2) })
        }
    } catch {
        return URLDataPromise(error: error)
    }
}

private let Q = NSOperationQueue()
