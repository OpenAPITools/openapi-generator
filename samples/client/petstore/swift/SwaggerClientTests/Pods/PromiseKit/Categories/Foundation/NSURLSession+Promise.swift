import Foundation
import OMGHTTPURLRQ
#if !COCOAPODS
import PromiseKit
#endif

//TODO cancellation

/**
 To import the `NSURLConnection` category:

    use_frameworks!
    pod "PromiseKit/Foundation"

 Or `NSURLConnection` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    import PromiseKit

 We provide convenience categories for the `sharedSession`, or 
 an instance method `promise`. If you need more complicated behavior
 we recommend wrapping that usage in a Promise initializer.
*/
extension NSURLSession {
    public class func GET(URL: String, query: [NSObject: AnyObject]? = nil) -> URLDataPromise {
        return start(try OMGHTTPURLRQ.GET(URL, query))
    }

    public class func POST(URL: String, formData: [NSObject: AnyObject]? = nil) -> URLDataPromise {
        return start(try OMGHTTPURLRQ.POST(URL, formData))
    }

    public class func POST(URL: String, multipartFormData: OMGMultipartFormData) -> URLDataPromise {
        return start(try OMGHTTPURLRQ.POST(URL, multipartFormData))
    }

    public class func PUT(URL: String) -> URLDataPromise {
        return start(try OMGHTTPURLRQ.PUT(URL, nil))
    }

    public class func DELETE(URL: String) -> URLDataPromise {
        return start(try OMGHTTPURLRQ.DELETE(URL, nil))
    }

    public func promise(request: NSURLRequest) -> URLDataPromise {
        return start(request, session: self)
    }
}

private func start(@autoclosure body: () throws -> NSURLRequest, session: NSURLSession = NSURLSession.sharedSession()) -> URLDataPromise {
    do {
        var request = try body()

        if request.valueForHTTPHeaderField("User-Agent") == nil {
            let rq = request.mutableCopy() as! NSMutableURLRequest
            rq.setValue(OMGUserAgent(), forHTTPHeaderField: "User-Agent")
            request = rq
        }

        return URLDataPromise.go(request) { completionHandler in
            let task = session.dataTaskWithRequest(request, completionHandler: completionHandler)
            task.resume()
        }
    } catch {
        return URLDataPromise(error: error)
    }
}
