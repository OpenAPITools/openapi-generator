import Foundation
#if !COCOAPODS
import PromiseKit
#endif

/**
 To import the `NSURLSession` category:

    use_frameworks!
    pod "PromiseKit/Foundation"

 Or `NSURLSession` is one of the categories imported by the umbrella pod:

    use_frameworks!
    pod "PromiseKit"

 And then in your sources:

    import PromiseKit
*/
extension URLSession {
    /**
     Makes an HTTP request using the parameters specified by the provided URL
     request.

     We recommend the use of [OMGHTTPURLRQ] which allows you to construct correct REST requests.

         let rq = OMGHTTPURLRQ.POST(url, json: parameters)
         NSURLSession.shared.dataTask(with: rq).asDictionary().then { json in
             //…
         }
     
     [We provide OMG extensions](https://github.com/PromiseKit/OMGHTTPURLRQ)
     that allow eg:
     
         URLSession.shared.POST(url, json: ["a": "b"])

     - Parameter request: The URL request.
     - Returns: A promise that represents the URL request.
     - SeeAlso: `URLDataPromise`
     - SeeAlso: [OMGHTTPURLRQ]
     
     [OMGHTTPURLRQ]: https://github.com/mxcl/OMGHTTPURLRQ
     */
    public func dataTask(with request: URLRequest) -> URLDataPromise {
        return URLDataPromise.go(request) { completionHandler in
            dataTask(with: request, completionHandler: completionHandler).resume()
        }
    }
}
