import Foundation

public enum Encoding {
    case JSON(NSJSONReadingOptions)
}

public class URLDataPromise: Promise<NSData> {
    public func asDataAndResponse() -> Promise<(NSData, NSURLResponse)> {
        return then(on: zalgo) { ($0, self.URLResponse) }
    }

    public func asString() -> Promise<String> {
        return then(on: waldo) { data -> String in
            guard let str = NSString(data: data, encoding: self.URLResponse.stringEncoding ?? NSUTF8StringEncoding) else {
                throw URLError.StringEncoding(self.URLRequest, data, self.URLResponse)
            }
            return str as String
        }
    }

    public func asArray(encoding: Encoding = .JSON(.AllowFragments)) -> Promise<NSArray> {
        return then(on: waldo) { data -> NSArray in
            switch encoding {
            case .JSON(let options):
                guard !data.b0rkedEmptyRailsResponse else { return NSArray() }
                let json = try NSJSONSerialization.JSONObjectWithData(data, options: options)
                guard let array = json as? NSArray else { throw JSONError.UnexpectedRootNode(json) }
                return array
            }
        }
    }

    public func asDictionary(encoding: Encoding = .JSON(.AllowFragments)) -> Promise<NSDictionary> {
        return then(on: waldo) { data -> NSDictionary in
            switch encoding {
            case .JSON(let options):
                guard !data.b0rkedEmptyRailsResponse else { return NSDictionary() }
                let json = try NSJSONSerialization.JSONObjectWithData(data, options: options)
                guard let dict = json as? NSDictionary else { throw JSONError.UnexpectedRootNode(json) }
                return dict
            }
        }
    }

    private override init(@noescape resolvers: (fulfill: (NSData) -> Void, reject: (ErrorType) -> Void) throws -> Void) {
        super.init(resolvers: resolvers)
    }

    public override init(error: ErrorType) {
        super.init(error: error)
    }

    private var URLRequest: NSURLRequest!
    private var URLResponse: NSURLResponse!

    public class func go(request: NSURLRequest, @noescape body: ((NSData?, NSURLResponse?, NSError?) -> Void) -> Void) -> URLDataPromise {
        var promise: URLDataPromise!
        promise = URLDataPromise { fulfill, reject in
            body { data, rsp, error in
                promise.URLRequest = request
                promise.URLResponse = rsp

                if let error = error {
                    reject(URLError.UnderlyingCocoaError(request, data, rsp, error))
                } else if let data = data, rsp = rsp as? NSHTTPURLResponse where rsp.statusCode >= 200 && rsp.statusCode < 300 {
                    fulfill(data)
                } else if let data = data where !(rsp is NSHTTPURLResponse) {
                    fulfill(data)
                } else {
                    reject(URLError.BadResponse(request, data, rsp))
                }
            }
        }
        return promise
    }
}

#if os(iOS)
    import UIKit.UIImage

    extension URLDataPromise {
        public func asImage() -> Promise<UIImage> {
            return then(on: waldo) { data -> UIImage in
                guard let img = UIImage(data: data), cgimg = img.CGImage else {
                    throw URLError.InvalidImageData(self.URLRequest, data)
                }
                return UIImage(CGImage: cgimg, scale: img.scale, orientation: img.imageOrientation)
            }
        }
    }
#endif

extension NSURLResponse {
    private var stringEncoding: UInt? {
        guard let encodingName = textEncodingName else { return nil }
        let encoding = CFStringConvertIANACharSetNameToEncoding(encodingName)
        guard encoding != kCFStringEncodingInvalidId else { return nil }
        return CFStringConvertEncodingToNSStringEncoding(encoding)
    }
}

extension NSData {
    private var b0rkedEmptyRailsResponse: Bool {
        return self == NSData(bytes: " ", length: 1)
    }
}
