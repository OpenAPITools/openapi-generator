import Foundation
#if !COCOAPODS
import PromiseKit
#endif

public enum Encoding {
    /// Decode as JSON
    case json(JSONSerialization.ReadingOptions)
}

/**
 A promise capable of decoding common Internet data types.
 
 Used by:
 
  - PromiseKit/Foundation
  - PromiseKit/Social
  - PromiseKit/OMGHTTPURLRQ
 
 But probably of general use to any promises that receive HTTP `Data`.
 */
public class URLDataPromise: Promise<Data> {
    /// Convert the promise to a tuple of `(Data, URLResponse)`
    public func asDataAndResponse() -> Promise<(Data, Foundation.URLResponse)> {
        return then(on: zalgo) { ($0, self.URLResponse) }
    }

    /// Decode the HTTP response to a String, the string encoding is read from the response.
    public func asString() -> Promise<String> {
        return then(on: waldo) { data -> String in
            guard let str = String(bytes: data, encoding: self.URLResponse.stringEncoding ?? .utf8) else {
                throw PMKURLError.stringEncoding(self.URLRequest, data, self.URLResponse)
            }
            return str
        }
    }

    /// Decode the HTTP response as a JSON array
    public func asArray(_ encoding: Encoding = .json(.allowFragments)) -> Promise<NSArray> {
        return then(on: waldo) { data -> NSArray in
            switch encoding {
            case .json(let options):
                guard !data.b0rkedEmptyRailsResponse else { return NSArray() }
                let json = try JSONSerialization.jsonObject(with: data, options: options)
                guard let array = json as? NSArray else { throw JSONError.unexpectedRootNode(json) }
                return array
            }
        }
    }

    /// Decode the HTTP response as a JSON dictionary
    public func asDictionary(_ encoding: Encoding = .json(.allowFragments)) -> Promise<NSDictionary> {
        return then(on: waldo) { data -> NSDictionary in
            switch encoding {
            case .json(let options):
                guard !data.b0rkedEmptyRailsResponse else { return NSDictionary() }
                let json = try JSONSerialization.jsonObject(with: data, options: options)
                guard let dict = json as? NSDictionary else { throw JSONError.unexpectedRootNode(json) }
                return dict
            }
        }
    }

    fileprivate var URLRequest: Foundation.URLRequest!
    fileprivate var URLResponse: Foundation.URLResponse!

    /// Internal
    public class func go(_ request: URLRequest, body: (@escaping (Data?, URLResponse?, Error?) -> Void) -> Void) -> URLDataPromise {
        let (p, fulfill, reject) = URLDataPromise.pending()
        let promise  = p as! URLDataPromise

        body { data, rsp, error in
            promise.URLRequest = request
            promise.URLResponse = rsp

            if let error = error {
                reject(error)
            } else if let data = data, let rsp = rsp as? HTTPURLResponse, (200..<300) ~= rsp.statusCode {
                fulfill(data)
            } else if let data = data, !(rsp is HTTPURLResponse) {
                fulfill(data)
            } else {
                reject(PMKURLError.badResponse(request, data, rsp))
            }
        }
        
        return promise
    }
}

#if os(iOS)
    import UIKit.UIImage

    extension URLDataPromise {
        /// Decode the HTTP response as a UIImage
        public func asImage() -> Promise<UIImage> {
            return then(on: waldo) { data -> UIImage in
                guard let img = UIImage(data: data), let cgimg = img.cgImage else {
                    throw PMKURLError.invalidImageData(self.URLRequest, data)
                }
                // this way of decoding the image limits main thread impact when displaying the image
                return UIImage(cgImage: cgimg, scale: img.scale, orientation: img.imageOrientation)
            }
        }
    }
#endif

extension URLResponse {
    fileprivate var stringEncoding: String.Encoding? {
        guard let encodingName = textEncodingName else { return nil }
        let encoding = CFStringConvertIANACharSetNameToEncoding(encodingName as CFString)
        guard encoding != kCFStringEncodingInvalidId else { return nil }
        return String.Encoding(rawValue: CFStringConvertEncodingToNSStringEncoding(encoding))
    }
}

extension Data {
    fileprivate var b0rkedEmptyRailsResponse: Bool {
        return count == 1 && withUnsafeBytes{ $0[0] == " " }
    }
}
