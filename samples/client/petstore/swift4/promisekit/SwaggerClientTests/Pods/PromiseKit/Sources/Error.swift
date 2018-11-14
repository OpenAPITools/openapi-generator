import Foundation

public enum PMKError: Error {
    /**
     The ErrorType for a rejected `join`.
     - Parameter 0: The promises passed to this `join` that did not *all* fulfill.
     - Note: The array is untyped because Swift generics are fussy with enums.
     */
    case join([AnyObject])

    /**
     The completionHandler with form (T?, ErrorType?) was called with (nil, nil)
     This is invalid as per Cocoa/Apple calling conventions.
     */
    case invalidCallingConvention

    /**
     A handler returned its own promise. 99% of the time, this is likely a 
     programming error. It is also invalid per Promises/A+.
     */
    case returnedSelf

    /** `when()` was called with a concurrency of <= 0 */
    case whenConcurrentlyZero

    /** AnyPromise.toPromise failed to cast as requested */
    case castError(Any.Type)
}

public enum PMKURLError: Error {
    /**
     The URLRequest succeeded but a valid UIImage could not be decoded from
     the data that was received.
     */
    case invalidImageData(URLRequest, Data)

    /**
     The HTTP request returned a non-200 status code.
     */
    case badResponse(URLRequest, Data?, URLResponse?)

    /**
     The data could not be decoded using the encoding specified by the HTTP
     response headers.
     */
    case stringEncoding(URLRequest, Data, URLResponse)

    /**
     Usually the `URLResponse` is actually an `HTTPURLResponse`, if so you
     can access it using this property. Since it is returned as an unwrapped
     optional: be sure.
     */
    public var NSHTTPURLResponse: Foundation.HTTPURLResponse! {
        switch self {
        case .invalidImageData:
            return nil
        case .badResponse(_, _, let rsp):
            return rsp as! Foundation.HTTPURLResponse
        case .stringEncoding(_, _, let rsp):
            return rsp as! Foundation.HTTPURLResponse
        }
    }
}

extension PMKURLError: CustomStringConvertible {
    public var description: String {
        switch self {
        case let .badResponse(rq, data, rsp):
            if let data = data, let str = String(data: data, encoding: .utf8), let rsp = rsp {
                return "PromiseKit: badResponse: \(rq): \(rsp)\n\(str)"
            } else {
                fallthrough
            }
        default:
            return "\(self)"
        }
    }
}

public enum JSONError: Error {
    /// The JSON response was different to that requested
    case unexpectedRootNode(Any)
}


//////////////////////////////////////////////////////////// Cancellation

public protocol CancellableError: Error {
    var isCancelled: Bool { get }
}

#if !SWIFT_PACKAGE

private struct ErrorPair: Hashable {
    let domain: String
    let code: Int
    init(_ d: String, _ c: Int) {
        domain = d; code = c
    }
    var hashValue: Int {
        return "\(domain):\(code)".hashValue
    }
}

private func ==(lhs: ErrorPair, rhs: ErrorPair) -> Bool {
    return lhs.domain == rhs.domain && lhs.code == rhs.code
}

extension NSError {
    @objc public class func cancelledError() -> NSError {
        let info = [NSLocalizedDescriptionKey: "The operation was cancelled"]
        return NSError(domain: PMKErrorDomain, code: PMKOperationCancelled, userInfo: info)
    }

    /**
      - Warning: You must call this method before any promises in your application are rejected. Failure to ensure this may lead to concurrency crashes.
      - Warning: You must call this method on the main thread. Failure to do this may lead to concurrency crashes.
     */
    @objc public class func registerCancelledErrorDomain(_ domain: String, code: Int) {
        cancelledErrorIdentifiers.insert(ErrorPair(domain, code))
    }

    /// - Returns: true if the error represents cancellation.
    @objc public var isCancelled: Bool {
        return (self as Error).isCancelledError
    }
}

private var cancelledErrorIdentifiers = Set([
    ErrorPair(PMKErrorDomain, PMKOperationCancelled),
    ErrorPair(NSCocoaErrorDomain, NSUserCancelledError),
    ErrorPair(NSURLErrorDomain, NSURLErrorCancelled),
])

#endif


extension Error {
    public var isCancelledError: Bool {
        if let ce = self as? CancellableError {
            return ce.isCancelled
        } else {
          #if SWIFT_PACKAGE
            return false
          #else
            let ne = self as NSError
            return cancelledErrorIdentifiers.contains(ErrorPair(ne.domain, ne.code))
          #endif
        }
    }
}


//////////////////////////////////////////////////////// Unhandled Errors
class ErrorConsumptionToken {
    var consumed = false
    let error: Error

    init(_ error: Error) {
        self.error = error
    }

    deinit {
        if !consumed {
#if os(Linux) || os(Android)
            PMKUnhandledErrorHandler(error)
#else
            PMKUnhandledErrorHandler(error as NSError)
#endif
        }
    }
}
