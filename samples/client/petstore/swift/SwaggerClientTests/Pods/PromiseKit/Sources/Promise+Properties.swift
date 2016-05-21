extension Promise {
    /**
     - Returns: The error with which this promise was rejected; `nil` if this promise is not rejected.
    */
    public var error: ErrorType? {
        switch state.get() {
        case .None:
            return nil
        case .Some(.Fulfilled):
            return nil
        case .Some(.Rejected(let error, _)):
            return error
        }
    }

    /**
     Provides an alias for the `error` property for cases where the Swift
     compiler cannot disambiguate from our `error` function.
     
     More than likely use of this alias will never be necessary as it's
     the inverse situation where Swift usually becomes confused. But 
     we provide this anyway just in case.
     
     If you absolutely cannot get Swift to accept `error` then 
     `errorValue` may be used instead as it returns the same thing.

     - Warning: This alias will be unavailable in PromiseKit 4.0.0
     - SeeAlso: [https://github.com/mxcl/PromiseKit/issues/347](https://github.com/mxcl/PromiseKit/issues/347)
    */
    @available(*, deprecated, renamed="error", message="Temporary alias `errorValue` will eventually be removed and should only be used when the Swift compiler cannot be satisfied with `error`")
    public var errorValue: ErrorType? {
        return self.error
    }

    /**
     - Returns: `true` if the promise has not yet resolved.
    */
    public var pending: Bool {
        return state.get() == nil
    }

    /**
     - Returns: `true` if the promise has resolved.
    */
    public var resolved: Bool {
        return !pending
    }

    /**
     - Returns: `true` if the promise was fulfilled.
    */
    public var fulfilled: Bool {
        return value != nil
    }

    /**
     - Returns: `true` if the promise was rejected.
    */
    public var rejected: Bool {
        return error != nil
    }

    /**
     - Returns: The value with which this promise was fulfilled or `nil` if this promise is pending or rejected.
    */
    public var value: T? {
        switch state.get() {
        case .None:
            return nil
        case .Some(.Fulfilled(let value)):
            return value
        case .Some(.Rejected):
            return nil
        }
    }
}
