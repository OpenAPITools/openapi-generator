//
//  RequestCancellationTests.swift
//  SwaggerClient
//

import Foundation
import PetstoreClient
import XCTest

/// Runs without a server: requests go to a fake session, and a request counts as sent once its task is resumed.
final class RequestCancellationTests: XCTestCase, @unchecked Sendable {

    func testCancelDuringAsyncInterceptDoesNotSendRequestAndThrowsCancellation() async throws {
        let session = FakeSession(statusCode: 200)
        let interceptor = GatedInterceptor()
        let builder = makeBuilder(session: session, interceptor: interceptor)

        let intercepting = expectation(description: "interceptor invoked")
        interceptor.onIntercept = { intercepting.fulfill() }

        let call = Task { try await builder.execute() }
        await fulfillment(of: [intercepting], timeout: 5)

        call.cancel()
        interceptor.releaseIntercept()

        do {
            _ = try await call.value
            XCTFail("expected a cancellation error")
        } catch let ErrorResponse.error(_, _, _, underlying) {
            XCTAssertEqual((underlying as? URLError)?.code, .cancelled)
        }
        XCTAssertEqual(session.resumedTaskCount, 0)
        XCTAssertEqual(interceptor.retryCallCount, 0)
    }

    func testCancelBeforeRetryDoesNotRetry() async throws {
        let session = FakeSession(statusCode: 500)
        let interceptor = GatedInterceptor()
        let builder = makeBuilder(session: session, interceptor: interceptor)

        interceptor.releaseIntercept()
        interceptor.holdsRetry = true
        let retrying = expectation(description: "retry requested")
        interceptor.onRetry = { retrying.fulfill() }

        let call = Task { try await builder.execute() }
        await fulfillment(of: [retrying], timeout: 5)
        XCTAssertEqual(session.resumedTaskCount, 1)

        call.cancel()
        interceptor.releaseRetry(.retry)

        do {
            _ = try await call.value
            XCTFail("expected the request to fail")
        } catch {}
        XCTAssertEqual(session.resumedTaskCount, 1)
    }

    func testRetryWithoutCancelRetries() async throws {
        let session = FakeSession(statusCode: 500)
        let interceptor = GatedInterceptor()
        interceptor.releaseIntercept()
        interceptor.retryAnswers = [.retry]
        let builder = makeBuilder(session: session, interceptor: interceptor)

        do {
            _ = try await builder.execute()
            XCTFail("expected the request to fail")
        } catch {}
        XCTAssertEqual(session.resumedTaskCount, 2)
        XCTAssertEqual(interceptor.retryCallCount, 2)
    }

    func testExecuteAfterCancelSendsRequest() async throws {
        let session = FakeSession(statusCode: 200)
        let interceptor = GatedInterceptor()
        interceptor.releaseIntercept()
        let builder = makeBuilder(session: session, interceptor: interceptor)

        builder.requestTask.cancel()
        _ = try await builder.execute()
        XCTAssertEqual(session.resumedTaskCount, 1)

        builder.requestTask.cancel()
        _ = try await builder.execute()
        XCTAssertEqual(session.resumedTaskCount, 2)
    }

    private func makeBuilder(session: FakeSession, interceptor: GatedInterceptor) -> FakeSessionRequestBuilder<Void> {
        let configuration = PetstoreClientAPIConfiguration(apiResponseQueue: .global(), interceptor: interceptor)
        let builder = FakeSessionRequestBuilder<Void>(
            method: "GET",
            URLString: "http://localhost/cancellation",
            parameters: nil,
            requiresAuthentication: false,
            apiConfiguration: configuration
        )
        builder.session = session
        return builder
    }
}

private final class FakeSessionRequestBuilder<T: Sendable>: URLSessionRequestBuilder<T>, @unchecked Sendable {
    var session: URLSessionProtocol = FakeSession(statusCode: 200)

    override func createURLSession() -> URLSessionProtocol { session }
}

private final class FakeSession: URLSessionProtocol, @unchecked Sendable {
    private let lock = NSLock()
    private var _resumedTaskCount = 0
    private var nextTaskIdentifier = 0
    private let statusCode: Int

    init(statusCode: Int) {
        self.statusCode = statusCode
    }

    var resumedTaskCount: Int { lock.withLock { _resumedTaskCount } }

    func dataTaskFromProtocol(with request: URLRequest, completionHandler: @escaping @Sendable (Data?, URLResponse?, (any Error)?) -> Void) -> URLSessionDataTaskProtocol {
        let response = HTTPURLResponse(url: request.url!, statusCode: statusCode, httpVersion: nil, headerFields: nil)
        let identifier = lock.withLock { () -> Int in
            nextTaskIdentifier += 1
            return nextTaskIdentifier
        }
        return FakeDataTask(
            taskIdentifier: identifier,
            onResume: { [self] in lock.withLock { _resumedTaskCount += 1 } },
            completionHandler: completionHandler,
            response: response
        )
    }
}

/// Completes like URLSessionDataTask: with the response after `resume()`, with `URLError.cancelled` after `cancel()`.
private final class FakeDataTask: URLSessionDataTaskProtocol, @unchecked Sendable {
    private let lock = NSLock()
    private var finished = false
    private let onResume: @Sendable () -> Void
    private let completionHandler: @Sendable (Data?, URLResponse?, (any Error)?) -> Void
    private let response: URLResponse?

    let taskIdentifier: Int
    let progress = Progress()

    init(taskIdentifier: Int, onResume: @escaping @Sendable () -> Void, completionHandler: @escaping @Sendable (Data?, URLResponse?, (any Error)?) -> Void, response: URLResponse?) {
        self.taskIdentifier = taskIdentifier
        self.onResume = onResume
        self.completionHandler = completionHandler
        self.response = response
    }

    func resume() {
        guard finish() else { return }
        onResume()
        DispatchQueue.global().async { self.completionHandler(Data(), self.response, nil) }
    }

    func cancel() {
        guard finish() else { return }
        DispatchQueue.global().async { self.completionHandler(nil, nil, URLError(.cancelled)) }
    }

    private func finish() -> Bool {
        lock.withLock {
            defer { finished = true }
            return !finished
        }
    }
}

/// Completes `intercept` and `retry` only when the test says so, to hold a request in the asynchronous window.
private final class GatedInterceptor: OpenAPIInterceptor, @unchecked Sendable {
    private let lock = NSLock()
    private var pendingIntercept: (@Sendable (Result<URLRequest, Error>) -> Void)?
    private var pendingRetry: (@Sendable (OpenAPIInterceptorRetry) -> Void)?
    private var interceptReleased = false
    private var request: URLRequest?
    var onIntercept: (@Sendable () -> Void)?
    private var _retryCallCount = 0
    var onRetry: (@Sendable () -> Void)?
    /// Holds the next `retry` call only; later ones are answered from `retryAnswers`.
    var holdsRetry = false
    /// Answers for `retry` calls in order; `.dontRetry` once exhausted.
    var retryAnswers: [OpenAPIInterceptorRetry] = []

    var retryCallCount: Int { lock.withLock { _retryCallCount } }

    func releaseIntercept() {
        let action: (() -> Void)? = lock.withLock {
            interceptReleased = true
            guard let pending = pendingIntercept, let request else { return nil }
            pendingIntercept = nil
            return { Task { pending(.success(request)) } }
        }
        action?()
    }

    func releaseRetry(_ decision: OpenAPIInterceptorRetry) {
        let pending = lock.withLock { () -> (@Sendable (OpenAPIInterceptorRetry) -> Void)? in
            defer { pendingRetry = nil }
            return pendingRetry
        }
        pending?(decision)
    }

    func intercept<T>(urlRequest: URLRequest, urlSession: URLSessionProtocol, requestBuilder: RequestBuilder<T>, completion: @Sendable @escaping (Result<URLRequest, Error>) -> Void) {
        let releaseNow = lock.withLock { () -> Bool in
            request = urlRequest
            if !interceptReleased { pendingIntercept = completion }
            return interceptReleased
        }
        onIntercept?()
        if releaseNow {
            completion(.success(urlRequest))
        }
    }

    func retry<T>(urlRequest: URLRequest, urlSession: URLSessionProtocol, requestBuilder: RequestBuilder<T>, data: Data?, response: URLResponse?, error: Error, completion: @Sendable @escaping (OpenAPIInterceptorRetry) -> Void) {
        let answer = lock.withLock { () -> OpenAPIInterceptorRetry? in
            _retryCallCount += 1
            defer { holdsRetry = false }
            if holdsRetry {
                pendingRetry = completion
                return nil
            }
            return retryAnswers.isEmpty ? .dontRetry : retryAnswers.removeFirst()
        }
        if let answer {
            completion(answer)
        } else {
            onRetry?()
        }
    }
}
