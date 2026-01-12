use wiremock::{Mock, MockServer, ResponseTemplate};
use wiremock::matchers::{method, path, query_param};
use petstore_reqwest_async::apis::{configuration::Configuration, testing_api};
use petstore_reqwest_async::models::ModelWithInlineEnum;

/// Test inline enum in query parameter (from /tests/inlineEnumBoxing GET)
/// This test actually calls the generated SDK and verifies the HTTP request
/// uses the correct query parameter format (no %22).
///
/// Regression test for: https://github.com/OpenAPITools/openapi-generator/issues/XXXXX
#[tokio::test]
async fn test_inline_enum_query_param_draft() {
    let mock_server = MockServer::start().await;

    // wiremock matcher will only succeed if query param is exactly "draft" (not "%22draft%22")
    Mock::given(method("GET"))
        .and(path("/tests/inlineEnumBoxing"))
        .and(query_param("status", "draft"))
        .respond_with(ResponseTemplate::new(200).set_body_json(Vec::<ModelWithInlineEnum>::new()))
        .expect(1)
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    // ACTUALLY CALL THE GENERATED SDK FUNCTION
    let result = testing_api::tests_inline_enum_boxing_get(&config, testing_api::TestsInlineEnumBoxingGetParams {
        status: Some("draft".to_string())
    }).await;

    assert!(result.is_ok(), "API call should succeed");
}

/// Test all inline enum values to ensure none get double-quoted
#[tokio::test]
async fn test_all_inline_enum_values() {
    let mock_server = MockServer::start().await;

    // Catch-all mock to intercept all requests
    Mock::given(method("GET"))
        .and(path("/tests/inlineEnumBoxing"))
        .respond_with(ResponseTemplate::new(200).set_body_json(Vec::<ModelWithInlineEnum>::new()))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    // Call with each enum value
    for status in ["draft", "published", "archived"] {
        let _ = testing_api::tests_inline_enum_boxing_get(&config, testing_api::TestsInlineEnumBoxingGetParams {
            status: Some(status.to_string())
        }).await;
    }

    // Inspect all intercepted requests
    let requests = mock_server.received_requests().await.unwrap();
    assert_eq!(requests.len(), 3, "Should have received 3 requests");

    for (i, request) in requests.iter().enumerate() {
        let url = request.url.as_str();

        // Main assertion: no %22 in any URL
        assert!(
            !url.contains("%22"),
            "Request {} has %22 in URL: {}",
            i, url
        );

        // Also verify no escaped quotes
        assert!(
            !url.contains("\\\""),
            "Request {} has escaped quotes in URL: {}",
            i, url
        );
    }
}

/// Test with None (no query param)
#[tokio::test]
async fn test_optional_enum_none() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/tests/inlineEnumBoxing"))
        .respond_with(ResponseTemplate::new(200).set_body_json(Vec::<ModelWithInlineEnum>::new()))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    let result = testing_api::tests_inline_enum_boxing_get(&config, testing_api::TestsInlineEnumBoxingGetParams {
        status: None
    }).await;

    assert!(result.is_ok(), "API call with None should succeed");

    // Verify no query param was added
    let requests = mock_server.received_requests().await.unwrap();
    let url = requests[0].url.as_str();
    assert!(!url.contains("status="), "URL should not contain status param: {}", url);
}

/// Comprehensive regression test - verify exact URL format
#[tokio::test]
async fn test_regression_exact_url_inspection() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/tests/inlineEnumBoxing"))
        .respond_with(ResponseTemplate::new(200).set_body_json(Vec::<ModelWithInlineEnum>::new()))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    // Call with specific enum value
    let _ = testing_api::tests_inline_enum_boxing_get(&config, testing_api::TestsInlineEnumBoxingGetParams {
        status: Some("published".to_string())
    }).await;

    // Inspect the exact URL that was sent
    let requests = mock_server.received_requests().await.unwrap();
    assert_eq!(requests.len(), 1);

    let url = requests[0].url.as_str();

    // Verify exact format
    assert!(
        url.contains("status=published"),
        "URL should contain 'status=published', got: {}",
        url
    );

    // Verify NO double-encoding
    assert!(
        !url.contains("%22"),
        "REGRESSION: URL contains %22 (double-encoded quotes): {}",
        url
    );
}
