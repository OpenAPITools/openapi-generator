use wiremock::{Mock, MockServer, ResponseTemplate};
use wiremock::matchers::{method, path, query_param};
use enum_query_params_reqwest::apis::{configuration::Configuration, default_api};
use enum_query_params_reqwest::models::{Status, TimeBucket, SortDirection, Priority, AggregateResponse, Item};

/// Test single required enum parameter by actually calling the generated SDK
/// and inspecting the HTTP request that was made.
///
/// Regression test for: https://github.com/OpenAPITools/openapi-generator/issues/XXXXX
#[tokio::test]
async fn test_required_enum_param() {
    let mock_server = MockServer::start().await;

    // wiremock will only match if query param is exactly "active" (not "%22active%22")
    Mock::given(method("GET"))
        .and(path("/aggregate"))
        .and(query_param("status", "active"))
        .respond_with(ResponseTemplate::new(200).set_body_json(
            AggregateResponse { count: Some(10), data: None }
        ))
        .expect(1)
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    // ACTUALLY CALL THE GENERATED SDK FUNCTION
    let result = default_api::get_aggregate_data(&config, Status::Active, None, None).await;
    assert!(result.is_ok(), "API call should succeed");
}

/// Test multiple optional enum parameters
#[tokio::test]
async fn test_multiple_optional_enum_params() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/aggregate"))
        .and(query_param("status", "pending"))
        .and(query_param("timeBucket", "week"))
        .and(query_param("sortDirection", "asc"))
        .respond_with(ResponseTemplate::new(200).set_body_json(
            AggregateResponse { count: Some(5), data: None }
        ))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    let result = default_api::get_aggregate_data(
        &config,
        Status::Pending,
        Some(TimeBucket::Week),
        Some(SortDirection::Asc)
    ).await;

    assert!(result.is_ok(), "API call with multiple enums should succeed");
}

/// Test with optional params as None
#[tokio::test]
async fn test_optional_enum_params_none() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/aggregate"))
        .and(query_param("status", "completed"))
        .respond_with(ResponseTemplate::new(200).set_body_json(
            AggregateResponse { count: Some(0), data: None }
        ))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    let result = default_api::get_aggregate_data(&config, Status::Completed, None, None).await;
    assert!(result.is_ok(), "API call with None optional params should succeed");
}

/// Test inline string enum (category parameter)
#[tokio::test]
async fn test_inline_string_enum() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/items"))
        .and(query_param("category", "electronics"))
        .respond_with(ResponseTemplate::new(200).set_body_json(Vec::<Item>::new()))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    let result = default_api::get_items(&config, Some("electronics"), None).await;
    assert!(result.is_ok(), "API call with inline enum should succeed");
}

/// Test enum ref parameter (Priority)
#[tokio::test]
async fn test_enum_ref_param() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/items"))
        .and(query_param("priority", "critical"))
        .respond_with(ResponseTemplate::new(200).set_body_json(Vec::<Item>::new()))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    let result = default_api::get_items(&config, None, Some(Priority::Critical)).await;
    assert!(result.is_ok(), "API call with enum ref should succeed");
}

/// Comprehensive regression test - inspect exact URL format
/// This is the KEY test that would catch the bug if it returned
#[tokio::test]
async fn test_regression_no_percent_22() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .respond_with(ResponseTemplate::new(200).set_body_json(
            AggregateResponse { count: Some(0), data: None }
        ))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    // Test all enum types by calling the actual generated SDK function
    let _ = default_api::get_aggregate_data(
        &config,
        Status::Inactive,
        Some(TimeBucket::Month),
        Some(SortDirection::Desc)
    ).await;

    // Inspect the intercepted HTTP request
    let requests = mock_server.received_requests().await.unwrap();
    let url = requests[0].url.as_str();

    // Main assertion: NO %22 anywhere in URL
    assert!(
        !url.contains("%22"),
        "REGRESSION: URL contains %22 (double-encoded quotes): {}",
        url
    );

    // Verify correct enum values are present
    assert!(url.contains("status=inactive"), "URL should contain status=inactive: {}", url);
    assert!(url.contains("timeBucket=month"), "URL should contain timeBucket=month: {}", url);
    assert!(url.contains("sortDirection=desc"), "URL should contain sortDirection=desc: {}", url);
}

/// Test all enum variants systematically
#[tokio::test]
async fn test_all_enum_variants() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .respond_with(ResponseTemplate::new(200).set_body_json(
            AggregateResponse { count: Some(0), data: None }
        ))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    // Test each Status variant by calling the generated SDK
    for status in [Status::Active, Status::Inactive, Status::Pending, Status::Completed] {
        let _ = default_api::get_aggregate_data(&config, status, None, None).await;
    }

    let requests = mock_server.received_requests().await.unwrap();
    assert_eq!(requests.len(), 4, "Should have received 4 requests");

    // Verify none have %22
    for request in requests {
        let url = request.url.as_str();
        assert!(!url.contains("%22"), "URL contains %22: {}", url);
        assert!(!url.contains("\\\""), "URL contains escaped quotes: {}", url);
    }
}

/// Test combination of inline enum and enum ref
#[tokio::test]
async fn test_inline_and_ref_enums() {
    let mock_server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/items"))
        .and(query_param("category", "food"))
        .and(query_param("priority", "high"))
        .respond_with(ResponseTemplate::new(200).set_body_json(Vec::<Item>::new()))
        .mount(&mock_server).await;

    let mut config = Configuration::new();
    config.base_path = mock_server.uri();

    let result = default_api::get_items(&config, Some("food"), Some(Priority::High)).await;
    assert!(result.is_ok(), "API call with both inline and ref enums should succeed");
}
