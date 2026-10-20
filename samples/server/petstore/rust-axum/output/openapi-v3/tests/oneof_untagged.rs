use openapi_v3::models::OneOfGet200Response;

#[test]
fn test_oneof_schema_untagged() {
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
    struct Test {
        value: OneOfGet200Response,
    }

    let test0 = r#"{"value": "ignored"}"#;
    let test1 = r#"{"value": 123}"#;
    let test2 = r#"{"value": ["foo", "bar"]}"#;

    let test3 = Test {
        value: OneOfGet200Response::I32(123),
    };
    let test4 = Test {
        value: OneOfGet200Response::VecOfString(vec!["foo".to_string(), "bar".to_string()].into()),
    };

    let test5 = r#"{"value":123}"#;
    let test6 = r#"{"value":["foo","bar"]}"#;

    assert!(serde_json::from_str::<Test>(test0).is_err());
    assert!(serde_json::from_str::<Test>(test1).is_ok());
    assert!(serde_json::from_str::<Test>(test2).is_ok());

    assert_eq!(
        serde_json::to_string(&test3).expect("Serialization error"),
        test5
    );
    assert_eq!(
        serde_json::to_string(&test4).expect("Serialization error"),
        test6
    );
}
