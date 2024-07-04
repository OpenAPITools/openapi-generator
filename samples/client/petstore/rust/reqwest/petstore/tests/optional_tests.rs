extern crate petstore_reqwest;

use petstore_reqwest::models::OptionalTesting;

#[test]
fn test_serialization() {
    let mut test = OptionalTesting {
        optional_nonnull: None,
        required_nonnull: "".to_string(),
        optional_nullable: None,
        required_nullable: None,
    };
    // Only the required fields should show up in JSON
    assert_eq!(
        serde_json::to_string(&test).unwrap(),
        "{\"required_nonnull\":\"\",\"required_nullable\":null}"
    );
    // Setting the outer of `optional_nullable` it should be serialized as null
    test.optional_nullable = Some(None);
    assert_eq!(
        serde_json::to_string(&test).unwrap(),
        "{\"required_nonnull\":\"\",\"optional_nullable\":null,\"required_nullable\":null}"
    );
}

#[test]
fn test_deserialization() {
    // `required_nullable` is missing so should fail to deserialize
    let input = "{\"required_nonnull\": \"\"}";
    assert!(serde_json::from_str::<OptionalTesting>(&input).is_err());

    // After adding `required_nullable` it should deserialize
    // `optional_nullable` should be None because it is not present
    let input = "{\"required_nonnull\": \"\", \"required_nullable\": null}";
    let out = serde_json::from_str::<OptionalTesting>(&input).unwrap();
    assert!(out.required_nullable.is_none());
    assert!(out.optional_nullable.is_none());

    // Setting `optional_nullable` to null should be Some(None)
    let input =
        "{\"required_nonnull\": \"\", \"required_nullable\": null, \"optional_nullable\": null}";
    assert!(matches!(
        serde_json::from_str::<OptionalTesting>(&input)
            .unwrap()
            .optional_nullable,
        Some(None)
    ));

    // Setting `optional_nullable` to a value
    let input =
        "{\"required_nonnull\": \"\", \"required_nullable\": null, \"optional_nullable\": \"abc\"}";
    assert!(matches!(
        serde_json::from_str::<OptionalTesting>(&input)
            .unwrap()
            .optional_nullable,
        Some(Some(_))
    ));
}
