use additional_properties::models::{
    ClosedNested, ClosedObject, ClosedOptional, OpenObject, TypedAdditionalProperties,
    UnspecifiedObject,
};
use serde_json::json;

#[test]
fn closed_object_accepts_declared_fields_and_free_form_config() {
    let input = json!({
        "name": "example",
        "nested": {"value": "known"},
        "config": {"custom": {"enabled": true}, "items": [1, "two", null]}
    });
    let model: ClosedObject = serde_json::from_value(input.clone()).unwrap();
    assert_eq!(serde_json::to_value(model).unwrap(), input);
}

#[test]
fn closed_object_rejects_unknown_fields() {
    let error = serde_json::from_value::<ClosedObject>(json!({
        "name": "example",
        "nested": {"value": "known"},
        "config": {},
        "unexpected": true
    }))
    .unwrap_err();
    assert!(error.to_string().contains("unknown field `unexpected`"));
}

#[test]
fn closed_nested_object_rejects_unknown_fields() {
    let error = serde_json::from_value::<ClosedNested>(json!({
        "value": "known",
        "unexpected": true
    }))
    .unwrap_err();
    assert!(error.to_string().contains("unknown field `unexpected`"));

    assert!(
        serde_json::from_value::<ClosedObject>(json!({
            "name": "example",
            "nested": {"value": "known", "unexpected": true},
            "config": {}
        }))
        .is_err()
    );
}

#[test]
fn closed_object_with_optional_fields_accepts_empty_objects() {
    assert!(serde_json::from_value::<ClosedOptional>(json!({})).is_ok());
    assert!(serde_json::from_value::<ClosedOptional>(json!({"value": "known"})).is_ok());
    assert!(serde_json::from_value::<ClosedOptional>(json!({"unexpected": true})).is_err());
}

#[test]
fn other_additional_properties_settings_keep_accepting_unknown_fields() {
    let input = json!({"name": "example", "unexpected": "extra"});
    assert!(serde_json::from_value::<OpenObject>(input.clone()).is_ok());
    assert!(serde_json::from_value::<TypedAdditionalProperties>(input.clone()).is_ok());
    assert!(serde_json::from_value::<UnspecifiedObject>(input).is_ok());
}
