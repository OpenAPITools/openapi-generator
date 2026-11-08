use rust_axum_oneof::models::{Hello, Message, YoMessage};
use serde_json::json;

#[test]
fn closed_object_rejects_unknown_fields() {
    let mut input = json!({"op": "Hello", "d": {"welcome_message": "known"}});
    assert!(serde_json::from_value::<Hello>(input.clone()).is_ok());

    input["unexpected"] = json!(true);
    let error = serde_json::from_value::<Hello>(input).unwrap_err();
    assert!(error.to_string().contains("unknown field `unexpected`"));
}

#[test]
fn tagged_closed_variants_reject_unknown_fields() {
    for mut input in [
        json!({"op": "Hello", "d": {"welcome_message": "known"}}),
        json!({"op": "Greeting", "d": {"greet_message": "known"}}),
        json!({"op": "Goodbye", "d": {"goodbye_message": "known"}}),
    ] {
        assert!(serde_json::from_value::<Message>(input.clone()).is_ok());
        input["unexpected"] = json!(true);
        assert!(serde_json::from_value::<Message>(input).is_err());
    }
}

#[test]
fn closed_object_keeps_its_declared_nested_object_open() {
    let input = json!({
        "op": "Hello",
        "d": {
            "welcome_message": "known",
            "custom": {"enabled": true, "items": [1, "two", null]}
        }
    });
    let model: Hello = serde_json::from_value(input.clone()).unwrap();
    assert_eq!(model.d.welcome_message, "known");
    assert!(serde_json::from_value::<Message>(input).is_ok());
}

#[test]
fn object_without_explicit_false_keeps_accepting_unknown_fields() {
    let input = json!({"op": "yo", "d": {"nickname": "known"}, "unexpected": true});
    let model: YoMessage = serde_json::from_value(input).unwrap();
    assert_eq!(model.d.nickname, "known");
}
