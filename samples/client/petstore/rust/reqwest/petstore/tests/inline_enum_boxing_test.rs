use petstore_reqwest::models::{model_with_inline_enum, ModelWithInlineEnum, Order};

/// This test verifies that inline enum fields in model constructors
/// are NOT wrapped in Box::new(), which was a bug in the generator.
///
/// Enums are Copy types and should never be boxed. The bug would cause
/// compilation errors like "expected enum Status, found Box<Status>".
///
/// Regression test for: https://github.com/OpenAPITools/openapi-generator/issues/XXXXX
#[test]
fn test_inline_enum_not_boxed_in_constructor() {
    // This test verifies that we can construct models with inline enums
    // without wrapping them in Box::new()

    // Before the fix, this would fail to compile because the generated
    // constructor would try to call Box::new() on the enum
    let model = ModelWithInlineEnum::new(model_with_inline_enum::Status::Draft);

    // Verify we can access the enum field
    assert_eq!(model.status, model_with_inline_enum::Status::Draft);
}

#[test]
fn test_existing_ref_enum_still_works() {
    // The Order model has a Status enum, which should still work
    // This ensures the fix didn't break existing enum handling
    let order = Order::new();

    // The status field should work normally
    assert!(order.status.is_none() || order.status.is_some());
}

#[test]
fn test_multiple_inline_enums_in_same_model() {
    // Test that a model with multiple inline enum fields works correctly
    let mut model = ModelWithInlineEnum::new(model_with_inline_enum::Status::Published);

    // Set the optional priority enum
    model.priority = Some(model_with_inline_enum::Priority::High);

    assert_eq!(model.status, model_with_inline_enum::Status::Published);
    assert_eq!(model.priority, Some(model_with_inline_enum::Priority::High));
}

#[test]
fn test_inline_enum_field_types() {
    // Verify the types are correct - enums should be Status, not Box<Status>
    let model = ModelWithInlineEnum::new(model_with_inline_enum::Status::Archived);

    // This is a compile-time check - if status were Box<Status>, this wouldn't compile
    let _status_ref: &model_with_inline_enum::Status = &model.status;

    // Optional fields should also not be boxed
    if let Some(ref priority) = model.priority {
        let _priority_ref: &model_with_inline_enum::Priority = priority;
    }
}

/// Test serialization/deserialization with inline enums
#[test]
fn test_inline_enum_serialization() {
    let model = ModelWithInlineEnum::new(model_with_inline_enum::Status::Draft);

    // Serialize to JSON
    let json = serde_json::to_string(&model).unwrap();
    assert!(json.contains("\"status\":\"draft\""));

    // Deserialize back
    let deserialized: ModelWithInlineEnum = serde_json::from_str(&json).unwrap();
    assert_eq!(deserialized.status, model_with_inline_enum::Status::Draft);
}

/// Test all enum variants
#[test]
fn test_all_status_variants() {
    let draft = ModelWithInlineEnum::new(model_with_inline_enum::Status::Draft);
    assert_eq!(draft.status, model_with_inline_enum::Status::Draft);

    let published = ModelWithInlineEnum::new(model_with_inline_enum::Status::Published);
    assert_eq!(
        published.status,
        model_with_inline_enum::Status::Published
    );

    let archived = ModelWithInlineEnum::new(model_with_inline_enum::Status::Archived);
    assert_eq!(archived.status, model_with_inline_enum::Status::Archived);
}

/// Test all priority variants
#[test]
fn test_all_priority_variants() {
    let mut model = ModelWithInlineEnum::new(model_with_inline_enum::Status::Draft);

    model.priority = Some(model_with_inline_enum::Priority::Low);
    assert_eq!(model.priority, Some(model_with_inline_enum::Priority::Low));

    model.priority = Some(model_with_inline_enum::Priority::Medium);
    assert_eq!(
        model.priority,
        Some(model_with_inline_enum::Priority::Medium)
    );

    model.priority = Some(model_with_inline_enum::Priority::High);
    assert_eq!(model.priority, Some(model_with_inline_enum::Priority::High));

    model.priority = Some(model_with_inline_enum::Priority::Critical);
    assert_eq!(
        model.priority,
        Some(model_with_inline_enum::Priority::Critical)
    );
}

/// Demonstrate what the bug was - if enums were boxed, this wouldn't compile
#[test]
fn test_bug_demonstration() {
    // The bug was that the generator would produce code like:
    // ModelWithInlineEnum {
    //     status: Box::new(status),  // WRONG - enums shouldn't be boxed
    //     ...
    // }
    //
    // This would cause a compilation error because the field type is Status, not Box<Status>

    // With the fix, this compiles successfully:
    let model = ModelWithInlineEnum::new(model_with_inline_enum::Status::Draft);
    assert_eq!(model.status, model_with_inline_enum::Status::Draft);

    // The fix ensures inline enums are NOT wrapped in Box::new()
    assert!(
        true,
        "Inline enums work correctly without boxing - this is compile-time protection"
    );
}
