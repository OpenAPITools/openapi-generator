use rust_axum_discriminator_enum_ref::models::*;

#[test]
fn test_discriminator_referencing_enum() {
    // Tagged dispatch on an enum-typed discriminator
    let dog: Pet = serde_json::from_str(r#"{"kind": "dog", "bark": "woof"}"#).unwrap();
    assert!(
        matches!(&dog, Pet::Dog(d) if d.kind == PetKind::Dog && d.bark.as_deref() == Some("woof"))
    );
    assert_eq!(
        serde_json::to_string(&dog).unwrap(),
        r#"{"kind":"dog","bark":"woof"}"#
    );

    // Second variant of the same union
    let cat: Pet = serde_json::from_str(r#"{"kind": "cat", "meow": "purr"}"#).unwrap();
    assert!(matches!(&cat, Pet::Cat(c) if c.kind == PetKind::Cat));
    assert_eq!(
        serde_json::to_string(&cat).unwrap(),
        r#"{"kind":"cat","meow":"purr"}"#
    );

    // Unknown or missing tag is rejected
    assert!(serde_json::from_str::<Pet>(r#"{"kind": "bird"}"#).is_err());
    assert!(serde_json::from_str::<Pet>(r#"{"bark": "woof"}"#).is_err());

    // Constructors fill in the tag
    assert_eq!(
        serde_json::to_string(&Pet::Dog(Dog::new())).unwrap(),
        r#"{"kind":"dog"}"#
    );
    assert_eq!(
        serde_json::to_string(&Pet::Cat(Cat::new())).unwrap(),
        r#"{"kind":"cat"}"#
    );

    // The tag is always written from the variant, whatever the field holds
    let mismatched = Dog {
        kind: PetKind::Cat,
        bark: None,
    };
    assert_eq!(
        serde_json::to_string(&Pet::Dog(mismatched)).unwrap(),
        r#"{"kind":"dog"}"#
    );
}
