use composed_oneof_reqwest::models::{
    dog, cat, Circle, LinkNote, Note, Pet,
    Dog, Shape, ShapeKind, Square, TextNote, Cat,
};
use serde_json::json;

#[test]
fn inline_enum_tag_is_written_once() {
    let request = Pet::Cat(Box::new(Cat::new(
        cat::PetType::Cat,
        "Ana".to_string(),
    )));
    let text = serde_json::to_string(&request).unwrap();
    assert_eq!(text.matches("\"petType\"").count(), 1, "{text}");
    assert_eq!(
        serde_json::from_str::<serde_json::Value>(&text).unwrap(),
        json!({"petType": "cat", "name": "Ana"})
    );
    assert_eq!(
        serde_json::from_str::<Pet>(&text).unwrap(),
        request
    );
}

#[test]
fn inline_enum_tag_selects_the_variant() {
    let request: Pet = serde_json::from_value(
        json!({"petType": "dog", "breed": "beagle"}),
    )
    .unwrap();
    assert_eq!(
        request,
        Pet::Dog(Box::new(Dog::new(
            dog::PetType::Dog,
            "beagle".to_string(),
        )))
    );

    // the variant struct keeps serializing on its own
    let standalone = Cat::new(
        cat::PetType::Cat,
        "Bo".to_string(),
    );
    assert_eq!(
        serde_json::to_value(&standalone).unwrap(),
        json!({"petType": "cat", "name": "Bo"})
    );
}

#[test]
fn enum_ref_tag_round_trips() {
    for (shape, expected) in [
        (
            Shape::Circle(Box::new(Circle::new(ShapeKind::Circle, 1.5))),
            json!({"kind": "circle", "radius": 1.5}),
        ),
        (
            Shape::Square(Box::new(Square::new(ShapeKind::Square, 2.0))),
            json!({"kind": "square", "side": 2.0}),
        ),
    ] {
        let text = serde_json::to_string(&shape).unwrap();
        assert_eq!(text.matches("\"kind\"").count(), 1, "{text}");
        assert_eq!(
            serde_json::from_str::<serde_json::Value>(&text).unwrap(),
            expected
        );
        assert_eq!(serde_json::from_str::<Shape>(&text).unwrap(), shape);
    }
}

#[test]
fn union_variant_overrides_a_mismatched_tag() {
    // a shared enum lets the variant hold another variant's tag; Square::default() holds ShapeKind::Circle
    let shape = Shape::Square(Box::new(Square::new(ShapeKind::Circle, 2.0)));
    assert_eq!(
        serde_json::to_value(&shape).unwrap(),
        json!({"kind": "square", "side": 2.0})
    );
    assert_eq!(
        serde_json::to_value(Shape::Square(Box::default())).unwrap()["kind"],
        "square"
    );
}

#[test]
fn unknown_or_missing_tag_is_rejected() {
    let unknown =
        serde_json::from_value::<Pet>(json!({"petType": "other"})).unwrap_err();
    assert!(
        unknown.to_string().contains("unknown variant `other`"),
        "{unknown}"
    );
    let missing = serde_json::from_value::<Pet>(json!({"name": "Ana"})).unwrap_err();
    assert!(
        missing.to_string().contains("missing field `petType`"),
        "{missing}"
    );
}

#[test]
fn optional_enum_tag_left_unset_is_written_by_the_union() {
    let note = Note::Text(Box::new(TextNote::new()));
    assert_eq!(
        serde_json::to_value(&note).unwrap(),
        json!({"kind": "text"})
    );

    let note: Note =
        serde_json::from_value(json!({"kind": "link", "href": "https://example.com"})).unwrap();
    assert!(matches!(&note, Note::Link(_)));
    let text = serde_json::to_string(&note).unwrap();
    assert_eq!(text.matches("\"kind\"").count(), 1, "{text}");
    assert_eq!(serde_json::from_str::<Note>(&text).unwrap(), note);
    let _ = LinkNote::new();
}
