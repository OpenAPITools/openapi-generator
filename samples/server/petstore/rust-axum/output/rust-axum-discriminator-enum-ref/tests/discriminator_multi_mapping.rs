use rust_axum_discriminator_enum_ref::models::*;

#[test]
fn test_discriminator_multiple_values_enum_ref() {
    // Several discriminator values map to the same variant, the deserialized one is kept
    for (tag, kind) in [
        ("green_apple", FruitType::GreenApple),
        ("red_apple", FruitType::RedApple),
    ] {
        let json = format!(r#"{{"fruitType":"{tag}","cultivar":"Gala"}}"#);
        let fruit: Fruit = serde_json::from_str(&json).unwrap();
        assert!(
            matches!(&fruit, Fruit::Apple(a) if a.fruit_type == kind && a.cultivar.as_deref() == Some("Gala"))
        );
        assert_eq!(serde_json::to_string(&fruit).unwrap(), json);
    }

    let json = r#"{"fruitType":"banana","lengthCm":20.5}"#;
    let fruit: Fruit = serde_json::from_str(json).unwrap();
    assert!(
        matches!(&fruit, Fruit::Banana(b) if b.fruit_type == FruitType::Banana && b.length_cm == Some(20.5))
    );
    assert_eq!(serde_json::to_string(&fruit).unwrap(), json);

    // Unknown, missing or model name tag is rejected
    assert!(serde_json::from_str::<Fruit>(r#"{"fruitType": "cherry"}"#).is_err());
    assert!(serde_json::from_str::<Fruit>(r#"{"fruitType": "Apple"}"#).is_err());
    assert!(serde_json::from_str::<Fruit>(r#"{"cultivar": "Gala"}"#).is_err());

    // Outside of the union, the discriminator property is required
    assert!(serde_json::from_str::<Apple>(r#"{"cultivar": "Gala"}"#).is_err());
    assert_eq!(
        serde_json::from_str::<Apple>(r#"{"fruitType": "red_apple"}"#)
            .unwrap()
            .fruit_type,
        FruitType::RedApple
    );

    // The discriminator value is chosen on construction
    assert_eq!(
        serde_json::to_string(&Fruit::Apple(Apple::new(FruitType::RedApple))).unwrap(),
        r#"{"fruitType":"red_apple"}"#
    );
}

#[test]
fn test_discriminator_multiple_values_not_defined_in_variant() {
    // The discriminator property is generated in the variant, and keeps the deserialized value
    for tag in ["student", "teacher"] {
        let json = format!(r#"{{"type":"human","name":"Ada","objectType":"{tag}"}}"#);
        let value: PersonOrVehicle = serde_json::from_str(&json).unwrap();
        assert!(
            matches!(&value, PersonOrVehicle::Person(p) if p.object_type == tag && p.name == "Ada")
        );
        assert_eq!(serde_json::to_string(&value).unwrap(), json);
    }

    // A variant mapped by a single value keeps a fixed discriminator
    let json = r#"{"type":"bike","speed":25.0,"objectType":"car"}"#;
    let value: PersonOrVehicle = serde_json::from_str(json).unwrap();
    assert!(matches!(&value, PersonOrVehicle::Vehicle(v) if v.speed == 25.0));
    assert_eq!(serde_json::to_string(&value).unwrap(), json);
    assert_eq!(
        serde_json::to_string(&PersonOrVehicle::Vehicle(Vehicle::new(
            "bike".to_string(),
            25.0
        )))
        .unwrap(),
        json
    );

    assert!(serde_json::from_str::<PersonOrVehicle>(r#"{"objectType":"Person"}"#).is_err());
}

#[test]
fn test_discriminator_multiple_values_tag_position() {
    // Tag first
    let fruit: Fruit =
        serde_json::from_str(r#"{"fruitType":"red_apple","cultivar":"Fuji"}"#).unwrap();
    assert!(matches!(&fruit, Fruit::Apple(a) if a.fruit_type == FruitType::RedApple));

    // Tag last
    let fruit: Fruit =
        serde_json::from_str(r#"{"cultivar":"Fuji","fruitType":"green_apple"}"#).unwrap();
    assert!(matches!(&fruit, Fruit::Apple(a) if a.fruit_type == FruitType::GreenApple));

    // Tag in the middle, with unknown properties of any type around it
    let value: PersonOrVehicle = serde_json::from_str(
        r#"{"extra":{"a":[1,2.5,null]},"name":"Ada","objectType":"teacher","type":"human","other":[{"b":true}]}"#,
    )
    .unwrap();
    assert!(matches!(&value, PersonOrVehicle::Person(p) if p.object_type == "teacher"));

    // Invalid variant content
    assert!(serde_json::from_str::<Fruit>(r#"{"fruitType":"banana","lengthCm":"long"}"#).is_err());

    // Duplicated tag: the content is buffered into a serde_json::Value, the last one wins
    let fruit: Fruit =
        serde_json::from_str(r#"{"fruitType":"green_apple","fruitType":"red_apple"}"#).unwrap();
    assert!(matches!(&fruit, Fruit::Apple(a) if a.fruit_type == FruitType::RedApple));

    // Tag of the wrong type, not an object
    assert!(serde_json::from_str::<Fruit>(r#"{"fruitType":1}"#).is_err());
    assert!(serde_json::from_str::<Fruit>(r#"["banana"]"#).is_err());

    // Unknown tag lists the expected values
    let err = serde_json::from_str::<Fruit>(r#"{"fruitType":"cherry"}"#)
        .unwrap_err()
        .to_string();
    assert!(err.contains("unknown variant `cherry`"), "{err}");
}
