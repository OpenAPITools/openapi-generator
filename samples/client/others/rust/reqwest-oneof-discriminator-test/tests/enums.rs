use reqwest_oneof_discriminator::models::FruitOneOfEnumMappingDisc;

#[test]
fn test_oneof_discriminator_deserialize() {
  assert_eq!(
    serde_json::from_str::<FruitOneOfEnumMappingDisc>(
      r#"{"fruitType":"APPLE","seeds":10}"#
    ).unwrap(),
    FruitOneOfEnumMappingDisc::AppleOneOfEnumMappingDisc { seeds: 10 }
  );

  assert_eq!(
    serde_json::from_str::<FruitOneOfEnumMappingDisc>(
      r#"{"fruitType":"BANANA","length":10}"#
    ).unwrap(),
    FruitOneOfEnumMappingDisc::BananaOneOfEnumMappingDisc { length: 10 }
  );
}
