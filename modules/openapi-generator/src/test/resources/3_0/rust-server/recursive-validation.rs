// Generate recursive-validation.yaml with --generate-alias-as-model and
// --additional-properties packageName=recursive_validation. Copy this file to
// tests/recursive_validation.rs in the output, then run
// cargo test --no-default-features --features validate --test recursive_validation.
#![cfg(feature = "validate")]
use recursive_validation::models::{
    BoundedChildren, MixedChoice, ModelAnyOf, ModelChoice, NamedModelChoice, NullableChildren,
    NullableStringItems, Parent, Positive, PositiveMap, Positives, PrimitiveChoice, StringAlias,
    StringItems, StringValue,
};
use serde_valid::Validate;

#[test]
fn root_validation_reaches_array_wrapper_items_and_bounds() {
    for values in [vec![], vec![1], vec![1, 1], vec![1, 1, 1], vec![0]] {
        let expected = (1..=2).contains(&values.len()) && values.iter().all(|value| *value >= 1);
        let root = Parent {
            children: Positives::from(
                values
                    .into_iter()
                    .map(|value| Positive { value })
                    .collect::<Vec<_>>(),
            ),
        };
        assert_eq!(root.validate().is_ok(), expected);
    }
}

#[test]
fn bounded_fields_validate_their_size_and_children() {
    for values in [vec![], vec![1], vec![1, 1], vec![1, 1, 1], vec![0]] {
        for optional in [false, true] {
            let expected =
                (1..=2).contains(&values.len()) && values.iter().all(|value| *value >= 1);
            let children = values
                .iter()
                .map(|value| Positive { value: *value })
                .collect();
            let root = if optional {
                BoundedChildren {
                    children: vec![Positive { value: 1 }],
                    optional_children: Some(children),
                }
            } else {
                BoundedChildren {
                    children,
                    optional_children: None,
                }
            };
            assert_eq!(root.validate().is_ok(), expected);
        }
    }
}

#[test]
fn primitive_array_wrappers_validate_their_size() {
    for size in 0..=3 {
        let value = StringItems::from(vec![String::new(); size]);
        assert_eq!(value.validate().is_ok(), (1..=2).contains(&size));
    }
}

#[test]
fn map_wrappers_validate_their_values() {
    for value in [0, 1] {
        let root = PositiveMap::from(std::collections::HashMap::from([(
            "key".to_owned(),
            Positive { value },
        )]));
        assert_eq!(root.validate().is_ok(), value == 1);
    }
}

#[test]
fn reference_wrappers_validate_their_inner_model() {
    for value in ["", "x"] {
        let root = StringAlias::from(StringValue::from(value.to_owned()));
        assert_eq!(root.validate().is_ok(), !value.is_empty());
    }
}

#[test]
fn union_wrappers_compile_with_model_and_primitive_members() {
    let model = ModelChoice::from(swagger::OneOf2::A(Positive { value: 1 }));
    let named = NamedModelChoice::from(swagger::OneOf2::A(Positive { value: 1 }));
    let any = ModelAnyOf::from(swagger::AnyOf2::B(Positives::from(vec![Positive {
        value: 1,
    }])));
    let mixed = MixedChoice::from(swagger::OneOf2::B("future".to_owned()));
    let primitive: PrimitiveChoice = serde_json::from_str("42").unwrap();
    assert!(model.validate().is_ok());
    assert!(named.validate().is_ok());
    assert!(any.validate().is_ok());
    assert!(mixed.validate().is_ok());
    assert!(primitive.validate().is_ok());
}

#[test]
fn bounded_nullable_fields_keep_compiling_and_validate_present_models() {
    let value = NullableChildren {
        children: swagger::Nullable::Null,
        optional_children: None,
    };
    assert!(value.validate().is_ok());
    for optional in [false, true] {
        for value in [0, 1] {
            let children = swagger::Nullable::Present(vec![Positive { value }]);
            let root = if optional {
                NullableChildren {
                    children: swagger::Nullable::Null,
                    optional_children: Some(children),
                }
            } else {
                NullableChildren {
                    children,
                    optional_children: Some(swagger::Nullable::Null),
                }
            };
            assert_eq!(root.validate().is_ok(), value == 1);
        }
    }
    let items = NullableStringItems::from(vec![String::new()]);
    assert!(items.validate().is_ok());
}
