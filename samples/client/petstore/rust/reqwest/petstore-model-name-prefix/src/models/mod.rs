pub mod foo_action_container;
pub use self::foo_action_container::FooActionContainer;
pub mod foo_any_type_test;
pub use self::foo_any_type_test::FooAnyTypeTest;
pub mod foo_api_response;
pub use self::foo_api_response::FooApiResponse;
pub mod foo_array_item_ref_test;
pub use self::foo_array_item_ref_test::FooArrayItemRefTest;
pub mod foo_baz;
pub use self::foo_baz::FooBaz;
pub mod foo_category;
pub use self::foo_category::FooCategory;
pub mod foo_duplicate_test;
pub use self::foo_duplicate_test::FooDuplicateTest;
pub mod foo_duplicatetest;
pub use self::foo_duplicatetest::FooDuplicatetest;
pub mod foo_enum_array_testing;
pub use self::foo_enum_array_testing::FooEnumArrayTesting;
pub mod foo_nullable_array;
pub use self::foo_nullable_array::FooNullableArray;
pub mod foo_numeric_enum_testing;
pub use self::foo_numeric_enum_testing::FooNumericEnumTesting;
pub mod foo_optional_testing;
pub use self::foo_optional_testing::FooOptionalTesting;
pub mod foo_order;
pub use self::foo_order::FooOrder;
pub mod foo_person;
pub use self::foo_person::FooPerson;
pub mod foo_pet;
pub use self::foo_pet::FooPet;
pub mod foo_property_test;
pub use self::foo_property_test::FooPropertyTest;
pub mod foo_ref;
pub use self::foo_ref::FooRef;
pub mod foo_return;
pub use self::foo_return::FooReturn;
pub mod foo_tag;
pub use self::foo_tag::FooTag;
pub mod foo_test_all_of_with_multi_metadata_only;
pub use self::foo_test_all_of_with_multi_metadata_only::FooTestAllOfWithMultiMetadataOnly;
pub mod foo__tests_discriminator_duplicate_enums_get_200_response;
pub use self::foo__tests_discriminator_duplicate_enums_get_200_response::FooTestsDiscriminatorDuplicateEnumsGet200Response;
pub mod foo_type_testing;
pub use self::foo_type_testing::FooTypeTesting;
pub mod foo_unique_item_array_testing;
pub use self::foo_unique_item_array_testing::FooUniqueItemArrayTesting;
pub mod foo_user;
pub use self::foo_user::FooUser;
pub mod foo_vehicle;
pub use self::foo_vehicle::FooVehicle;
use serde::{Deserialize, Deserializer, Serializer};
use serde_with::{de::DeserializeAsWrap, ser::SerializeAsWrap, DeserializeAs, SerializeAs};
use std::marker::PhantomData;

pub(crate) struct DoubleOption<T>(PhantomData<T>);

impl<T, TAs> SerializeAs<Option<Option<T>>> for DoubleOption<TAs>
where
    TAs: SerializeAs<T>,
{
    fn serialize_as<S>(values: &Option<Option<T>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match values {
            None => serializer.serialize_unit(),
            Some(None) => serializer.serialize_none(),
            Some(Some(v)) => serializer.serialize_some(&SerializeAsWrap::<T, TAs>::new(v)),
        }
    }
}

impl<'de, T, TAs> DeserializeAs<'de, Option<Option<T>>> for DoubleOption<TAs>
where
    TAs: DeserializeAs<'de, T>,
    T: std::fmt::Debug,
{
    fn deserialize_as<D>(deserializer: D) -> Result<Option<Option<T>>, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(Some(
            DeserializeAsWrap::<Option<T>, Option<TAs>>::deserialize(deserializer)?
                .into_inner(),
        ))
    }
}
