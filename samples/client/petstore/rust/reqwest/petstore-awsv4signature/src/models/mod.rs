pub mod action_container;
pub use self::action_container::ActionContainer;
pub mod any_type_test;
pub use self::any_type_test::AnyTypeTest;
pub mod api_response;
pub use self::api_response::ApiResponse;
pub mod array_item_ref_test;
pub use self::array_item_ref_test::ArrayItemRefTest;
pub mod baz;
pub use self::baz::Baz;
pub mod category;
pub use self::category::Category;
pub mod duplicate_test;
pub use self::duplicate_test::DuplicateTest;
pub mod duplicatetest;
pub use self::duplicatetest::Duplicatetest;
pub mod enum_array_testing;
pub use self::enum_array_testing::EnumArrayTesting;
pub mod nullable_array;
pub use self::nullable_array::NullableArray;
pub mod numeric_enum_testing;
pub use self::numeric_enum_testing::NumericEnumTesting;
pub mod optional_testing;
pub use self::optional_testing::OptionalTesting;
pub mod order;
pub use self::order::Order;
pub mod person;
pub use self::person::Person;
pub mod pet;
pub use self::pet::Pet;
pub mod property_test;
pub use self::property_test::PropertyTest;
pub mod model_ref;
pub use self::model_ref::Ref;
pub mod model_return;
pub use self::model_return::Return;
pub mod tag;
pub use self::tag::Tag;
pub mod test_all_of_with_multi_metadata_only;
pub use self::test_all_of_with_multi_metadata_only::TestAllOfWithMultiMetadataOnly;
pub mod _tests_discriminator_duplicate_enums_get_200_response;
pub use self::_tests_discriminator_duplicate_enums_get_200_response::TestsDiscriminatorDuplicateEnumsGet200Response;
pub mod type_testing;
pub use self::type_testing::TypeTesting;
pub mod unique_item_array_testing;
pub use self::unique_item_array_testing::UniqueItemArrayTesting;
pub mod user;
pub use self::user::User;
pub mod vehicle;
pub use self::vehicle::Vehicle;
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
