extern crate petstore_reqwest;

use petstore_reqwest::models::TypeTesting;
use uuid::Uuid;

#[test]
fn test_types() {
    let tt = TypeTesting {
        int32: 123,
        int64: 456,
        float: 12.34,
        double: 45.56,
        string: String::from("something"),
        boolean: true,
        uuid: Uuid::new_v4(),
        bytes: vec![1, 2, 3, 4],
        nullable_bytes: Some(Some(vec![1, 2, 3, 4])),
        decimal: String::from("foo"),
    };
    assert_eq!(type_of(tt.int32), "i32");
    assert_eq!(type_of(tt.int64), "i64");
    assert_eq!(type_of(tt.float), "f32");
    assert_eq!(type_of(tt.double), "f64");
    assert_eq!(type_of(tt.string), "alloc::string::String");
    assert_eq!(type_of(tt.boolean), "bool");
    assert_eq!(type_of(tt.uuid), "uuid::Uuid");
    assert_eq!(type_of(tt.decimal), "alloc::string::String");
}

fn type_of<T>(_: T) -> &'static str {
    std::any::type_name::<T>()
}
