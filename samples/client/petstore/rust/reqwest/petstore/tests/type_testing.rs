extern crate petstore_reqwest;

use petstore_reqwest::models::TypeTesting;

#[test]
fn test_types() {
    let tt = TypeTesting::default();
    assert_eq!(type_of(tt.int32), "i32");
    assert_eq!(type_of(tt.int64), "i64");
    assert_eq!(type_of(tt.float), "f32");
    assert_eq!(type_of(tt.double), "f64");
    assert_eq!(type_of(tt.string), "alloc::string::String");
    assert_eq!(type_of(tt.boolean), "bool");
    assert_eq!(type_of(tt.uuid), "uuid::Uuid");
}

fn type_of<T>(_: T) -> &'static str {
    std::any::type_name::<T>()
}
