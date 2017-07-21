/// Pet Tag
///
/// A tag for a pet
#[derive(Debug, Serialize, Deserialize)]
pub struct Tag {
    id: Option<i64>,
    name: Option<String>,
}
