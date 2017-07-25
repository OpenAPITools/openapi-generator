/// Pet catehgry
///
/// A category for a pet
#[derive(Debug, Serialize, Deserialize)]
pub struct Category {
    id: Option<i64>,
    name: Option<String>,
}
