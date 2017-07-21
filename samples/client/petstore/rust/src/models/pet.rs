/// a Pet
///
/// A pet for sale in the pet store
#[derive(Debug, Serialize, Deserialize)]
pub struct Pet {
    id: Option<i64>,
    category: Option<super::Category>,
    name: String,
    #[serde(rename = "photoUrls")] photo_urls: Vec<String>,
    tags: Vec<super::Tag>,
    status: Option<String>,
}

impl Pet {
  pub fn new(name: String, photo_urls: Vec<String>) -> Pet {
    Pet {
      id: None,
      category: None,
      name: name,
      photo_urls: photo_urls,
      tags: Vec::new(),
      status: None,
    }
  }

  pub fn set_id(&mut self, id: i64) {
    self.id = Some(id);
  }

  pub fn with_id(mut self, id: i64) -> Pet {
    self.id = Some(id);
    self
  }
}
