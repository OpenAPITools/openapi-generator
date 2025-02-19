#![allow(unused_qualifications)]

use http::HeaderValue;
use validator::Validate;

#[cfg(feature = "server")]
use crate::header;
use crate::{models, types::*};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct UsersPostHeaderParams {
    pub some_uid: uuid::Uuid,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct HeaderUuid(uuid::Uuid);

impl validator::Validate for HeaderUuid {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<uuid::Uuid> for HeaderUuid {
    fn from(x: uuid::Uuid) -> Self {
        HeaderUuid(x)
    }
}

impl std::convert::From<HeaderUuid> for uuid::Uuid {
    fn from(x: HeaderUuid) -> Self {
        x.0
    }
}

impl std::ops::Deref for HeaderUuid {
    type Target = uuid::Uuid;
    fn deref(&self) -> &uuid::Uuid {
        &self.0
    }
}

impl std::ops::DerefMut for HeaderUuid {
    fn deref_mut(&mut self) -> &mut uuid::Uuid {
        &mut self.0
    }
}
