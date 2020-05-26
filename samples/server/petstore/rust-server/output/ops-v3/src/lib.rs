#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

use futures::Stream;
use std::io::Error;

#[deprecated(note = "Import swagger-rs directly")]
pub use swagger::{ApiError, ContextWrapper};
#[deprecated(note = "Import futures directly")]
pub use futures::Future;

pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "0.0.1";

#[derive(Debug, PartialEq)]
pub enum Op10GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op11GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op12GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op13GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op14GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op15GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op16GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op17GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op18GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op19GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op1GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op20GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op21GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op22GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op23GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op24GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op25GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op26GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op27GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op28GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op29GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op2GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op30GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op31GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op32GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op33GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op34GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op35GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op36GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op37GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op3GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op4GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op5GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op6GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op7GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op8GetResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum Op9GetResponse {
    /// OK
    OK
}

/// API
pub trait Api<C> {
    fn op10_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op10GetResponse, Error=ApiError> + Send>;

    fn op11_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op11GetResponse, Error=ApiError> + Send>;

    fn op12_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op12GetResponse, Error=ApiError> + Send>;

    fn op13_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op13GetResponse, Error=ApiError> + Send>;

    fn op14_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op14GetResponse, Error=ApiError> + Send>;

    fn op15_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op15GetResponse, Error=ApiError> + Send>;

    fn op16_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op16GetResponse, Error=ApiError> + Send>;

    fn op17_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op17GetResponse, Error=ApiError> + Send>;

    fn op18_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op18GetResponse, Error=ApiError> + Send>;

    fn op19_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op19GetResponse, Error=ApiError> + Send>;

    fn op1_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op1GetResponse, Error=ApiError> + Send>;

    fn op20_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op20GetResponse, Error=ApiError> + Send>;

    fn op21_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op21GetResponse, Error=ApiError> + Send>;

    fn op22_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op22GetResponse, Error=ApiError> + Send>;

    fn op23_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op23GetResponse, Error=ApiError> + Send>;

    fn op24_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op24GetResponse, Error=ApiError> + Send>;

    fn op25_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op25GetResponse, Error=ApiError> + Send>;

    fn op26_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op26GetResponse, Error=ApiError> + Send>;

    fn op27_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op27GetResponse, Error=ApiError> + Send>;

    fn op28_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op28GetResponse, Error=ApiError> + Send>;

    fn op29_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op29GetResponse, Error=ApiError> + Send>;

    fn op2_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op2GetResponse, Error=ApiError> + Send>;

    fn op30_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op30GetResponse, Error=ApiError> + Send>;

    fn op31_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op31GetResponse, Error=ApiError> + Send>;

    fn op32_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op32GetResponse, Error=ApiError> + Send>;

    fn op33_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op33GetResponse, Error=ApiError> + Send>;

    fn op34_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op34GetResponse, Error=ApiError> + Send>;

    fn op35_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op35GetResponse, Error=ApiError> + Send>;

    fn op36_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op36GetResponse, Error=ApiError> + Send>;

    fn op37_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op37GetResponse, Error=ApiError> + Send>;

    fn op3_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op3GetResponse, Error=ApiError> + Send>;

    fn op4_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op4GetResponse, Error=ApiError> + Send>;

    fn op5_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op5GetResponse, Error=ApiError> + Send>;

    fn op6_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op6GetResponse, Error=ApiError> + Send>;

    fn op7_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op7GetResponse, Error=ApiError> + Send>;

    fn op8_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op8GetResponse, Error=ApiError> + Send>;

    fn op9_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Op9GetResponse, Error=ApiError> + Send>;

}

/// API without a `Context`
pub trait ApiNoContext {
    fn op10_get(
        &self,
        ) -> Box<dyn Future<Item=Op10GetResponse, Error=ApiError> + Send>;

    fn op11_get(
        &self,
        ) -> Box<dyn Future<Item=Op11GetResponse, Error=ApiError> + Send>;

    fn op12_get(
        &self,
        ) -> Box<dyn Future<Item=Op12GetResponse, Error=ApiError> + Send>;

    fn op13_get(
        &self,
        ) -> Box<dyn Future<Item=Op13GetResponse, Error=ApiError> + Send>;

    fn op14_get(
        &self,
        ) -> Box<dyn Future<Item=Op14GetResponse, Error=ApiError> + Send>;

    fn op15_get(
        &self,
        ) -> Box<dyn Future<Item=Op15GetResponse, Error=ApiError> + Send>;

    fn op16_get(
        &self,
        ) -> Box<dyn Future<Item=Op16GetResponse, Error=ApiError> + Send>;

    fn op17_get(
        &self,
        ) -> Box<dyn Future<Item=Op17GetResponse, Error=ApiError> + Send>;

    fn op18_get(
        &self,
        ) -> Box<dyn Future<Item=Op18GetResponse, Error=ApiError> + Send>;

    fn op19_get(
        &self,
        ) -> Box<dyn Future<Item=Op19GetResponse, Error=ApiError> + Send>;

    fn op1_get(
        &self,
        ) -> Box<dyn Future<Item=Op1GetResponse, Error=ApiError> + Send>;

    fn op20_get(
        &self,
        ) -> Box<dyn Future<Item=Op20GetResponse, Error=ApiError> + Send>;

    fn op21_get(
        &self,
        ) -> Box<dyn Future<Item=Op21GetResponse, Error=ApiError> + Send>;

    fn op22_get(
        &self,
        ) -> Box<dyn Future<Item=Op22GetResponse, Error=ApiError> + Send>;

    fn op23_get(
        &self,
        ) -> Box<dyn Future<Item=Op23GetResponse, Error=ApiError> + Send>;

    fn op24_get(
        &self,
        ) -> Box<dyn Future<Item=Op24GetResponse, Error=ApiError> + Send>;

    fn op25_get(
        &self,
        ) -> Box<dyn Future<Item=Op25GetResponse, Error=ApiError> + Send>;

    fn op26_get(
        &self,
        ) -> Box<dyn Future<Item=Op26GetResponse, Error=ApiError> + Send>;

    fn op27_get(
        &self,
        ) -> Box<dyn Future<Item=Op27GetResponse, Error=ApiError> + Send>;

    fn op28_get(
        &self,
        ) -> Box<dyn Future<Item=Op28GetResponse, Error=ApiError> + Send>;

    fn op29_get(
        &self,
        ) -> Box<dyn Future<Item=Op29GetResponse, Error=ApiError> + Send>;

    fn op2_get(
        &self,
        ) -> Box<dyn Future<Item=Op2GetResponse, Error=ApiError> + Send>;

    fn op30_get(
        &self,
        ) -> Box<dyn Future<Item=Op30GetResponse, Error=ApiError> + Send>;

    fn op31_get(
        &self,
        ) -> Box<dyn Future<Item=Op31GetResponse, Error=ApiError> + Send>;

    fn op32_get(
        &self,
        ) -> Box<dyn Future<Item=Op32GetResponse, Error=ApiError> + Send>;

    fn op33_get(
        &self,
        ) -> Box<dyn Future<Item=Op33GetResponse, Error=ApiError> + Send>;

    fn op34_get(
        &self,
        ) -> Box<dyn Future<Item=Op34GetResponse, Error=ApiError> + Send>;

    fn op35_get(
        &self,
        ) -> Box<dyn Future<Item=Op35GetResponse, Error=ApiError> + Send>;

    fn op36_get(
        &self,
        ) -> Box<dyn Future<Item=Op36GetResponse, Error=ApiError> + Send>;

    fn op37_get(
        &self,
        ) -> Box<dyn Future<Item=Op37GetResponse, Error=ApiError> + Send>;

    fn op3_get(
        &self,
        ) -> Box<dyn Future<Item=Op3GetResponse, Error=ApiError> + Send>;

    fn op4_get(
        &self,
        ) -> Box<dyn Future<Item=Op4GetResponse, Error=ApiError> + Send>;

    fn op5_get(
        &self,
        ) -> Box<dyn Future<Item=Op5GetResponse, Error=ApiError> + Send>;

    fn op6_get(
        &self,
        ) -> Box<dyn Future<Item=Op6GetResponse, Error=ApiError> + Send>;

    fn op7_get(
        &self,
        ) -> Box<dyn Future<Item=Op7GetResponse, Error=ApiError> + Send>;

    fn op8_get(
        &self,
        ) -> Box<dyn Future<Item=Op8GetResponse, Error=ApiError> + Send>;

    fn op9_get(
        &self,
        ) -> Box<dyn Future<Item=Op9GetResponse, Error=ApiError> + Send>;

}

/// Trait to extend an API to make it easy to bind it to a context.
pub trait ContextWrapperExt<'a, C> where Self: Sized {
    /// Binds this API to a context.
    fn with_context(self: &'a Self, context: C) -> ContextWrapper<'a, Self, C>;
}

impl<'a, T: Api<C> + Sized, C> ContextWrapperExt<'a, C> for T {
    fn with_context(self: &'a T, context: C) -> ContextWrapper<'a, T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

impl<'a, T: Api<C>, C> ApiNoContext for ContextWrapper<'a, T, C> {
    fn op10_get(
        &self,
        ) -> Box<dyn Future<Item=Op10GetResponse, Error=ApiError> + Send>
    {
        self.api().op10_get(&self.context())
    }

    fn op11_get(
        &self,
        ) -> Box<dyn Future<Item=Op11GetResponse, Error=ApiError> + Send>
    {
        self.api().op11_get(&self.context())
    }

    fn op12_get(
        &self,
        ) -> Box<dyn Future<Item=Op12GetResponse, Error=ApiError> + Send>
    {
        self.api().op12_get(&self.context())
    }

    fn op13_get(
        &self,
        ) -> Box<dyn Future<Item=Op13GetResponse, Error=ApiError> + Send>
    {
        self.api().op13_get(&self.context())
    }

    fn op14_get(
        &self,
        ) -> Box<dyn Future<Item=Op14GetResponse, Error=ApiError> + Send>
    {
        self.api().op14_get(&self.context())
    }

    fn op15_get(
        &self,
        ) -> Box<dyn Future<Item=Op15GetResponse, Error=ApiError> + Send>
    {
        self.api().op15_get(&self.context())
    }

    fn op16_get(
        &self,
        ) -> Box<dyn Future<Item=Op16GetResponse, Error=ApiError> + Send>
    {
        self.api().op16_get(&self.context())
    }

    fn op17_get(
        &self,
        ) -> Box<dyn Future<Item=Op17GetResponse, Error=ApiError> + Send>
    {
        self.api().op17_get(&self.context())
    }

    fn op18_get(
        &self,
        ) -> Box<dyn Future<Item=Op18GetResponse, Error=ApiError> + Send>
    {
        self.api().op18_get(&self.context())
    }

    fn op19_get(
        &self,
        ) -> Box<dyn Future<Item=Op19GetResponse, Error=ApiError> + Send>
    {
        self.api().op19_get(&self.context())
    }

    fn op1_get(
        &self,
        ) -> Box<dyn Future<Item=Op1GetResponse, Error=ApiError> + Send>
    {
        self.api().op1_get(&self.context())
    }

    fn op20_get(
        &self,
        ) -> Box<dyn Future<Item=Op20GetResponse, Error=ApiError> + Send>
    {
        self.api().op20_get(&self.context())
    }

    fn op21_get(
        &self,
        ) -> Box<dyn Future<Item=Op21GetResponse, Error=ApiError> + Send>
    {
        self.api().op21_get(&self.context())
    }

    fn op22_get(
        &self,
        ) -> Box<dyn Future<Item=Op22GetResponse, Error=ApiError> + Send>
    {
        self.api().op22_get(&self.context())
    }

    fn op23_get(
        &self,
        ) -> Box<dyn Future<Item=Op23GetResponse, Error=ApiError> + Send>
    {
        self.api().op23_get(&self.context())
    }

    fn op24_get(
        &self,
        ) -> Box<dyn Future<Item=Op24GetResponse, Error=ApiError> + Send>
    {
        self.api().op24_get(&self.context())
    }

    fn op25_get(
        &self,
        ) -> Box<dyn Future<Item=Op25GetResponse, Error=ApiError> + Send>
    {
        self.api().op25_get(&self.context())
    }

    fn op26_get(
        &self,
        ) -> Box<dyn Future<Item=Op26GetResponse, Error=ApiError> + Send>
    {
        self.api().op26_get(&self.context())
    }

    fn op27_get(
        &self,
        ) -> Box<dyn Future<Item=Op27GetResponse, Error=ApiError> + Send>
    {
        self.api().op27_get(&self.context())
    }

    fn op28_get(
        &self,
        ) -> Box<dyn Future<Item=Op28GetResponse, Error=ApiError> + Send>
    {
        self.api().op28_get(&self.context())
    }

    fn op29_get(
        &self,
        ) -> Box<dyn Future<Item=Op29GetResponse, Error=ApiError> + Send>
    {
        self.api().op29_get(&self.context())
    }

    fn op2_get(
        &self,
        ) -> Box<dyn Future<Item=Op2GetResponse, Error=ApiError> + Send>
    {
        self.api().op2_get(&self.context())
    }

    fn op30_get(
        &self,
        ) -> Box<dyn Future<Item=Op30GetResponse, Error=ApiError> + Send>
    {
        self.api().op30_get(&self.context())
    }

    fn op31_get(
        &self,
        ) -> Box<dyn Future<Item=Op31GetResponse, Error=ApiError> + Send>
    {
        self.api().op31_get(&self.context())
    }

    fn op32_get(
        &self,
        ) -> Box<dyn Future<Item=Op32GetResponse, Error=ApiError> + Send>
    {
        self.api().op32_get(&self.context())
    }

    fn op33_get(
        &self,
        ) -> Box<dyn Future<Item=Op33GetResponse, Error=ApiError> + Send>
    {
        self.api().op33_get(&self.context())
    }

    fn op34_get(
        &self,
        ) -> Box<dyn Future<Item=Op34GetResponse, Error=ApiError> + Send>
    {
        self.api().op34_get(&self.context())
    }

    fn op35_get(
        &self,
        ) -> Box<dyn Future<Item=Op35GetResponse, Error=ApiError> + Send>
    {
        self.api().op35_get(&self.context())
    }

    fn op36_get(
        &self,
        ) -> Box<dyn Future<Item=Op36GetResponse, Error=ApiError> + Send>
    {
        self.api().op36_get(&self.context())
    }

    fn op37_get(
        &self,
        ) -> Box<dyn Future<Item=Op37GetResponse, Error=ApiError> + Send>
    {
        self.api().op37_get(&self.context())
    }

    fn op3_get(
        &self,
        ) -> Box<dyn Future<Item=Op3GetResponse, Error=ApiError> + Send>
    {
        self.api().op3_get(&self.context())
    }

    fn op4_get(
        &self,
        ) -> Box<dyn Future<Item=Op4GetResponse, Error=ApiError> + Send>
    {
        self.api().op4_get(&self.context())
    }

    fn op5_get(
        &self,
        ) -> Box<dyn Future<Item=Op5GetResponse, Error=ApiError> + Send>
    {
        self.api().op5_get(&self.context())
    }

    fn op6_get(
        &self,
        ) -> Box<dyn Future<Item=Op6GetResponse, Error=ApiError> + Send>
    {
        self.api().op6_get(&self.context())
    }

    fn op7_get(
        &self,
        ) -> Box<dyn Future<Item=Op7GetResponse, Error=ApiError> + Send>
    {
        self.api().op7_get(&self.context())
    }

    fn op8_get(
        &self,
        ) -> Box<dyn Future<Item=Op8GetResponse, Error=ApiError> + Send>
    {
        self.api().op8_get(&self.context())
    }

    fn op9_get(
        &self,
        ) -> Box<dyn Future<Item=Op9GetResponse, Error=ApiError> + Send>
    {
        self.api().op9_get(&self.context())
    }

}

#[cfg(feature = "client")]
pub mod client;

// Re-export Client as a top-level name
#[cfg(feature = "client")]
pub use client::Client;

#[cfg(feature = "server")]
pub mod server;

// Re-export router() as a top-level name
#[cfg(feature = "server")]
pub use self::server::Service;

#[cfg(feature = "server")]
pub mod context;

pub mod models;

#[cfg(any(feature = "client", feature = "server"))]
pub(crate) mod header;
