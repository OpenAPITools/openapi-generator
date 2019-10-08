#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]
use async_trait::async_trait;
use futures::{FutureExt, Stream, StreamExt, TryStreamExt};
use openapi_context::ContextWrapper;
use std::io::Error;

#[allow(unused_imports)]
use std::collections::HashMap;

#[cfg(any(feature = "client", feature = "server"))]
mod mimetypes;

pub use openapi_context::ApiError;
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
#[async_trait]
pub trait Api<C> {


    async fn op10_get(&mut self, context: &C) -> Result<Op10GetResponse, ApiError>;


    async fn op11_get(&mut self, context: &C) -> Result<Op11GetResponse, ApiError>;


    async fn op12_get(&mut self, context: &C) -> Result<Op12GetResponse, ApiError>;


    async fn op13_get(&mut self, context: &C) -> Result<Op13GetResponse, ApiError>;


    async fn op14_get(&mut self, context: &C) -> Result<Op14GetResponse, ApiError>;


    async fn op15_get(&mut self, context: &C) -> Result<Op15GetResponse, ApiError>;


    async fn op16_get(&mut self, context: &C) -> Result<Op16GetResponse, ApiError>;


    async fn op17_get(&mut self, context: &C) -> Result<Op17GetResponse, ApiError>;


    async fn op18_get(&mut self, context: &C) -> Result<Op18GetResponse, ApiError>;


    async fn op19_get(&mut self, context: &C) -> Result<Op19GetResponse, ApiError>;


    async fn op1_get(&mut self, context: &C) -> Result<Op1GetResponse, ApiError>;


    async fn op20_get(&mut self, context: &C) -> Result<Op20GetResponse, ApiError>;


    async fn op21_get(&mut self, context: &C) -> Result<Op21GetResponse, ApiError>;


    async fn op22_get(&mut self, context: &C) -> Result<Op22GetResponse, ApiError>;


    async fn op23_get(&mut self, context: &C) -> Result<Op23GetResponse, ApiError>;


    async fn op24_get(&mut self, context: &C) -> Result<Op24GetResponse, ApiError>;


    async fn op25_get(&mut self, context: &C) -> Result<Op25GetResponse, ApiError>;


    async fn op26_get(&mut self, context: &C) -> Result<Op26GetResponse, ApiError>;


    async fn op27_get(&mut self, context: &C) -> Result<Op27GetResponse, ApiError>;


    async fn op28_get(&mut self, context: &C) -> Result<Op28GetResponse, ApiError>;


    async fn op29_get(&mut self, context: &C) -> Result<Op29GetResponse, ApiError>;


    async fn op2_get(&mut self, context: &C) -> Result<Op2GetResponse, ApiError>;


    async fn op30_get(&mut self, context: &C) -> Result<Op30GetResponse, ApiError>;


    async fn op31_get(&mut self, context: &C) -> Result<Op31GetResponse, ApiError>;


    async fn op32_get(&mut self, context: &C) -> Result<Op32GetResponse, ApiError>;


    async fn op33_get(&mut self, context: &C) -> Result<Op33GetResponse, ApiError>;


    async fn op34_get(&mut self, context: &C) -> Result<Op34GetResponse, ApiError>;


    async fn op35_get(&mut self, context: &C) -> Result<Op35GetResponse, ApiError>;


    async fn op36_get(&mut self, context: &C) -> Result<Op36GetResponse, ApiError>;


    async fn op37_get(&mut self, context: &C) -> Result<Op37GetResponse, ApiError>;


    async fn op3_get(&mut self, context: &C) -> Result<Op3GetResponse, ApiError>;


    async fn op4_get(&mut self, context: &C) -> Result<Op4GetResponse, ApiError>;


    async fn op5_get(&mut self, context: &C) -> Result<Op5GetResponse, ApiError>;


    async fn op6_get(&mut self, context: &C) -> Result<Op6GetResponse, ApiError>;


    async fn op7_get(&mut self, context: &C) -> Result<Op7GetResponse, ApiError>;


    async fn op8_get(&mut self, context: &C) -> Result<Op8GetResponse, ApiError>;


    async fn op9_get(&mut self, context: &C) -> Result<Op9GetResponse, ApiError>;

}

/// API without a `Context`
#[async_trait]
pub trait ApiNoContext {


    async fn op10_get(&mut self) -> Result<Op10GetResponse, ApiError>;


    async fn op11_get(&mut self) -> Result<Op11GetResponse, ApiError>;


    async fn op12_get(&mut self) -> Result<Op12GetResponse, ApiError>;


    async fn op13_get(&mut self) -> Result<Op13GetResponse, ApiError>;


    async fn op14_get(&mut self) -> Result<Op14GetResponse, ApiError>;


    async fn op15_get(&mut self) -> Result<Op15GetResponse, ApiError>;


    async fn op16_get(&mut self) -> Result<Op16GetResponse, ApiError>;


    async fn op17_get(&mut self) -> Result<Op17GetResponse, ApiError>;


    async fn op18_get(&mut self) -> Result<Op18GetResponse, ApiError>;


    async fn op19_get(&mut self) -> Result<Op19GetResponse, ApiError>;


    async fn op1_get(&mut self) -> Result<Op1GetResponse, ApiError>;


    async fn op20_get(&mut self) -> Result<Op20GetResponse, ApiError>;


    async fn op21_get(&mut self) -> Result<Op21GetResponse, ApiError>;


    async fn op22_get(&mut self) -> Result<Op22GetResponse, ApiError>;


    async fn op23_get(&mut self) -> Result<Op23GetResponse, ApiError>;


    async fn op24_get(&mut self) -> Result<Op24GetResponse, ApiError>;


    async fn op25_get(&mut self) -> Result<Op25GetResponse, ApiError>;


    async fn op26_get(&mut self) -> Result<Op26GetResponse, ApiError>;


    async fn op27_get(&mut self) -> Result<Op27GetResponse, ApiError>;


    async fn op28_get(&mut self) -> Result<Op28GetResponse, ApiError>;


    async fn op29_get(&mut self) -> Result<Op29GetResponse, ApiError>;


    async fn op2_get(&mut self) -> Result<Op2GetResponse, ApiError>;


    async fn op30_get(&mut self) -> Result<Op30GetResponse, ApiError>;


    async fn op31_get(&mut self) -> Result<Op31GetResponse, ApiError>;


    async fn op32_get(&mut self) -> Result<Op32GetResponse, ApiError>;


    async fn op33_get(&mut self) -> Result<Op33GetResponse, ApiError>;


    async fn op34_get(&mut self) -> Result<Op34GetResponse, ApiError>;


    async fn op35_get(&mut self) -> Result<Op35GetResponse, ApiError>;


    async fn op36_get(&mut self) -> Result<Op36GetResponse, ApiError>;


    async fn op37_get(&mut self) -> Result<Op37GetResponse, ApiError>;


    async fn op3_get(&mut self) -> Result<Op3GetResponse, ApiError>;


    async fn op4_get(&mut self) -> Result<Op4GetResponse, ApiError>;


    async fn op5_get(&mut self) -> Result<Op5GetResponse, ApiError>;


    async fn op6_get(&mut self) -> Result<Op6GetResponse, ApiError>;


    async fn op7_get(&mut self) -> Result<Op7GetResponse, ApiError>;


    async fn op8_get(&mut self) -> Result<Op8GetResponse, ApiError>;


    async fn op9_get(&mut self) -> Result<Op9GetResponse, ApiError>;

}

/// Trait to extend an API to make it easy to bind it to a context.
pub trait ContextWrapperExt<C> where Self: Sized {
    /// Binds this API to a context.
    fn with_context(self, context: C) -> ContextWrapper<Self, C>;
}

impl<T: Api<C> + Sized, C> ContextWrapperExt<C> for T {
    fn with_context(self, context: C) -> ContextWrapper<T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

#[async_trait]
impl<T: Api<C>, C> ApiNoContext for ContextWrapper<T, C>
    where C: Clone + Send + Sync,
          T: Send + Sync,
{


    async fn op10_get(&mut self) -> Result<Op10GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op10_get(&ctx).await
    }


    async fn op11_get(&mut self) -> Result<Op11GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op11_get(&ctx).await
    }


    async fn op12_get(&mut self) -> Result<Op12GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op12_get(&ctx).await
    }


    async fn op13_get(&mut self) -> Result<Op13GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op13_get(&ctx).await
    }


    async fn op14_get(&mut self) -> Result<Op14GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op14_get(&ctx).await
    }


    async fn op15_get(&mut self) -> Result<Op15GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op15_get(&ctx).await
    }


    async fn op16_get(&mut self) -> Result<Op16GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op16_get(&ctx).await
    }


    async fn op17_get(&mut self) -> Result<Op17GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op17_get(&ctx).await
    }


    async fn op18_get(&mut self) -> Result<Op18GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op18_get(&ctx).await
    }


    async fn op19_get(&mut self) -> Result<Op19GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op19_get(&ctx).await
    }


    async fn op1_get(&mut self) -> Result<Op1GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op1_get(&ctx).await
    }


    async fn op20_get(&mut self) -> Result<Op20GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op20_get(&ctx).await
    }


    async fn op21_get(&mut self) -> Result<Op21GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op21_get(&ctx).await
    }


    async fn op22_get(&mut self) -> Result<Op22GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op22_get(&ctx).await
    }


    async fn op23_get(&mut self) -> Result<Op23GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op23_get(&ctx).await
    }


    async fn op24_get(&mut self) -> Result<Op24GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op24_get(&ctx).await
    }


    async fn op25_get(&mut self) -> Result<Op25GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op25_get(&ctx).await
    }


    async fn op26_get(&mut self) -> Result<Op26GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op26_get(&ctx).await
    }


    async fn op27_get(&mut self) -> Result<Op27GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op27_get(&ctx).await
    }


    async fn op28_get(&mut self) -> Result<Op28GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op28_get(&ctx).await
    }


    async fn op29_get(&mut self) -> Result<Op29GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op29_get(&ctx).await
    }


    async fn op2_get(&mut self) -> Result<Op2GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op2_get(&ctx).await
    }


    async fn op30_get(&mut self) -> Result<Op30GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op30_get(&ctx).await
    }


    async fn op31_get(&mut self) -> Result<Op31GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op31_get(&ctx).await
    }


    async fn op32_get(&mut self) -> Result<Op32GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op32_get(&ctx).await
    }


    async fn op33_get(&mut self) -> Result<Op33GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op33_get(&ctx).await
    }


    async fn op34_get(&mut self) -> Result<Op34GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op34_get(&ctx).await
    }


    async fn op35_get(&mut self) -> Result<Op35GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op35_get(&ctx).await
    }


    async fn op36_get(&mut self) -> Result<Op36GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op36_get(&ctx).await
    }


    async fn op37_get(&mut self) -> Result<Op37GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op37_get(&ctx).await
    }


    async fn op3_get(&mut self) -> Result<Op3GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op3_get(&ctx).await
    }


    async fn op4_get(&mut self) -> Result<Op4GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op4_get(&ctx).await
    }


    async fn op5_get(&mut self) -> Result<Op5GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op5_get(&ctx).await
    }


    async fn op6_get(&mut self) -> Result<Op6GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op6_get(&ctx).await
    }


    async fn op7_get(&mut self) -> Result<Op7GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op7_get(&ctx).await
    }


    async fn op8_get(&mut self) -> Result<Op8GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op8_get(&ctx).await
    }


    async fn op9_get(&mut self) -> Result<Op9GetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().op9_get(&ctx).await
    }

}

#[cfg(feature = "client")]
pub mod client;

// Re-export Client as a top-level name
#[cfg(feature = "client")]
pub use self::client::Client;

#[cfg(feature = "server")]
pub mod server;

// Re-export router() as a top-level name
#[cfg(feature = "server")]
pub use self::server::Service;

pub mod models;
#[allow(non_upper_case_globals)]
pub mod headers;
