//! Server implementation of ops_v3.

#![allow(unused_imports)]

use futures::{self, Future};
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;
use swagger;
use swagger::{Has, XSpanIdString};

use ops_v3::{Api, ApiError,
                      Op10GetResponse,
                      Op11GetResponse,
                      Op12GetResponse,
                      Op13GetResponse,
                      Op14GetResponse,
                      Op15GetResponse,
                      Op16GetResponse,
                      Op17GetResponse,
                      Op18GetResponse,
                      Op19GetResponse,
                      Op1GetResponse,
                      Op20GetResponse,
                      Op21GetResponse,
                      Op22GetResponse,
                      Op23GetResponse,
                      Op24GetResponse,
                      Op25GetResponse,
                      Op26GetResponse,
                      Op27GetResponse,
                      Op28GetResponse,
                      Op29GetResponse,
                      Op2GetResponse,
                      Op30GetResponse,
                      Op31GetResponse,
                      Op32GetResponse,
                      Op33GetResponse,
                      Op34GetResponse,
                      Op35GetResponse,
                      Op36GetResponse,
                      Op37GetResponse,
                      Op3GetResponse,
                      Op4GetResponse,
                      Op5GetResponse,
                      Op6GetResponse,
                      Op7GetResponse,
                      Op8GetResponse,
                      Op9GetResponse
};
use ops_v3::models;

#[derive(Copy, Clone)]
pub struct Server<C> {
    marker: PhantomData<C>,
}

impl<C> Server<C> {
    pub fn new() -> Self {
        Server{marker: PhantomData}
    }
}

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{


    fn op10_get(&self, context: &C) -> Box<Future<Item=Op10GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op10_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op11_get(&self, context: &C) -> Box<Future<Item=Op11GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op11_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op12_get(&self, context: &C) -> Box<Future<Item=Op12GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op12_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op13_get(&self, context: &C) -> Box<Future<Item=Op13GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op13_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op14_get(&self, context: &C) -> Box<Future<Item=Op14GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op14_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op15_get(&self, context: &C) -> Box<Future<Item=Op15GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op15_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op16_get(&self, context: &C) -> Box<Future<Item=Op16GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op16_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op17_get(&self, context: &C) -> Box<Future<Item=Op17GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op17_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op18_get(&self, context: &C) -> Box<Future<Item=Op18GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op18_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op19_get(&self, context: &C) -> Box<Future<Item=Op19GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op19_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op1_get(&self, context: &C) -> Box<Future<Item=Op1GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op1_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op20_get(&self, context: &C) -> Box<Future<Item=Op20GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op20_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op21_get(&self, context: &C) -> Box<Future<Item=Op21GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op21_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op22_get(&self, context: &C) -> Box<Future<Item=Op22GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op22_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op23_get(&self, context: &C) -> Box<Future<Item=Op23GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op23_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op24_get(&self, context: &C) -> Box<Future<Item=Op24GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op24_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op25_get(&self, context: &C) -> Box<Future<Item=Op25GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op25_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op26_get(&self, context: &C) -> Box<Future<Item=Op26GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op26_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op27_get(&self, context: &C) -> Box<Future<Item=Op27GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op27_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op28_get(&self, context: &C) -> Box<Future<Item=Op28GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op28_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op29_get(&self, context: &C) -> Box<Future<Item=Op29GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op29_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op2_get(&self, context: &C) -> Box<Future<Item=Op2GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op2_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op30_get(&self, context: &C) -> Box<Future<Item=Op30GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op30_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op31_get(&self, context: &C) -> Box<Future<Item=Op31GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op31_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op32_get(&self, context: &C) -> Box<Future<Item=Op32GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op32_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op33_get(&self, context: &C) -> Box<Future<Item=Op33GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op33_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op34_get(&self, context: &C) -> Box<Future<Item=Op34GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op34_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op35_get(&self, context: &C) -> Box<Future<Item=Op35GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op35_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op36_get(&self, context: &C) -> Box<Future<Item=Op36GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op36_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op37_get(&self, context: &C) -> Box<Future<Item=Op37GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op37_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op3_get(&self, context: &C) -> Box<Future<Item=Op3GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op3_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op4_get(&self, context: &C) -> Box<Future<Item=Op4GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op4_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op5_get(&self, context: &C) -> Box<Future<Item=Op5GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op5_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op6_get(&self, context: &C) -> Box<Future<Item=Op6GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op6_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op7_get(&self, context: &C) -> Box<Future<Item=Op7GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op7_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op8_get(&self, context: &C) -> Box<Future<Item=Op8GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op8_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn op9_get(&self, context: &C) -> Box<Future<Item=Op9GetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("op9_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

}
