// Wire-level verification harness for OpenAPI 3.2 support in the generated
// `rust` (reqwest, blocking) client. Copied into the generated crate as
// src/bin/capture.rs by RustClientCodegenTest and executed with `cargo run`.
//
// A raw TCP listener records the HTTP request line for every generated call:
// standard methods must keep `reqwest::Method::GET` etc., while
// `query`/additionalOperations methods and `in: querystring` parameters must
// reach the wire verbatim (no upper-casing, no re-encoding).
use openapi::apis::{configuration, default_api};
use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::sync::mpsc;
use std::time::Duration;

fn main() {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind capture server");
    let port = listener.local_addr().unwrap().port();
    let (tx, rx) = mpsc::channel::<String>();
    std::thread::spawn(move || {
        for stream in listener.incoming() {
            let stream = match stream {
                Ok(s) => s,
                Err(_) => break,
            };
            let mut reader = BufReader::new(stream.try_clone().unwrap());
            let mut request_line = String::new();
            if reader.read_line(&mut request_line).is_err() {
                continue;
            }
            // consume request headers (no body in this fixture)
            loop {
                let mut h = String::new();
                match reader.read_line(&mut h) {
                    Ok(0) | Err(_) => break,
                    _ => {
                        if h.trim().is_empty() {
                            break;
                        }
                    }
                }
            }
            let mut s = stream;
            let _ = s.write_all(
                b"HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n",
            );
            if tx.send(request_line.trim().to_string()).is_err() {
                break;
            }
        }
    });

    let mut conf = configuration::Configuration::default();
    conf.base_path = format!("http://127.0.0.1:{}", port);
    conf.user_agent = None;
    // hermetic: reqwest honors HTTP(S)_PROXY env vars, which would bypass the local listener
    conf.client = reqwest::blocking::Client::builder().no_proxy().build().unwrap();

    // `in: querystring` callers pass the query component without the leading `?`
    default_api::query_pets(&conf, "a=1&b=%20x").expect("query_pets");
    default_api::custom_pets(&conf).expect("custom_pets");
    default_api::check_fetch_pets(&conf).expect("check_fetch_pets");
    default_api::purge_pets(&conf).expect("purge_pets");
    default_api::list_pets(&conf).expect("list_pets");

    let mut got = Vec::new();
    for _ in 0..5 {
        got.push(rx.recv_timeout(Duration::from_secs(15)).expect("request captured"));
    }

    // `in: querystring` is appended verbatim with `?` added: `%20` not double-encoded
    assert_eq!(got[0], "QUERY /pets?a=1&b=%20x HTTP/1.1");
    // additionalOperations methods arrive verbatim, case preserved
    assert_eq!(got[1], "customMethod /pets HTTP/1.1");
    assert_eq!(got[2], "CHECK&FETCH /pets HTTP/1.1");
    assert_eq!(got[3], "PURGE /pets HTTP/1.1");
    // standard methods are unaffected
    assert_eq!(got[4], "GET /pets HTTP/1.1");

    println!("CAPTURE-PASS");
}
