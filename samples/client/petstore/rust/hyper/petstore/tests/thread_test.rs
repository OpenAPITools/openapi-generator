#[cfg(test)]
mod tests {
    use petstore_hyper::apis::client::APIClient;
    use petstore_hyper::apis::configuration::Configuration;
    use std::thread;

    #[test]
    fn test_client_is_send_for_threads() {
        let client = APIClient::new(Configuration::new());

        let handle = thread::spawn(move || {
            let _ = client
                .fake_api()
                .test_nullable_required_param("username", None, "any_type", None, None);
        });

        handle.join().expect("Thread panicked!");
    }
}
