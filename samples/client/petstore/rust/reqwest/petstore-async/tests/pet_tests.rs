extern crate petstore_reqwest_async;
//use petstore_reqwest_async::apis::PetApi;
//use petstore_reqwest_async::apis::PetApiClient;
use petstore_reqwest_async::apis::configuration;
//use petstore_reqwest::apis::PetApiUpdatePetWithFormParams;
use petstore_reqwest_async::models::{Pet};
use std::option::Option;

#[test]
fn test_pet() {
    let config = configuration::Configuration::new();
    //let pet_api_client = PetApiClient::new(Rc::new(config));

    // create pet object
    let photo_urls = vec!["https://11".to_string(), "https://22".to_string()];
    let mut new_pet = Pet::new("Rust Pet".to_string(), photo_urls);
    new_pet.id = Option::Some(8787);

    let new_pet_params = petstore_reqwest_async::apis::pet_api::AddPetParams {
        body: new_pet,
    };

    // add pet
    let _add_result = petstore_reqwest_async::apis::pet_api::add_pet(&config, new_pet_params);

    let get_pet_params = petstore_reqwest_async::apis::pet_api::GetPetByIdParams {
        pet_id: 8787,
    };

    // get pet
    let pet_result = petstore_reqwest_async::apis::pet_api::get_pet_by_id(&config, get_pet_params);

    // TODO Testing async functions requires some additionnal testing crates.
}
