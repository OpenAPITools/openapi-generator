extern crate petstore_reqwest;

use petstore_reqwest::apis::pet_api::{add_pet, get_pet_by_id};
use petstore_reqwest::apis::configuration;
use petstore_reqwest::models::{Pet};

#[test]
fn test_pet() {
    let config = configuration::Configuration::new();

    // create pet object
    let photo_urls = vec!["https://11".to_string(), "https://22".to_string()];
    let mut new_pet = Pet::new("Rust Pet".to_string(), photo_urls);
    new_pet.id = Option::Some(8787);

    // add pet
    let _add_result = add_pet(&config, new_pet);

    // get pet
    let pet_result = get_pet_by_id(&config, 8787);

    match pet_result {
        Ok(resp) => {
            /* Test code when multiple returns option is not set. */
            assert_eq!(resp.id, Option::Some(8787));
            assert_eq!(resp.name, "Rust Pet");
            assert_eq!(resp.photo_urls, vec!["https://11".to_string(), "https://22".to_string()]);
            /* Test code for multiple returns option.
            match resp.entity {
                Some(petstore_reqwest::apis::pet_api::GetPetByIdSuccess::Status200(pet)) => {
                    assert_eq!(pet.id, Option::Some(8787));
                    assert_eq!(pet.name, "Rust Pet");
                    assert_eq!(pet.photo_urls, vec!["https://11".to_string(), "https://22".to_string()]);
                },
                _ => {
                    panic!("Response should contain a pet entity");
                },
            };
            */
        },
        Err(error) => {
            println!("error: {:?}", error);
            panic!("Query should succeed");
        },
    };
}
