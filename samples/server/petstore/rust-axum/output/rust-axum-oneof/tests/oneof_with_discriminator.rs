use rust_axum_oneof::models::*;

#[test]
fn test_oneof_schema_with_discriminator() {
    let test0 = r#"{"op": "ignored", "d": {"welcome_message": "test0"}}"#;

    let test1 = r#"{"op": "Hello", "d": {"welcome_message": "test1"}}"#;
    let test2 = r#"{"op": "Greeting", "d": {"greet_message": "test2"}}"#;
    let test3 = r#"{"op": "Goodbye", "d": {"goodbye_message": "test3"}}"#;

    let test4 = Hello {
        op: "ignored".to_string(),
        d: HelloD {
            welcome_message: "test4".to_string(),
        },
    };

    let test5 = Greeting {
        op: "ignored".to_string(),
        d: GreetingD {
            greet_message: "test5".to_string(),
        },
    };

    let test6 = Goodbye {
        op: "ignored".to_string(),
        d: GoodbyeD {
            goodbye_message: "test6".to_string(),
        },
    };

    let test7 = Message::Hello(test4.clone().into());
    let test8 = Message::Greeting(test5.clone().into());
    let test9 = Message::Goodbye(test6.clone().into());

    let test10: Message = test4.clone().into();
    let test11: Message = test5.clone().into();
    let test12: Message = test6.clone().into();

    let test13 = r#"{"op":"Hello","d":{"welcome_message":"test4"}}"#;
    let test14 = r#"{"d":{"greet_message":"test5"},"op":"Greeting"}"#;
    let test15 = r#"{"op":"Goodbye","d":{"goodbye_message":"test6"}}"#;

    assert!(serde_json::from_str::<Message>(test0).is_err());

    assert!(serde_json::from_str::<Hello>(test0).is_ok());
    assert!(serde_json::from_str::<Greeting>(test0).is_err());
    assert!(serde_json::from_str::<Goodbye>(test0).is_err());

    assert!(serde_json::from_str::<Message>(test1).is_ok());
    assert!(serde_json::from_str::<Message>(test2).is_ok());
    assert!(serde_json::from_str::<Message>(test3).is_ok());

    assert!(serde_json::from_str::<Hello>(test1).is_ok());
    assert!(serde_json::from_str::<Greeting>(test2).is_ok());
    assert!(serde_json::from_str::<Message>(test3).is_ok());

    assert_eq!(
        serde_json::to_string(&test4).expect("Serialization error"),
        test13
    );
    assert_eq!(
        serde_json::to_string(&test5).expect("Serialization error"),
        test14
    );
    assert_eq!(
        serde_json::to_string(&test6).expect("Serialization error"),
        test15
    );

    assert_eq!(
        serde_json::to_string(&test7).expect("Serialization error"),
        test13
    );
    assert_eq!(
        serde_json::to_string(&test8).expect("Serialization error"),
        test14
    );
    assert_eq!(
        serde_json::to_string(&test9).expect("Serialization error"),
        test15
    );

    assert_eq!(
        serde_json::to_string(&test10).expect("Serialization error"),
        test13
    );
    assert_eq!(
        serde_json::to_string(&test11).expect("Serialization error"),
        test14
    );
    assert_eq!(
        serde_json::to_string(&test12).expect("Serialization error"),
        test15
    );
}
