
#include "ApiResponseTest.cpp"

#include "CategoryTest.cpp"

#include "OrderTest.cpp"

#include "PetTest.cpp"

#include "TagTest.cpp"

#include "UserTest.cpp"



void setUp(){}

void tearDown(){}

void runTests(){
    
    
    RUN_TEST(test_ApiResponse_code_is_assigned_from_json);
    
    
    RUN_TEST(test_ApiResponse_type_is_assigned_from_json);
    
    
    RUN_TEST(test_ApiResponse_message_is_assigned_from_json);
    
    
    
    RUN_TEST(test_Category_id_is_assigned_from_json);
    
    
    RUN_TEST(test_Category_name_is_assigned_from_json);
    
    
    
    RUN_TEST(test_Order_id_is_assigned_from_json);
    
    
    RUN_TEST(test_Order_petId_is_assigned_from_json);
    
    
    RUN_TEST(test_Order_quantity_is_assigned_from_json);
    
    
    RUN_TEST(test_Order_shipDate_is_assigned_from_json);
    
    
    RUN_TEST(test_Order_status_is_assigned_from_json);
    
    
    RUN_TEST(test_Order_complete_is_assigned_from_json);
    
    
    
    RUN_TEST(test_Pet_id_is_assigned_from_json);
    
    
    
    RUN_TEST(test_Pet_name_is_assigned_from_json);
    
    
    
    
    RUN_TEST(test_Pet_status_is_assigned_from_json);
    
    
    
    RUN_TEST(test_Tag_id_is_assigned_from_json);
    
    
    RUN_TEST(test_Tag_name_is_assigned_from_json);
    
    
    
    RUN_TEST(test_User_id_is_assigned_from_json);
    
    
    RUN_TEST(test_User_username_is_assigned_from_json);
    
    
    RUN_TEST(test_User_firstName_is_assigned_from_json);
    
    
    RUN_TEST(test_User_lastName_is_assigned_from_json);
    
    
    RUN_TEST(test_User_email_is_assigned_from_json);
    
    
    RUN_TEST(test_User_password_is_assigned_from_json);
    
    
    RUN_TEST(test_User_phone_is_assigned_from_json);
    
    
    RUN_TEST(test_User_userStatus_is_assigned_from_json);
    
    

    
    
    RUN_TEST(test_ApiResponse_code_is_converted_to_json);
    
    
    RUN_TEST(test_ApiResponse_type_is_converted_to_json);
    
    
    RUN_TEST(test_ApiResponse_message_is_converted_to_json);
    
    
    
    RUN_TEST(test_Category_id_is_converted_to_json);
    
    
    RUN_TEST(test_Category_name_is_converted_to_json);
    
    
    
    RUN_TEST(test_Order_id_is_converted_to_json);
    
    
    RUN_TEST(test_Order_petId_is_converted_to_json);
    
    
    RUN_TEST(test_Order_quantity_is_converted_to_json);
    
    
    RUN_TEST(test_Order_shipDate_is_converted_to_json);
    
    
    RUN_TEST(test_Order_status_is_converted_to_json);
    
    
    RUN_TEST(test_Order_complete_is_converted_to_json);
    
    
    
    RUN_TEST(test_Pet_id_is_converted_to_json);
    
    
    
    RUN_TEST(test_Pet_name_is_converted_to_json);
    
    
    
    
    RUN_TEST(test_Pet_status_is_converted_to_json);
    
    
    
    RUN_TEST(test_Tag_id_is_converted_to_json);
    
    
    RUN_TEST(test_Tag_name_is_converted_to_json);
    
    
    
    RUN_TEST(test_User_id_is_converted_to_json);
    
    
    RUN_TEST(test_User_username_is_converted_to_json);
    
    
    RUN_TEST(test_User_firstName_is_converted_to_json);
    
    
    RUN_TEST(test_User_lastName_is_converted_to_json);
    
    
    RUN_TEST(test_User_email_is_converted_to_json);
    
    
    RUN_TEST(test_User_password_is_converted_to_json);
    
    
    RUN_TEST(test_User_phone_is_converted_to_json);
    
    
    RUN_TEST(test_User_userStatus_is_converted_to_json);
    
    

    
}

int main(void) {
    UNITY_BEGIN();
    runTests();
    return UNITY_END();
}

void setup() {
    UNITY_BEGIN();
    runTests();
    UNITY_END();
}

void loop() {
    
}
