#!/usr/bin/env bats


export PETSTORE_CLI="petstore-cli"

export PETSTORE_HOST="http://petstore.swagger.io"

#
# Bash syntax check
#
@test "Generated script should pass Bash syntax check" {
    result="$(bash -n $PETSTORE_CLI)"
    [ "$result" -eq 0 ]
}

#
# Tests for parameter handling and validation
#
@test "addPet without host" {
    unset PETSTORE_HOST
    run bash $PETSTORE_CLI -ac xml -ct json \
        addPet id:=123321 name==lucky status==available
    [[ "$output" =~ "Error: No hostname provided!!!" ]]
}

@test "addPet without content type" {
    run bash $PETSTORE_CLI  -ac xml --host $PETSTORE_HOST \
        addPet id:=123321 name==lucky status==available
    [[ "$output" =~ "Error: Request's content-type not specified!" ]]
}

@test "addPet abbreviated content type" {
    run bash $PETSTORE_CLI -ct json -ac xml --host $PETSTORE_HOST \
        addPet id:=123321 name==lucky status==available --dry-run
    [[ "$output" =~ "Content-type: application/json" ]]
}

@test "addPet unabbreviated content type" {
    run bash $PETSTORE_CLI -ct userdefined/custom -ac xml --host $PETSTORE_HOST \
        addPet id:=123321 name==lucky status==available --dry-run
    [[ "$output" =~ "Content-type: userdefined/custom" ]]
}

@test "fakeOperation invalid operation name" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io fakeOperation"
    [[ "$output" =~ "Error: No operation specified!" ]]
}

@test "findPetsByStatus basic auth" {
    run bash \
        -c "bash $PETSTORE_CLI -u alice:secret --host http://petstore.swagger.io findPetsByStatus status=s1 --dry-run"
    [[ "$output" =~ "-u alice:secret" ]]
}

@test "findPetsByStatus api key" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io findPetsByStatus status=s1 api_key:1234 --dry-run"
    [[ "$output" =~ "-H \"api_key: 1234\""  ]]
}

@test "findPetsByStatus empty api key" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io findPetsByStatus status=s1 --dry-run"
    [[ ! "$output" =~ "-H \"api_key:" ]]
}

@test "findPetsByStatus has default cURL parameters" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io findPetsByStatus status=s1 --dry-run"
    [[ ! "$output" =~ " -Ss " ]]
}

@test "findPetsByStatus too few values" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io findPetsByStatus"
    [[ "$output" =~ "Error: Too few values" ]]
}

@test "findPetsByTags too few values" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io findPetsByTags"
    [[ "$output" =~ "Error: Too few values" ]]
}

@test "findPetsByStatus status with space" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io findPetsByStatus \
            status=available status=\"gone test\" --dry-run"
    [[ "$output" =~ "status=available,gone%20test" ]]
}

@test "findPetsByStatus collection csv" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io findPetsByTags \
            tags=TAG1 tags=TAG2 --dry-run"
    [[ "$output" =~ "tags=TAG1,TAG2" ]]
}

@test "findPetsByStatus collection csv with space and question mark" {
    run bash \
        -c "bash $PETSTORE_CLI --host http://petstore.swagger.io findPetsByTags \
            tags=TAG1 tags=\"TAG2 TEST\" tags=TAG3?TEST --dry-run"
    [[ "$output" =~ "tags=TAG1,TAG2%20TEST,TAG3%3FTEST" ]]
}

#
# Operations calling the service and checking result
#
@test "addPet from parameters" {
    run bash $PETSTORE_CLI -ct json -ac xml \
        addPet id:=123321 name==lucky status==available
    [[ "$output" =~ "<id>123321</id>" ]]
}

@test "addPet from pipe" {
    run bash \
      -c "echo '{\"id\": 37567, \"name\": \"lucky\", \"status\": \"available\"}' | \
            bash $PETSTORE_CLI -ct json -ac xml addPet -"
    [[ "$output" =~ "<id>37567</id>" ]]
}
