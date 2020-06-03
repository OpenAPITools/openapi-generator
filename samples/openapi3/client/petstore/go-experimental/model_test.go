package main

import (
	"testing"
    "encoding/json"
	"github.com/stretchr/testify/assert"
	sw "./go-petstore"
)

func TestBanana(t *testing.T) {
    assert := assert.New(t)

    lengthCm := float32(2.3)
    lengthCm2 := float32(2.4)
    newBanana := (sw.Banana{LengthCm: &lengthCm})
    newBanana.LengthCm = &lengthCm2
    assert.Equal(newBanana.LengthCm, &lengthCm2, "Banana LengthCm should be equal")

    // test additioanl properties
    jsonBanana:= `{"fruits":["apple","peach"],"lengthCm":3.1}`
    jsonMap := make(map[string]interface{})
    json.Unmarshal([]byte(jsonBanana), &jsonMap)

    newBanana2 := (sw.Banana{})
    lengthCm3 := float32(3.1)
    json.Unmarshal([]byte(jsonBanana), &newBanana2)
    assert.Equal(newBanana2.LengthCm, &lengthCm3, "Banana2 LengthCm should be equal")
    assert.Equal(newBanana2.AdditionalProperties["fruits"], jsonMap["fruits"], "Banana2 AdditonalProperties should be equal")

    newBanana2Json, _ := json.Marshal(newBanana2)
    assert.Equal(string(newBanana2Json), jsonBanana, "Banana2 JSON string should be equal")

}

