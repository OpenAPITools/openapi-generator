package mock

import (
	"context"
	"net/http"

	sw "../go-petstore"
)

// MockPetApi is a mock of the PetApi interface
type MockPetApi struct {
}

// NewMockPetApi creates a new mock instance
func NewMockPetApi() *MockPetApi {
	return &MockPetApi{}
}

func (m *MockPetApi) AddPet(ctx context.Context) sw.ApiAddPetRequest {
	return sw.ApiAddPetRequest{ApiService: m}
}

func (m *MockPetApi) AddPetExecute(r sw.ApiAddPetRequest) (*http.Response, error) {
	return &http.Response{StatusCode:200}, nil
}

func (m *MockPetApi) DeletePet(ctx context.Context, petId int64) sw.ApiDeletePetRequest {
	return sw.ApiDeletePetRequest{ApiService: m}
}

func (m *MockPetApi) DeletePetExecute(r sw.ApiDeletePetRequest) (*http.Response, error) {
	return &http.Response{StatusCode:200}, nil
}

func (m *MockPetApi) FindPetsByStatus(ctx context.Context) sw.ApiFindPetsByStatusRequest {
	return sw.ApiFindPetsByStatusRequest{ApiService: m}
}

func (m *MockPetApi) FindPetsByStatusExecute(r sw.ApiFindPetsByStatusRequest) ([]sw.Pet, *http.Response, error) {
	return []sw.Pet{}, &http.Response{StatusCode:200}, nil
}

func (m *MockPetApi) FindPetsByTags(ctx context.Context) sw.ApiFindPetsByTagsRequest {
	return sw.ApiFindPetsByTagsRequest{ApiService: m}
}

func (m *MockPetApi) FindPetsByTagsExecute(r sw.ApiFindPetsByTagsRequest) ([]sw.Pet, *http.Response, error) {
	return []sw.Pet{}, &http.Response{StatusCode:200}, nil
}

func (m *MockPetApi) GetPetById(ctx context.Context, petId int64) sw.ApiGetPetByIdRequest {
	return sw.ApiGetPetByIdRequest{ApiService: m}
}

func (m *MockPetApi) GetPetByIdExecute(r sw.ApiGetPetByIdRequest) (sw.Pet, *http.Response, error) {
	return sw.Pet{}, &http.Response{StatusCode:200}, nil
}

func (m *MockPetApi) UpdatePet(ctx context.Context) sw.ApiUpdatePetRequest {
	return sw.ApiUpdatePetRequest{ApiService: m}
}

func (m *MockPetApi) UpdatePetExecute(r sw.ApiUpdatePetRequest) (*http.Response, error) {
	return &http.Response{StatusCode:200}, nil
}

func (m *MockPetApi) UpdatePetWithForm(ctx context.Context, petId int64) sw.ApiUpdatePetWithFormRequest {
	return sw.ApiUpdatePetWithFormRequest{ApiService: m}
}

func (m *MockPetApi) UpdatePetWithFormExecute(r sw.ApiUpdatePetWithFormRequest) (*http.Response, error) {
	return &http.Response{StatusCode:200}, nil
}

func (m *MockPetApi) UploadFile(ctx context.Context, petId int64) sw.ApiUploadFileRequest {
	return sw.ApiUploadFileRequest{ApiService: m}
}

func (m *MockPetApi) UploadFileExecute(r sw.ApiUploadFileRequest) (sw.ApiResponse, *http.Response, error) {
	return sw.ApiResponse{}, &http.Response{StatusCode:200}, nil
}

func (m *MockPetApi) UploadFileWithRequiredFile(ctx context.Context, petId int64) sw.ApiUploadFileWithRequiredFileRequest {
	return sw.ApiUploadFileWithRequiredFileRequest{ApiService: m}
}

func (m *MockPetApi) UploadFileWithRequiredFileExecute(r sw.ApiUploadFileWithRequiredFileRequest) (sw.ApiResponse, *http.Response, error) {
	return sw.ApiResponse{}, &http.Response{StatusCode:200}, nil
}
