package openapi

import (
	"context"
	"encoding/json"
	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"os"
	"testing"
)

func Test_openapi_DefaultAPIService(t *testing.T) {

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)

	t.Run("Matching regex", func(t *testing.T) {
		filesNames := []string{"issue_20079_matching01.json", "issue_20079_matching02.json", "issue_20079_matching03.json", "issue_20079_matching04.json"}

		for _, fileName := range filesNames {
			data, errLoad := os.ReadFile(fileName)

			if errLoad != nil {
				t.Errorf("Cannot read test file resource %s: %s", fileName, errLoad.Error())
			}

			var importCode *openapiclient.FooGet200ResponseCode

			errParse := json.Unmarshal(data, &importCode)

			if errParse != nil {
				t.Errorf("The resource file %s that was expected matching, doesn't: %s", fileName, errParse.Error())
			}
		}
	})

	t.Run("Non-matching regex", func(t *testing.T) {
		filesNames := []string{"issue_20079_non_matching01.json", "issue_20079_non_matching02.json", "issue_20079_non_matching03.json", "issue_20079_non_matching04.json"}

		for _, fileName := range filesNames {
			data, errLoad := os.ReadFile(fileName)

			if errLoad != nil {
				t.Errorf("Cannot read test file resource %s: %s", fileName, errLoad.Error())
			}

			var importCode *openapiclient.FooGet200ResponseCode

			errParse := json.Unmarshal(data, &importCode)

			if errParse == nil {
				t.Errorf("The resource file %s that was expected non-matching, does", fileName)
			}
		}
	})

	t.Run("Test DefaultAPIService FooGet", func(t *testing.T) {

		t.Skip("skip test") // remove to run test

		resp, httpRes, err := apiClient.DefaultAPI.FooGet(context.Background()).Execute()

		require.Nil(t, err)
		require.NotNil(t, resp)
		assert.Equal(t, 200, httpRes.StatusCode)

	})

}
