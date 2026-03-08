package openapi

import (
	"encoding/json"
	"os"
	"testing"

	openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func Test_openapi_DefaultAPIService(t *testing.T) {
	t.Run("Matching regex", func(t *testing.T) {
		filesNames := []string{"issue_20079_matching01.json", "issue_20079_matching02.json", "issue_20079_matching03.json", "issue_20079_matching04.json"}

		for _, fileName := range filesNames {
			data, errLoad := os.ReadFile(fileName)

			if errLoad != nil {
				t.Errorf("Cannot read test file resource %s: %s", fileName, errLoad.Error())
			}

			var importCode *openapiclient.ImportCode

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

			var importCode *openapiclient.ImportCode

			errParse := json.Unmarshal(data, &importCode)

			if errParse == nil {
				t.Errorf("The resource file %s that was expected non-matching, does", fileName)
			} else {
				t.Logf("Good. The resource file %s doesn't match for %s", fileName, errParse.Error())
			}
		}
	})

}
