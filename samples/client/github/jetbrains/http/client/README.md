# GitHub v3 REST API - Jetbrains API Client

## General API description

GitHub&#39;s v3 REST API.

* API basepath : [https://api.github.com](https://api.github.com)
* Version : 1.1.4

## Documentation for API Endpoints

All URIs are relative to *https://api.github.com*, but will link to the `.http` file that contains the endpoint definition.
There may be multiple requests for a single endpoint, one for each example described in the OpenAPI specification.

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*ActionsApi* | [**actionsAddCustomLabelsToSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actionsaddcustomlabelstoselfhostedrunnerfororg) | **POST** /orgs/{org}/actions/runners/{runner_id}/labels | Add custom labels to a self-hosted runner for an organization
*ActionsApi* | [**actionsAddCustomLabelsToSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actionsaddcustomlabelstoselfhostedrunnerforrepo) | **POST** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels | Add custom labels to a self-hosted runner for a repository
*ActionsApi* | [**actionsAddSelectedRepoToOrgSecret**](Apis/ActionsApi.http#actionsaddselectedrepotoorgsecret) | **PUT** /orgs/{org}/actions/secrets/{secret_name}/repositories/{repository_id} | Add selected repository to an organization secret
*ActionsApi* | [**actionsAddSelectedRepoToOrgVariable**](Apis/ActionsApi.http#actionsaddselectedrepotoorgvariable) | **PUT** /orgs/{org}/actions/variables/{name}/repositories/{repository_id} | Add selected repository to an organization variable
*ActionsApi* | [**actionsApproveWorkflowRun**](Apis/ActionsApi.http#actionsapproveworkflowrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/approve | Approve a workflow run for a fork pull request
*ActionsApi* | [**actionsCancelWorkflowRun**](Apis/ActionsApi.http#actionscancelworkflowrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/cancel | Cancel a workflow run
*ActionsApi* | [**actionsCreateEnvironmentVariable**](Apis/ActionsApi.http#actionscreateenvironmentvariable) | **POST** /repositories/{repository_id}/environments/{environment_name}/variables | Create an environment variable
*ActionsApi* | [**actionsCreateOrUpdateEnvironmentSecret**](Apis/ActionsApi.http#actionscreateorupdateenvironmentsecret) | **PUT** /repositories/{repository_id}/environments/{environment_name}/secrets/{secret_name} | Create or update an environment secret
*ActionsApi* | [**actionsCreateOrUpdateOrgSecret**](Apis/ActionsApi.http#actionscreateorupdateorgsecret) | **PUT** /orgs/{org}/actions/secrets/{secret_name} | Create or update an organization secret
*ActionsApi* | [**actionsCreateOrUpdateRepoSecret**](Apis/ActionsApi.http#actionscreateorupdatereposecret) | **PUT** /repos/{owner}/{repo}/actions/secrets/{secret_name} | Create or update a repository secret
*ActionsApi* | [**actionsCreateOrgVariable**](Apis/ActionsApi.http#actionscreateorgvariable) | **POST** /orgs/{org}/actions/variables | Create an organization variable
*ActionsApi* | [**actionsCreateRegistrationTokenForOrg**](Apis/ActionsApi.http#actionscreateregistrationtokenfororg) | **POST** /orgs/{org}/actions/runners/registration-token | Create a registration token for an organization
*ActionsApi* | [**actionsCreateRegistrationTokenForRepo**](Apis/ActionsApi.http#actionscreateregistrationtokenforrepo) | **POST** /repos/{owner}/{repo}/actions/runners/registration-token | Create a registration token for a repository
*ActionsApi* | [**actionsCreateRemoveTokenForOrg**](Apis/ActionsApi.http#actionscreateremovetokenfororg) | **POST** /orgs/{org}/actions/runners/remove-token | Create a remove token for an organization
*ActionsApi* | [**actionsCreateRemoveTokenForRepo**](Apis/ActionsApi.http#actionscreateremovetokenforrepo) | **POST** /repos/{owner}/{repo}/actions/runners/remove-token | Create a remove token for a repository
*ActionsApi* | [**actionsCreateRepoVariable**](Apis/ActionsApi.http#actionscreaterepovariable) | **POST** /repos/{owner}/{repo}/actions/variables | Create a repository variable
*ActionsApi* | [**actionsCreateWorkflowDispatch**](Apis/ActionsApi.http#actionscreateworkflowdispatch) | **POST** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/dispatches | Create a workflow dispatch event
*ActionsApi* | [**actionsDeleteActionsCacheById**](Apis/ActionsApi.http#actionsdeleteactionscachebyid) | **DELETE** /repos/{owner}/{repo}/actions/caches/{cache_id} | Delete a GitHub Actions cache for a repository (using a cache ID)
*ActionsApi* | [**actionsDeleteActionsCacheByKey**](Apis/ActionsApi.http#actionsdeleteactionscachebykey) | **DELETE** /repos/{owner}/{repo}/actions/caches | Delete GitHub Actions caches for a repository (using a cache key)
*ActionsApi* | [**actionsDeleteArtifact**](Apis/ActionsApi.http#actionsdeleteartifact) | **DELETE** /repos/{owner}/{repo}/actions/artifacts/{artifact_id} | Delete an artifact
*ActionsApi* | [**actionsDeleteEnvironmentSecret**](Apis/ActionsApi.http#actionsdeleteenvironmentsecret) | **DELETE** /repositories/{repository_id}/environments/{environment_name}/secrets/{secret_name} | Delete an environment secret
*ActionsApi* | [**actionsDeleteEnvironmentVariable**](Apis/ActionsApi.http#actionsdeleteenvironmentvariable) | **DELETE** /repositories/{repository_id}/environments/{environment_name}/variables/{name} | Delete an environment variable
*ActionsApi* | [**actionsDeleteOrgSecret**](Apis/ActionsApi.http#actionsdeleteorgsecret) | **DELETE** /orgs/{org}/actions/secrets/{secret_name} | Delete an organization secret
*ActionsApi* | [**actionsDeleteOrgVariable**](Apis/ActionsApi.http#actionsdeleteorgvariable) | **DELETE** /orgs/{org}/actions/variables/{name} | Delete an organization variable
*ActionsApi* | [**actionsDeleteRepoSecret**](Apis/ActionsApi.http#actionsdeletereposecret) | **DELETE** /repos/{owner}/{repo}/actions/secrets/{secret_name} | Delete a repository secret
*ActionsApi* | [**actionsDeleteRepoVariable**](Apis/ActionsApi.http#actionsdeleterepovariable) | **DELETE** /repos/{owner}/{repo}/actions/variables/{name} | Delete a repository variable
*ActionsApi* | [**actionsDeleteSelfHostedRunnerFromOrg**](Apis/ActionsApi.http#actionsdeleteselfhostedrunnerfromorg) | **DELETE** /orgs/{org}/actions/runners/{runner_id} | Delete a self-hosted runner from an organization
*ActionsApi* | [**actionsDeleteSelfHostedRunnerFromRepo**](Apis/ActionsApi.http#actionsdeleteselfhostedrunnerfromrepo) | **DELETE** /repos/{owner}/{repo}/actions/runners/{runner_id} | Delete a self-hosted runner from a repository
*ActionsApi* | [**actionsDeleteWorkflowRun**](Apis/ActionsApi.http#actionsdeleteworkflowrun) | **DELETE** /repos/{owner}/{repo}/actions/runs/{run_id} | Delete a workflow run
*ActionsApi* | [**actionsDeleteWorkflowRunLogs**](Apis/ActionsApi.http#actionsdeleteworkflowrunlogs) | **DELETE** /repos/{owner}/{repo}/actions/runs/{run_id}/logs | Delete workflow run logs
*ActionsApi* | [**actionsDisableSelectedRepositoryGithubActionsOrganization**](Apis/ActionsApi.http#actionsdisableselectedrepositorygithubactionsorganization) | **DELETE** /orgs/{org}/actions/permissions/repositories/{repository_id} | Disable a selected repository for GitHub Actions in an organization
*ActionsApi* | [**actionsDisableWorkflow**](Apis/ActionsApi.http#actionsdisableworkflow) | **PUT** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/disable | Disable a workflow
*ActionsApi* | [**actionsDownloadArtifact**](Apis/ActionsApi.http#actionsdownloadartifact) | **GET** /repos/{owner}/{repo}/actions/artifacts/{artifact_id}/{archive_format} | Download an artifact
*ActionsApi* | [**actionsDownloadJobLogsForWorkflowRun**](Apis/ActionsApi.http#actionsdownloadjoblogsforworkflowrun) | **GET** /repos/{owner}/{repo}/actions/jobs/{job_id}/logs | Download job logs for a workflow run
*ActionsApi* | [**actionsDownloadWorkflowRunAttemptLogs**](Apis/ActionsApi.http#actionsdownloadworkflowrunattemptlogs) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/attempts/{attempt_number}/logs | Download workflow run attempt logs
*ActionsApi* | [**actionsDownloadWorkflowRunLogs**](Apis/ActionsApi.http#actionsdownloadworkflowrunlogs) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/logs | Download workflow run logs
*ActionsApi* | [**actionsEnableSelectedRepositoryGithubActionsOrganization**](Apis/ActionsApi.http#actionsenableselectedrepositorygithubactionsorganization) | **PUT** /orgs/{org}/actions/permissions/repositories/{repository_id} | Enable a selected repository for GitHub Actions in an organization
*ActionsApi* | [**actionsEnableWorkflow**](Apis/ActionsApi.http#actionsenableworkflow) | **PUT** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/enable | Enable a workflow
*ActionsApi* | [**actionsForceCancelWorkflowRun**](Apis/ActionsApi.http#actionsforcecancelworkflowrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/force-cancel | Force cancel a workflow run
*ActionsApi* | [**actionsGenerateRunnerJitconfigForOrg**](Apis/ActionsApi.http#actionsgeneraterunnerjitconfigfororg) | **POST** /orgs/{org}/actions/runners/generate-jitconfig | Create configuration for a just-in-time runner for an organization
*ActionsApi* | [**actionsGenerateRunnerJitconfigForRepo**](Apis/ActionsApi.http#actionsgeneraterunnerjitconfigforrepo) | **POST** /repos/{owner}/{repo}/actions/runners/generate-jitconfig | Create configuration for a just-in-time runner for a repository
*ActionsApi* | [**actionsGetActionsCacheList**](Apis/ActionsApi.http#actionsgetactionscachelist) | **GET** /repos/{owner}/{repo}/actions/caches | List GitHub Actions caches for a repository
*ActionsApi* | [**actionsGetActionsCacheUsage**](Apis/ActionsApi.http#actionsgetactionscacheusage) | **GET** /repos/{owner}/{repo}/actions/cache/usage | Get GitHub Actions cache usage for a repository
*ActionsApi* | [**actionsGetActionsCacheUsageByRepoForOrg**](Apis/ActionsApi.http#actionsgetactionscacheusagebyrepofororg) | **GET** /orgs/{org}/actions/cache/usage-by-repository | List repositories with GitHub Actions cache usage for an organization
*ActionsApi* | [**actionsGetActionsCacheUsageForOrg**](Apis/ActionsApi.http#actionsgetactionscacheusagefororg) | **GET** /orgs/{org}/actions/cache/usage | Get GitHub Actions cache usage for an organization
*ActionsApi* | [**actionsGetAllowedActionsOrganization**](Apis/ActionsApi.http#actionsgetallowedactionsorganization) | **GET** /orgs/{org}/actions/permissions/selected-actions | Get allowed actions and reusable workflows for an organization
*ActionsApi* | [**actionsGetAllowedActionsRepository**](Apis/ActionsApi.http#actionsgetallowedactionsrepository) | **GET** /repos/{owner}/{repo}/actions/permissions/selected-actions | Get allowed actions and reusable workflows for a repository
*ActionsApi* | [**actionsGetArtifact**](Apis/ActionsApi.http#actionsgetartifact) | **GET** /repos/{owner}/{repo}/actions/artifacts/{artifact_id} | Get an artifact
*ActionsApi* | [**actionsGetCustomOidcSubClaimForRepo**](Apis/ActionsApi.http#actionsgetcustomoidcsubclaimforrepo) | **GET** /repos/{owner}/{repo}/actions/oidc/customization/sub | Get the customization template for an OIDC subject claim for a repository
*ActionsApi* | [**actionsGetEnvironmentPublicKey**](Apis/ActionsApi.http#actionsgetenvironmentpublickey) | **GET** /repositories/{repository_id}/environments/{environment_name}/secrets/public-key | Get an environment public key
*ActionsApi* | [**actionsGetEnvironmentSecret**](Apis/ActionsApi.http#actionsgetenvironmentsecret) | **GET** /repositories/{repository_id}/environments/{environment_name}/secrets/{secret_name} | Get an environment secret
*ActionsApi* | [**actionsGetEnvironmentVariable**](Apis/ActionsApi.http#actionsgetenvironmentvariable) | **GET** /repositories/{repository_id}/environments/{environment_name}/variables/{name} | Get an environment variable
*ActionsApi* | [**actionsGetGithubActionsDefaultWorkflowPermissionsOrganization**](Apis/ActionsApi.http#actionsgetgithubactionsdefaultworkflowpermissionsorganization) | **GET** /orgs/{org}/actions/permissions/workflow | Get default workflow permissions for an organization
*ActionsApi* | [**actionsGetGithubActionsDefaultWorkflowPermissionsRepository**](Apis/ActionsApi.http#actionsgetgithubactionsdefaultworkflowpermissionsrepository) | **GET** /repos/{owner}/{repo}/actions/permissions/workflow | Get default workflow permissions for a repository
*ActionsApi* | [**actionsGetGithubActionsPermissionsOrganization**](Apis/ActionsApi.http#actionsgetgithubactionspermissionsorganization) | **GET** /orgs/{org}/actions/permissions | Get GitHub Actions permissions for an organization
*ActionsApi* | [**actionsGetGithubActionsPermissionsRepository**](Apis/ActionsApi.http#actionsgetgithubactionspermissionsrepository) | **GET** /repos/{owner}/{repo}/actions/permissions | Get GitHub Actions permissions for a repository
*ActionsApi* | [**actionsGetJobForWorkflowRun**](Apis/ActionsApi.http#actionsgetjobforworkflowrun) | **GET** /repos/{owner}/{repo}/actions/jobs/{job_id} | Get a job for a workflow run
*ActionsApi* | [**actionsGetOrgPublicKey**](Apis/ActionsApi.http#actionsgetorgpublickey) | **GET** /orgs/{org}/actions/secrets/public-key | Get an organization public key
*ActionsApi* | [**actionsGetOrgSecret**](Apis/ActionsApi.http#actionsgetorgsecret) | **GET** /orgs/{org}/actions/secrets/{secret_name} | Get an organization secret
*ActionsApi* | [**actionsGetOrgVariable**](Apis/ActionsApi.http#actionsgetorgvariable) | **GET** /orgs/{org}/actions/variables/{name} | Get an organization variable
*ActionsApi* | [**actionsGetPendingDeploymentsForRun**](Apis/ActionsApi.http#actionsgetpendingdeploymentsforrun) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/pending_deployments | Get pending deployments for a workflow run
*ActionsApi* | [**actionsGetRepoPublicKey**](Apis/ActionsApi.http#actionsgetrepopublickey) | **GET** /repos/{owner}/{repo}/actions/secrets/public-key | Get a repository public key
*ActionsApi* | [**actionsGetRepoSecret**](Apis/ActionsApi.http#actionsgetreposecret) | **GET** /repos/{owner}/{repo}/actions/secrets/{secret_name} | Get a repository secret
*ActionsApi* | [**actionsGetRepoVariable**](Apis/ActionsApi.http#actionsgetrepovariable) | **GET** /repos/{owner}/{repo}/actions/variables/{name} | Get a repository variable
*ActionsApi* | [**actionsGetReviewsForRun**](Apis/ActionsApi.http#actionsgetreviewsforrun) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/approvals | Get the review history for a workflow run
*ActionsApi* | [**actionsGetSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actionsgetselfhostedrunnerfororg) | **GET** /orgs/{org}/actions/runners/{runner_id} | Get a self-hosted runner for an organization
*ActionsApi* | [**actionsGetSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actionsgetselfhostedrunnerforrepo) | **GET** /repos/{owner}/{repo}/actions/runners/{runner_id} | Get a self-hosted runner for a repository
*ActionsApi* | [**actionsGetWorkflow**](Apis/ActionsApi.http#actionsgetworkflow) | **GET** /repos/{owner}/{repo}/actions/workflows/{workflow_id} | Get a workflow
*ActionsApi* | [**actionsGetWorkflowAccessToRepository**](Apis/ActionsApi.http#actionsgetworkflowaccesstorepository) | **GET** /repos/{owner}/{repo}/actions/permissions/access | Get the level of access for workflows outside of the repository
*ActionsApi* | [**actionsGetWorkflowRun**](Apis/ActionsApi.http#actionsgetworkflowrun) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id} | Get a workflow run
*ActionsApi* | [**actionsGetWorkflowRunAttempt**](Apis/ActionsApi.http#actionsgetworkflowrunattempt) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/attempts/{attempt_number} | Get a workflow run attempt
*ActionsApi* | [**actionsGetWorkflowRunUsage**](Apis/ActionsApi.http#actionsgetworkflowrunusage) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/timing | Get workflow run usage
*ActionsApi* | [**actionsGetWorkflowUsage**](Apis/ActionsApi.http#actionsgetworkflowusage) | **GET** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/timing | Get workflow usage
*ActionsApi* | [**actionsListArtifactsForRepo**](Apis/ActionsApi.http#actionslistartifactsforrepo) | **GET** /repos/{owner}/{repo}/actions/artifacts | List artifacts for a repository
*ActionsApi* | [**actionsListEnvironmentSecrets**](Apis/ActionsApi.http#actionslistenvironmentsecrets) | **GET** /repositories/{repository_id}/environments/{environment_name}/secrets | List environment secrets
*ActionsApi* | [**actionsListEnvironmentVariables**](Apis/ActionsApi.http#actionslistenvironmentvariables) | **GET** /repositories/{repository_id}/environments/{environment_name}/variables | List environment variables
*ActionsApi* | [**actionsListJobsForWorkflowRun**](Apis/ActionsApi.http#actionslistjobsforworkflowrun) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/jobs | List jobs for a workflow run
*ActionsApi* | [**actionsListJobsForWorkflowRunAttempt**](Apis/ActionsApi.http#actionslistjobsforworkflowrunattempt) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/attempts/{attempt_number}/jobs | List jobs for a workflow run attempt
*ActionsApi* | [**actionsListLabelsForSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actionslistlabelsforselfhostedrunnerfororg) | **GET** /orgs/{org}/actions/runners/{runner_id}/labels | List labels for a self-hosted runner for an organization
*ActionsApi* | [**actionsListLabelsForSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actionslistlabelsforselfhostedrunnerforrepo) | **GET** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels | List labels for a self-hosted runner for a repository
*ActionsApi* | [**actionsListOrgSecrets**](Apis/ActionsApi.http#actionslistorgsecrets) | **GET** /orgs/{org}/actions/secrets | List organization secrets
*ActionsApi* | [**actionsListOrgVariables**](Apis/ActionsApi.http#actionslistorgvariables) | **GET** /orgs/{org}/actions/variables | List organization variables
*ActionsApi* | [**actionsListRepoOrganizationSecrets**](Apis/ActionsApi.http#actionslistrepoorganizationsecrets) | **GET** /repos/{owner}/{repo}/actions/organization-secrets | List repository organization secrets
*ActionsApi* | [**actionsListRepoOrganizationVariables**](Apis/ActionsApi.http#actionslistrepoorganizationvariables) | **GET** /repos/{owner}/{repo}/actions/organization-variables | List repository organization variables
*ActionsApi* | [**actionsListRepoSecrets**](Apis/ActionsApi.http#actionslistreposecrets) | **GET** /repos/{owner}/{repo}/actions/secrets | List repository secrets
*ActionsApi* | [**actionsListRepoVariables**](Apis/ActionsApi.http#actionslistrepovariables) | **GET** /repos/{owner}/{repo}/actions/variables | List repository variables
*ActionsApi* | [**actionsListRepoWorkflows**](Apis/ActionsApi.http#actionslistrepoworkflows) | **GET** /repos/{owner}/{repo}/actions/workflows | List repository workflows
*ActionsApi* | [**actionsListRunnerApplicationsForOrg**](Apis/ActionsApi.http#actionslistrunnerapplicationsfororg) | **GET** /orgs/{org}/actions/runners/downloads | List runner applications for an organization
*ActionsApi* | [**actionsListRunnerApplicationsForRepo**](Apis/ActionsApi.http#actionslistrunnerapplicationsforrepo) | **GET** /repos/{owner}/{repo}/actions/runners/downloads | List runner applications for a repository
*ActionsApi* | [**actionsListSelectedReposForOrgSecret**](Apis/ActionsApi.http#actionslistselectedreposfororgsecret) | **GET** /orgs/{org}/actions/secrets/{secret_name}/repositories | List selected repositories for an organization secret
*ActionsApi* | [**actionsListSelectedReposForOrgVariable**](Apis/ActionsApi.http#actionslistselectedreposfororgvariable) | **GET** /orgs/{org}/actions/variables/{name}/repositories | List selected repositories for an organization variable
*ActionsApi* | [**actionsListSelectedRepositoriesEnabledGithubActionsOrganization**](Apis/ActionsApi.http#actionslistselectedrepositoriesenabledgithubactionsorganization) | **GET** /orgs/{org}/actions/permissions/repositories | List selected repositories enabled for GitHub Actions in an organization
*ActionsApi* | [**actionsListSelfHostedRunnersForOrg**](Apis/ActionsApi.http#actionslistselfhostedrunnersfororg) | **GET** /orgs/{org}/actions/runners | List self-hosted runners for an organization
*ActionsApi* | [**actionsListSelfHostedRunnersForRepo**](Apis/ActionsApi.http#actionslistselfhostedrunnersforrepo) | **GET** /repos/{owner}/{repo}/actions/runners | List self-hosted runners for a repository
*ActionsApi* | [**actionsListWorkflowRunArtifacts**](Apis/ActionsApi.http#actionslistworkflowrunartifacts) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/artifacts | List workflow run artifacts
*ActionsApi* | [**actionsListWorkflowRuns**](Apis/ActionsApi.http#actionslistworkflowruns) | **GET** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs | List workflow runs for a workflow
*ActionsApi* | [**actionsListWorkflowRunsForRepo**](Apis/ActionsApi.http#actionslistworkflowrunsforrepo) | **GET** /repos/{owner}/{repo}/actions/runs | List workflow runs for a repository
*ActionsApi* | [**actionsReRunJobForWorkflowRun**](Apis/ActionsApi.http#actionsrerunjobforworkflowrun) | **POST** /repos/{owner}/{repo}/actions/jobs/{job_id}/rerun | Re-run a job from a workflow run
*ActionsApi* | [**actionsReRunWorkflow**](Apis/ActionsApi.http#actionsrerunworkflow) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/rerun | Re-run a workflow
*ActionsApi* | [**actionsReRunWorkflowFailedJobs**](Apis/ActionsApi.http#actionsrerunworkflowfailedjobs) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/rerun-failed-jobs | Re-run failed jobs from a workflow run
*ActionsApi* | [**actionsRemoveAllCustomLabelsFromSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actionsremoveallcustomlabelsfromselfhostedrunnerfororg) | **DELETE** /orgs/{org}/actions/runners/{runner_id}/labels | Remove all custom labels from a self-hosted runner for an organization
*ActionsApi* | [**actionsRemoveAllCustomLabelsFromSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actionsremoveallcustomlabelsfromselfhostedrunnerforrepo) | **DELETE** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels | Remove all custom labels from a self-hosted runner for a repository
*ActionsApi* | [**actionsRemoveCustomLabelFromSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actionsremovecustomlabelfromselfhostedrunnerfororg) | **DELETE** /orgs/{org}/actions/runners/{runner_id}/labels/{name} | Remove a custom label from a self-hosted runner for an organization
*ActionsApi* | [**actionsRemoveCustomLabelFromSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actionsremovecustomlabelfromselfhostedrunnerforrepo) | **DELETE** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels/{name} | Remove a custom label from a self-hosted runner for a repository
*ActionsApi* | [**actionsRemoveSelectedRepoFromOrgSecret**](Apis/ActionsApi.http#actionsremoveselectedrepofromorgsecret) | **DELETE** /orgs/{org}/actions/secrets/{secret_name}/repositories/{repository_id} | Remove selected repository from an organization secret
*ActionsApi* | [**actionsRemoveSelectedRepoFromOrgVariable**](Apis/ActionsApi.http#actionsremoveselectedrepofromorgvariable) | **DELETE** /orgs/{org}/actions/variables/{name}/repositories/{repository_id} | Remove selected repository from an organization variable
*ActionsApi* | [**actionsReviewCustomGatesForRun**](Apis/ActionsApi.http#actionsreviewcustomgatesforrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/deployment_protection_rule | Review custom deployment protection rules for a workflow run
*ActionsApi* | [**actionsReviewPendingDeploymentsForRun**](Apis/ActionsApi.http#actionsreviewpendingdeploymentsforrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/pending_deployments | Review pending deployments for a workflow run
*ActionsApi* | [**actionsSetAllowedActionsOrganization**](Apis/ActionsApi.http#actionssetallowedactionsorganization) | **PUT** /orgs/{org}/actions/permissions/selected-actions | Set allowed actions and reusable workflows for an organization
*ActionsApi* | [**actionsSetAllowedActionsRepository**](Apis/ActionsApi.http#actionssetallowedactionsrepository) | **PUT** /repos/{owner}/{repo}/actions/permissions/selected-actions | Set allowed actions and reusable workflows for a repository
*ActionsApi* | [**actionsSetCustomLabelsForSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actionssetcustomlabelsforselfhostedrunnerfororg) | **PUT** /orgs/{org}/actions/runners/{runner_id}/labels | Set custom labels for a self-hosted runner for an organization
*ActionsApi* | [**actionsSetCustomLabelsForSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actionssetcustomlabelsforselfhostedrunnerforrepo) | **PUT** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels | Set custom labels for a self-hosted runner for a repository
*ActionsApi* | [**actionsSetCustomOidcSubClaimForRepo**](Apis/ActionsApi.http#actionssetcustomoidcsubclaimforrepo) | **PUT** /repos/{owner}/{repo}/actions/oidc/customization/sub | Set the customization template for an OIDC subject claim for a repository
*ActionsApi* | [**actionsSetGithubActionsDefaultWorkflowPermissionsOrganization**](Apis/ActionsApi.http#actionssetgithubactionsdefaultworkflowpermissionsorganization) | **PUT** /orgs/{org}/actions/permissions/workflow | Set default workflow permissions for an organization
*ActionsApi* | [**actionsSetGithubActionsDefaultWorkflowPermissionsRepository**](Apis/ActionsApi.http#actionssetgithubactionsdefaultworkflowpermissionsrepository) | **PUT** /repos/{owner}/{repo}/actions/permissions/workflow | Set default workflow permissions for a repository
*ActionsApi* | [**actionsSetGithubActionsPermissionsOrganization**](Apis/ActionsApi.http#actionssetgithubactionspermissionsorganization) | **PUT** /orgs/{org}/actions/permissions | Set GitHub Actions permissions for an organization
*ActionsApi* | [**actionsSetGithubActionsPermissionsRepository**](Apis/ActionsApi.http#actionssetgithubactionspermissionsrepository) | **PUT** /repos/{owner}/{repo}/actions/permissions | Set GitHub Actions permissions for a repository
*ActionsApi* | [**actionsSetSelectedReposForOrgSecret**](Apis/ActionsApi.http#actionssetselectedreposfororgsecret) | **PUT** /orgs/{org}/actions/secrets/{secret_name}/repositories | Set selected repositories for an organization secret
*ActionsApi* | [**actionsSetSelectedReposForOrgVariable**](Apis/ActionsApi.http#actionssetselectedreposfororgvariable) | **PUT** /orgs/{org}/actions/variables/{name}/repositories | Set selected repositories for an organization variable
*ActionsApi* | [**actionsSetSelectedRepositoriesEnabledGithubActionsOrganization**](Apis/ActionsApi.http#actionssetselectedrepositoriesenabledgithubactionsorganization) | **PUT** /orgs/{org}/actions/permissions/repositories | Set selected repositories enabled for GitHub Actions in an organization
*ActionsApi* | [**actionsSetWorkflowAccessToRepository**](Apis/ActionsApi.http#actionssetworkflowaccesstorepository) | **PUT** /repos/{owner}/{repo}/actions/permissions/access | Set the level of access for workflows outside of the repository
*ActionsApi* | [**actionsUpdateEnvironmentVariable**](Apis/ActionsApi.http#actionsupdateenvironmentvariable) | **PATCH** /repositories/{repository_id}/environments/{environment_name}/variables/{name} | Update an environment variable
*ActionsApi* | [**actionsUpdateOrgVariable**](Apis/ActionsApi.http#actionsupdateorgvariable) | **PATCH** /orgs/{org}/actions/variables/{name} | Update an organization variable
*ActionsApi* | [**actionsUpdateRepoVariable**](Apis/ActionsApi.http#actionsupdaterepovariable) | **PATCH** /repos/{owner}/{repo}/actions/variables/{name} | Update a repository variable
*ActivityApi* | [**activityCheckRepoIsStarredByAuthenticatedUser**](Apis/ActivityApi.http#activitycheckrepoisstarredbyauthenticateduser) | **GET** /user/starred/{owner}/{repo} | Check if a repository is starred by the authenticated user
*ActivityApi* | [**activityDeleteRepoSubscription**](Apis/ActivityApi.http#activitydeletereposubscription) | **DELETE** /repos/{owner}/{repo}/subscription | Delete a repository subscription
*ActivityApi* | [**activityDeleteThreadSubscription**](Apis/ActivityApi.http#activitydeletethreadsubscription) | **DELETE** /notifications/threads/{thread_id}/subscription | Delete a thread subscription
*ActivityApi* | [**activityGetFeeds**](Apis/ActivityApi.http#activitygetfeeds) | **GET** /feeds | Get feeds
*ActivityApi* | [**activityGetRepoSubscription**](Apis/ActivityApi.http#activitygetreposubscription) | **GET** /repos/{owner}/{repo}/subscription | Get a repository subscription
*ActivityApi* | [**activityGetThread**](Apis/ActivityApi.http#activitygetthread) | **GET** /notifications/threads/{thread_id} | Get a thread
*ActivityApi* | [**activityGetThreadSubscriptionForAuthenticatedUser**](Apis/ActivityApi.http#activitygetthreadsubscriptionforauthenticateduser) | **GET** /notifications/threads/{thread_id}/subscription | Get a thread subscription for the authenticated user
*ActivityApi* | [**activityListEventsForAuthenticatedUser**](Apis/ActivityApi.http#activitylisteventsforauthenticateduser) | **GET** /users/{username}/events | List events for the authenticated user
*ActivityApi* | [**activityListNotificationsForAuthenticatedUser**](Apis/ActivityApi.http#activitylistnotificationsforauthenticateduser) | **GET** /notifications | List notifications for the authenticated user
*ActivityApi* | [**activityListOrgEventsForAuthenticatedUser**](Apis/ActivityApi.http#activitylistorgeventsforauthenticateduser) | **GET** /users/{username}/events/orgs/{org} | List organization events for the authenticated user
*ActivityApi* | [**activityListPublicEvents**](Apis/ActivityApi.http#activitylistpublicevents) | **GET** /events | List public events
*ActivityApi* | [**activityListPublicEventsForRepoNetwork**](Apis/ActivityApi.http#activitylistpubliceventsforreponetwork) | **GET** /networks/{owner}/{repo}/events | List public events for a network of repositories
*ActivityApi* | [**activityListPublicEventsForUser**](Apis/ActivityApi.http#activitylistpubliceventsforuser) | **GET** /users/{username}/events/public | List public events for a user
*ActivityApi* | [**activityListPublicOrgEvents**](Apis/ActivityApi.http#activitylistpublicorgevents) | **GET** /orgs/{org}/events | List public organization events
*ActivityApi* | [**activityListReceivedEventsForUser**](Apis/ActivityApi.http#activitylistreceivedeventsforuser) | **GET** /users/{username}/received_events | List events received by the authenticated user
*ActivityApi* | [**activityListReceivedPublicEventsForUser**](Apis/ActivityApi.http#activitylistreceivedpubliceventsforuser) | **GET** /users/{username}/received_events/public | List public events received by a user
*ActivityApi* | [**activityListRepoEvents**](Apis/ActivityApi.http#activitylistrepoevents) | **GET** /repos/{owner}/{repo}/events | List repository events
*ActivityApi* | [**activityListRepoNotificationsForAuthenticatedUser**](Apis/ActivityApi.http#activitylistreponotificationsforauthenticateduser) | **GET** /repos/{owner}/{repo}/notifications | List repository notifications for the authenticated user
*ActivityApi* | [**activityListReposStarredByAuthenticatedUser**](Apis/ActivityApi.http#activitylistreposstarredbyauthenticateduser) | **GET** /user/starred | List repositories starred by the authenticated user
*ActivityApi* | [**activityListReposStarredByUser**](Apis/ActivityApi.http#activitylistreposstarredbyuser) | **GET** /users/{username}/starred | List repositories starred by a user
*ActivityApi* | [**activityListReposWatchedByUser**](Apis/ActivityApi.http#activitylistreposwatchedbyuser) | **GET** /users/{username}/subscriptions | List repositories watched by a user
*ActivityApi* | [**activityListStargazersForRepo**](Apis/ActivityApi.http#activityliststargazersforrepo) | **GET** /repos/{owner}/{repo}/stargazers | List stargazers
*ActivityApi* | [**activityListWatchedReposForAuthenticatedUser**](Apis/ActivityApi.http#activitylistwatchedreposforauthenticateduser) | **GET** /user/subscriptions | List repositories watched by the authenticated user
*ActivityApi* | [**activityListWatchersForRepo**](Apis/ActivityApi.http#activitylistwatchersforrepo) | **GET** /repos/{owner}/{repo}/subscribers | List watchers
*ActivityApi* | [**activityMarkNotificationsAsRead**](Apis/ActivityApi.http#activitymarknotificationsasread) | **PUT** /notifications | Mark notifications as read
*ActivityApi* | [**activityMarkRepoNotificationsAsRead**](Apis/ActivityApi.http#activitymarkreponotificationsasread) | **PUT** /repos/{owner}/{repo}/notifications | Mark repository notifications as read
*ActivityApi* | [**activityMarkThreadAsDone**](Apis/ActivityApi.http#activitymarkthreadasdone) | **DELETE** /notifications/threads/{thread_id} | Mark a thread as done
*ActivityApi* | [**activityMarkThreadAsRead**](Apis/ActivityApi.http#activitymarkthreadasread) | **PATCH** /notifications/threads/{thread_id} | Mark a thread as read
*ActivityApi* | [**activitySetRepoSubscription**](Apis/ActivityApi.http#activitysetreposubscription) | **PUT** /repos/{owner}/{repo}/subscription | Set a repository subscription
*ActivityApi* | [**activitySetThreadSubscription**](Apis/ActivityApi.http#activitysetthreadsubscription) | **PUT** /notifications/threads/{thread_id}/subscription | Set a thread subscription
*ActivityApi* | [**activityStarRepoForAuthenticatedUser**](Apis/ActivityApi.http#activitystarrepoforauthenticateduser) | **PUT** /user/starred/{owner}/{repo} | Star a repository for the authenticated user
*ActivityApi* | [**activityUnstarRepoForAuthenticatedUser**](Apis/ActivityApi.http#activityunstarrepoforauthenticateduser) | **DELETE** /user/starred/{owner}/{repo} | Unstar a repository for the authenticated user
*AppsApi* | [**appsAddRepoToInstallationForAuthenticatedUser**](Apis/AppsApi.http#appsaddrepotoinstallationforauthenticateduser) | **PUT** /user/installations/{installation_id}/repositories/{repository_id} | Add a repository to an app installation
*AppsApi* | [**appsCheckToken**](Apis/AppsApi.http#appschecktoken) | **POST** /applications/{client_id}/token | Check a token
*AppsApi* | [**appsCreateFromManifest**](Apis/AppsApi.http#appscreatefrommanifest) | **POST** /app-manifests/{code}/conversions | Create a GitHub App from a manifest
*AppsApi* | [**appsCreateInstallationAccessToken**](Apis/AppsApi.http#appscreateinstallationaccesstoken) | **POST** /app/installations/{installation_id}/access_tokens | Create an installation access token for an app
*AppsApi* | [**appsDeleteAuthorization**](Apis/AppsApi.http#appsdeleteauthorization) | **DELETE** /applications/{client_id}/grant | Delete an app authorization
*AppsApi* | [**appsDeleteInstallation**](Apis/AppsApi.http#appsdeleteinstallation) | **DELETE** /app/installations/{installation_id} | Delete an installation for the authenticated app
*AppsApi* | [**appsDeleteToken**](Apis/AppsApi.http#appsdeletetoken) | **DELETE** /applications/{client_id}/token | Delete an app token
*AppsApi* | [**appsGetAuthenticated**](Apis/AppsApi.http#appsgetauthenticated) | **GET** /app | Get the authenticated app
*AppsApi* | [**appsGetBySlug**](Apis/AppsApi.http#appsgetbyslug) | **GET** /apps/{app_slug} | Get an app
*AppsApi* | [**appsGetInstallation**](Apis/AppsApi.http#appsgetinstallation) | **GET** /app/installations/{installation_id} | Get an installation for the authenticated app
*AppsApi* | [**appsGetOrgInstallation**](Apis/AppsApi.http#appsgetorginstallation) | **GET** /orgs/{org}/installation | Get an organization installation for the authenticated app
*AppsApi* | [**appsGetRepoInstallation**](Apis/AppsApi.http#appsgetrepoinstallation) | **GET** /repos/{owner}/{repo}/installation | Get a repository installation for the authenticated app
*AppsApi* | [**appsGetSubscriptionPlanForAccount**](Apis/AppsApi.http#appsgetsubscriptionplanforaccount) | **GET** /marketplace_listing/accounts/{account_id} | Get a subscription plan for an account
*AppsApi* | [**appsGetSubscriptionPlanForAccountStubbed**](Apis/AppsApi.http#appsgetsubscriptionplanforaccountstubbed) | **GET** /marketplace_listing/stubbed/accounts/{account_id} | Get a subscription plan for an account (stubbed)
*AppsApi* | [**appsGetUserInstallation**](Apis/AppsApi.http#appsgetuserinstallation) | **GET** /users/{username}/installation | Get a user installation for the authenticated app
*AppsApi* | [**appsGetWebhookConfigForApp**](Apis/AppsApi.http#appsgetwebhookconfigforapp) | **GET** /app/hook/config | Get a webhook configuration for an app
*AppsApi* | [**appsGetWebhookDelivery**](Apis/AppsApi.http#appsgetwebhookdelivery) | **GET** /app/hook/deliveries/{delivery_id} | Get a delivery for an app webhook
*AppsApi* | [**appsListAccountsForPlan**](Apis/AppsApi.http#appslistaccountsforplan) | **GET** /marketplace_listing/plans/{plan_id}/accounts | List accounts for a plan
*AppsApi* | [**appsListAccountsForPlanStubbed**](Apis/AppsApi.http#appslistaccountsforplanstubbed) | **GET** /marketplace_listing/stubbed/plans/{plan_id}/accounts | List accounts for a plan (stubbed)
*AppsApi* | [**appsListInstallationReposForAuthenticatedUser**](Apis/AppsApi.http#appslistinstallationreposforauthenticateduser) | **GET** /user/installations/{installation_id}/repositories | List repositories accessible to the user access token
*AppsApi* | [**appsListInstallationRequestsForAuthenticatedApp**](Apis/AppsApi.http#appslistinstallationrequestsforauthenticatedapp) | **GET** /app/installation-requests | List installation requests for the authenticated app
*AppsApi* | [**appsListInstallations**](Apis/AppsApi.http#appslistinstallations) | **GET** /app/installations | List installations for the authenticated app
*AppsApi* | [**appsListInstallationsForAuthenticatedUser**](Apis/AppsApi.http#appslistinstallationsforauthenticateduser) | **GET** /user/installations | List app installations accessible to the user access token
*AppsApi* | [**appsListPlans**](Apis/AppsApi.http#appslistplans) | **GET** /marketplace_listing/plans | List plans
*AppsApi* | [**appsListPlansStubbed**](Apis/AppsApi.http#appslistplansstubbed) | **GET** /marketplace_listing/stubbed/plans | List plans (stubbed)
*AppsApi* | [**appsListReposAccessibleToInstallation**](Apis/AppsApi.http#appslistreposaccessibletoinstallation) | **GET** /installation/repositories | List repositories accessible to the app installation
*AppsApi* | [**appsListSubscriptionsForAuthenticatedUser**](Apis/AppsApi.http#appslistsubscriptionsforauthenticateduser) | **GET** /user/marketplace_purchases | List subscriptions for the authenticated user
*AppsApi* | [**appsListSubscriptionsForAuthenticatedUserStubbed**](Apis/AppsApi.http#appslistsubscriptionsforauthenticateduserstubbed) | **GET** /user/marketplace_purchases/stubbed | List subscriptions for the authenticated user (stubbed)
*AppsApi* | [**appsListWebhookDeliveries**](Apis/AppsApi.http#appslistwebhookdeliveries) | **GET** /app/hook/deliveries | List deliveries for an app webhook
*AppsApi* | [**appsRedeliverWebhookDelivery**](Apis/AppsApi.http#appsredeliverwebhookdelivery) | **POST** /app/hook/deliveries/{delivery_id}/attempts | Redeliver a delivery for an app webhook
*AppsApi* | [**appsRemoveRepoFromInstallationForAuthenticatedUser**](Apis/AppsApi.http#appsremoverepofrominstallationforauthenticateduser) | **DELETE** /user/installations/{installation_id}/repositories/{repository_id} | Remove a repository from an app installation
*AppsApi* | [**appsResetToken**](Apis/AppsApi.http#appsresettoken) | **PATCH** /applications/{client_id}/token | Reset a token
*AppsApi* | [**appsRevokeInstallationAccessToken**](Apis/AppsApi.http#appsrevokeinstallationaccesstoken) | **DELETE** /installation/token | Revoke an installation access token
*AppsApi* | [**appsScopeToken**](Apis/AppsApi.http#appsscopetoken) | **POST** /applications/{client_id}/token/scoped | Create a scoped access token
*AppsApi* | [**appsSuspendInstallation**](Apis/AppsApi.http#appssuspendinstallation) | **PUT** /app/installations/{installation_id}/suspended | Suspend an app installation
*AppsApi* | [**appsUnsuspendInstallation**](Apis/AppsApi.http#appsunsuspendinstallation) | **DELETE** /app/installations/{installation_id}/suspended | Unsuspend an app installation
*AppsApi* | [**appsUpdateWebhookConfigForApp**](Apis/AppsApi.http#appsupdatewebhookconfigforapp) | **PATCH** /app/hook/config | Update a webhook configuration for an app
*BillingApi* | [**billingGetGithubActionsBillingOrg**](Apis/BillingApi.http#billinggetgithubactionsbillingorg) | **GET** /orgs/{org}/settings/billing/actions | Get GitHub Actions billing for an organization
*BillingApi* | [**billingGetGithubActionsBillingUser**](Apis/BillingApi.http#billinggetgithubactionsbillinguser) | **GET** /users/{username}/settings/billing/actions | Get GitHub Actions billing for a user
*BillingApi* | [**billingGetGithubPackagesBillingOrg**](Apis/BillingApi.http#billinggetgithubpackagesbillingorg) | **GET** /orgs/{org}/settings/billing/packages | Get GitHub Packages billing for an organization
*BillingApi* | [**billingGetGithubPackagesBillingUser**](Apis/BillingApi.http#billinggetgithubpackagesbillinguser) | **GET** /users/{username}/settings/billing/packages | Get GitHub Packages billing for a user
*BillingApi* | [**billingGetSharedStorageBillingOrg**](Apis/BillingApi.http#billinggetsharedstoragebillingorg) | **GET** /orgs/{org}/settings/billing/shared-storage | Get shared storage billing for an organization
*BillingApi* | [**billingGetSharedStorageBillingUser**](Apis/BillingApi.http#billinggetsharedstoragebillinguser) | **GET** /users/{username}/settings/billing/shared-storage | Get shared storage billing for a user
*ChecksApi* | [**checksCreate**](Apis/ChecksApi.http#checkscreate) | **POST** /repos/{owner}/{repo}/check-runs | Create a check run
*ChecksApi* | [**checksCreateSuite**](Apis/ChecksApi.http#checkscreatesuite) | **POST** /repos/{owner}/{repo}/check-suites | Create a check suite
*ChecksApi* | [**checksGet**](Apis/ChecksApi.http#checksget) | **GET** /repos/{owner}/{repo}/check-runs/{check_run_id} | Get a check run
*ChecksApi* | [**checksGetSuite**](Apis/ChecksApi.http#checksgetsuite) | **GET** /repos/{owner}/{repo}/check-suites/{check_suite_id} | Get a check suite
*ChecksApi* | [**checksListAnnotations**](Apis/ChecksApi.http#checkslistannotations) | **GET** /repos/{owner}/{repo}/check-runs/{check_run_id}/annotations | List check run annotations
*ChecksApi* | [**checksListForRef**](Apis/ChecksApi.http#checkslistforref) | **GET** /repos/{owner}/{repo}/commits/{ref}/check-runs | List check runs for a Git reference
*ChecksApi* | [**checksListForSuite**](Apis/ChecksApi.http#checkslistforsuite) | **GET** /repos/{owner}/{repo}/check-suites/{check_suite_id}/check-runs | List check runs in a check suite
*ChecksApi* | [**checksListSuitesForRef**](Apis/ChecksApi.http#checkslistsuitesforref) | **GET** /repos/{owner}/{repo}/commits/{ref}/check-suites | List check suites for a Git reference
*ChecksApi* | [**checksRerequestRun**](Apis/ChecksApi.http#checksrerequestrun) | **POST** /repos/{owner}/{repo}/check-runs/{check_run_id}/rerequest | Rerequest a check run
*ChecksApi* | [**checksRerequestSuite**](Apis/ChecksApi.http#checksrerequestsuite) | **POST** /repos/{owner}/{repo}/check-suites/{check_suite_id}/rerequest | Rerequest a check suite
*ChecksApi* | [**checksSetSuitesPreferences**](Apis/ChecksApi.http#checkssetsuitespreferences) | **PATCH** /repos/{owner}/{repo}/check-suites/preferences | Update repository preferences for check suites
*ChecksApi* | [**checksUpdate**](Apis/ChecksApi.http#checksupdate) | **PATCH** /repos/{owner}/{repo}/check-runs/{check_run_id} | Update a check run
*ClassroomApi* | [**classroomGetAClassroom**](Apis/ClassroomApi.http#classroomgetaclassroom) | **GET** /classrooms/{classroom_id} | Get a classroom
*ClassroomApi* | [**classroomGetAnAssignment**](Apis/ClassroomApi.http#classroomgetanassignment) | **GET** /assignments/{assignment_id} | Get an assignment
*ClassroomApi* | [**classroomGetAssignmentGrades**](Apis/ClassroomApi.http#classroomgetassignmentgrades) | **GET** /assignments/{assignment_id}/grades | Get assignment grades
*ClassroomApi* | [**classroomListAcceptedAssigmentsForAnAssignment**](Apis/ClassroomApi.http#classroomlistacceptedassigmentsforanassignment) | **GET** /assignments/{assignment_id}/accepted_assignments | List accepted assignments for an assignment
*ClassroomApi* | [**classroomListAssignmentsForAClassroom**](Apis/ClassroomApi.http#classroomlistassignmentsforaclassroom) | **GET** /classrooms/{classroom_id}/assignments | List assignments for a classroom
*ClassroomApi* | [**classroomListClassrooms**](Apis/ClassroomApi.http#classroomlistclassrooms) | **GET** /classrooms | List classrooms
*CodeScanningApi* | [**codeScanning/deleteAnalysis**](Apis/CodeScanningApi.http#codescanning/deleteanalysis) | **DELETE** /repos/{owner}/{repo}/code-scanning/analyses/{analysis_id} | Delete a code scanning analysis from a repository
*CodeScanningApi* | [**codeScanning/getAlert**](Apis/CodeScanningApi.http#codescanning/getalert) | **GET** /repos/{owner}/{repo}/code-scanning/alerts/{alert_number} | Get a code scanning alert
*CodeScanningApi* | [**codeScanning/getAnalysis**](Apis/CodeScanningApi.http#codescanning/getanalysis) | **GET** /repos/{owner}/{repo}/code-scanning/analyses/{analysis_id} | Get a code scanning analysis for a repository
*CodeScanningApi* | [**codeScanning/getCodeqlDatabase**](Apis/CodeScanningApi.http#codescanning/getcodeqldatabase) | **GET** /repos/{owner}/{repo}/code-scanning/codeql/databases/{language} | Get a CodeQL database for a repository
*CodeScanningApi* | [**codeScanning/getDefaultSetup**](Apis/CodeScanningApi.http#codescanning/getdefaultsetup) | **GET** /repos/{owner}/{repo}/code-scanning/default-setup | Get a code scanning default setup configuration
*CodeScanningApi* | [**codeScanning/getSarif**](Apis/CodeScanningApi.http#codescanning/getsarif) | **GET** /repos/{owner}/{repo}/code-scanning/sarifs/{sarif_id} | Get information about a SARIF upload
*CodeScanningApi* | [**codeScanning/listAlertInstances**](Apis/CodeScanningApi.http#codescanning/listalertinstances) | **GET** /repos/{owner}/{repo}/code-scanning/alerts/{alert_number}/instances | List instances of a code scanning alert
*CodeScanningApi* | [**codeScanning/listAlertsForOrg**](Apis/CodeScanningApi.http#codescanning/listalertsfororg) | **GET** /orgs/{org}/code-scanning/alerts | List code scanning alerts for an organization
*CodeScanningApi* | [**codeScanning/listAlertsForRepo**](Apis/CodeScanningApi.http#codescanning/listalertsforrepo) | **GET** /repos/{owner}/{repo}/code-scanning/alerts | List code scanning alerts for a repository
*CodeScanningApi* | [**codeScanning/listCodeqlDatabases**](Apis/CodeScanningApi.http#codescanning/listcodeqldatabases) | **GET** /repos/{owner}/{repo}/code-scanning/codeql/databases | List CodeQL databases for a repository
*CodeScanningApi* | [**codeScanning/listRecentAnalyses**](Apis/CodeScanningApi.http#codescanning/listrecentanalyses) | **GET** /repos/{owner}/{repo}/code-scanning/analyses | List code scanning analyses for a repository
*CodeScanningApi* | [**codeScanning/updateAlert**](Apis/CodeScanningApi.http#codescanning/updatealert) | **PATCH** /repos/{owner}/{repo}/code-scanning/alerts/{alert_number} | Update a code scanning alert
*CodeScanningApi* | [**codeScanning/updateDefaultSetup**](Apis/CodeScanningApi.http#codescanning/updatedefaultsetup) | **PATCH** /repos/{owner}/{repo}/code-scanning/default-setup | Update a code scanning default setup configuration
*CodeScanningApi* | [**codeScanning/uploadSarif**](Apis/CodeScanningApi.http#codescanning/uploadsarif) | **POST** /repos/{owner}/{repo}/code-scanning/sarifs | Upload an analysis as SARIF data
*CodesOfConductApi* | [**codesOfConduct/getAllCodesOfConduct**](Apis/CodesOfConductApi.http#codesofconduct/getallcodesofconduct) | **GET** /codes_of_conduct | Get all codes of conduct
*CodesOfConductApi* | [**codesOfConduct/getConductCode**](Apis/CodesOfConductApi.http#codesofconduct/getconductcode) | **GET** /codes_of_conduct/{key} | Get a code of conduct
*CodespacesApi* | [**codespacesAddRepositoryForSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesaddrepositoryforsecretforauthenticateduser) | **PUT** /user/codespaces/secrets/{secret_name}/repositories/{repository_id} | Add a selected repository to a user secret
*CodespacesApi* | [**codespacesAddSelectedRepoToOrgSecret**](Apis/CodespacesApi.http#codespacesaddselectedrepotoorgsecret) | **PUT** /orgs/{org}/codespaces/secrets/{secret_name}/repositories/{repository_id} | Add selected repository to an organization secret
*CodespacesApi* | [**codespacesCheckPermissionsForDevcontainer**](Apis/CodespacesApi.http#codespacescheckpermissionsfordevcontainer) | **GET** /repos/{owner}/{repo}/codespaces/permissions_check | Check if permissions defined by a devcontainer have been accepted by the authenticated user
*CodespacesApi* | [**codespacesCodespaceMachinesForAuthenticatedUser**](Apis/CodespacesApi.http#codespacescodespacemachinesforauthenticateduser) | **GET** /user/codespaces/{codespace_name}/machines | List machine types for a codespace
*CodespacesApi* | [**codespacesCreateForAuthenticatedUser**](Apis/CodespacesApi.http#codespacescreateforauthenticateduser) | **POST** /user/codespaces | Create a codespace for the authenticated user
*CodespacesApi* | [**codespacesCreateOrUpdateOrgSecret**](Apis/CodespacesApi.http#codespacescreateorupdateorgsecret) | **PUT** /orgs/{org}/codespaces/secrets/{secret_name} | Create or update an organization secret
*CodespacesApi* | [**codespacesCreateOrUpdateRepoSecret**](Apis/CodespacesApi.http#codespacescreateorupdatereposecret) | **PUT** /repos/{owner}/{repo}/codespaces/secrets/{secret_name} | Create or update a repository secret
*CodespacesApi* | [**codespacesCreateOrUpdateSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespacescreateorupdatesecretforauthenticateduser) | **PUT** /user/codespaces/secrets/{secret_name} | Create or update a secret for the authenticated user
*CodespacesApi* | [**codespacesCreateWithPrForAuthenticatedUser**](Apis/CodespacesApi.http#codespacescreatewithprforauthenticateduser) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/codespaces | Create a codespace from a pull request
*CodespacesApi* | [**codespacesCreateWithRepoForAuthenticatedUser**](Apis/CodespacesApi.http#codespacescreatewithrepoforauthenticateduser) | **POST** /repos/{owner}/{repo}/codespaces | Create a codespace in a repository
*CodespacesApi* | [**codespacesDeleteCodespacesAccessUsers**](Apis/CodespacesApi.http#codespacesdeletecodespacesaccessusers) | **DELETE** /orgs/{org}/codespaces/access/selected_users | Remove users from Codespaces access for an organization
*CodespacesApi* | [**codespacesDeleteForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesdeleteforauthenticateduser) | **DELETE** /user/codespaces/{codespace_name} | Delete a codespace for the authenticated user
*CodespacesApi* | [**codespacesDeleteFromOrganization**](Apis/CodespacesApi.http#codespacesdeletefromorganization) | **DELETE** /orgs/{org}/members/{username}/codespaces/{codespace_name} | Delete a codespace from the organization
*CodespacesApi* | [**codespacesDeleteOrgSecret**](Apis/CodespacesApi.http#codespacesdeleteorgsecret) | **DELETE** /orgs/{org}/codespaces/secrets/{secret_name} | Delete an organization secret
*CodespacesApi* | [**codespacesDeleteRepoSecret**](Apis/CodespacesApi.http#codespacesdeletereposecret) | **DELETE** /repos/{owner}/{repo}/codespaces/secrets/{secret_name} | Delete a repository secret
*CodespacesApi* | [**codespacesDeleteSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesdeletesecretforauthenticateduser) | **DELETE** /user/codespaces/secrets/{secret_name} | Delete a secret for the authenticated user
*CodespacesApi* | [**codespacesExportForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesexportforauthenticateduser) | **POST** /user/codespaces/{codespace_name}/exports | Export a codespace for the authenticated user
*CodespacesApi* | [**codespacesGetCodespacesForUserInOrg**](Apis/CodespacesApi.http#codespacesgetcodespacesforuserinorg) | **GET** /orgs/{org}/members/{username}/codespaces | List codespaces for a user in organization
*CodespacesApi* | [**codespacesGetExportDetailsForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesgetexportdetailsforauthenticateduser) | **GET** /user/codespaces/{codespace_name}/exports/{export_id} | Get details about a codespace export
*CodespacesApi* | [**codespacesGetForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesgetforauthenticateduser) | **GET** /user/codespaces/{codespace_name} | Get a codespace for the authenticated user
*CodespacesApi* | [**codespacesGetOrgPublicKey**](Apis/CodespacesApi.http#codespacesgetorgpublickey) | **GET** /orgs/{org}/codespaces/secrets/public-key | Get an organization public key
*CodespacesApi* | [**codespacesGetOrgSecret**](Apis/CodespacesApi.http#codespacesgetorgsecret) | **GET** /orgs/{org}/codespaces/secrets/{secret_name} | Get an organization secret
*CodespacesApi* | [**codespacesGetPublicKeyForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesgetpublickeyforauthenticateduser) | **GET** /user/codespaces/secrets/public-key | Get public key for the authenticated user
*CodespacesApi* | [**codespacesGetRepoPublicKey**](Apis/CodespacesApi.http#codespacesgetrepopublickey) | **GET** /repos/{owner}/{repo}/codespaces/secrets/public-key | Get a repository public key
*CodespacesApi* | [**codespacesGetRepoSecret**](Apis/CodespacesApi.http#codespacesgetreposecret) | **GET** /repos/{owner}/{repo}/codespaces/secrets/{secret_name} | Get a repository secret
*CodespacesApi* | [**codespacesGetSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesgetsecretforauthenticateduser) | **GET** /user/codespaces/secrets/{secret_name} | Get a secret for the authenticated user
*CodespacesApi* | [**codespacesListDevcontainersInRepositoryForAuthenticatedUser**](Apis/CodespacesApi.http#codespaceslistdevcontainersinrepositoryforauthenticateduser) | **GET** /repos/{owner}/{repo}/codespaces/devcontainers | List devcontainer configurations in a repository for the authenticated user
*CodespacesApi* | [**codespacesListForAuthenticatedUser**](Apis/CodespacesApi.http#codespaceslistforauthenticateduser) | **GET** /user/codespaces | List codespaces for the authenticated user
*CodespacesApi* | [**codespacesListInOrganization**](Apis/CodespacesApi.http#codespaceslistinorganization) | **GET** /orgs/{org}/codespaces | List codespaces for the organization
*CodespacesApi* | [**codespacesListInRepositoryForAuthenticatedUser**](Apis/CodespacesApi.http#codespaceslistinrepositoryforauthenticateduser) | **GET** /repos/{owner}/{repo}/codespaces | List codespaces in a repository for the authenticated user
*CodespacesApi* | [**codespacesListOrgSecrets**](Apis/CodespacesApi.http#codespaceslistorgsecrets) | **GET** /orgs/{org}/codespaces/secrets | List organization secrets
*CodespacesApi* | [**codespacesListRepoSecrets**](Apis/CodespacesApi.http#codespaceslistreposecrets) | **GET** /repos/{owner}/{repo}/codespaces/secrets | List repository secrets
*CodespacesApi* | [**codespacesListRepositoriesForSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespaceslistrepositoriesforsecretforauthenticateduser) | **GET** /user/codespaces/secrets/{secret_name}/repositories | List selected repositories for a user secret
*CodespacesApi* | [**codespacesListSecretsForAuthenticatedUser**](Apis/CodespacesApi.http#codespaceslistsecretsforauthenticateduser) | **GET** /user/codespaces/secrets | List secrets for the authenticated user
*CodespacesApi* | [**codespacesListSelectedReposForOrgSecret**](Apis/CodespacesApi.http#codespaceslistselectedreposfororgsecret) | **GET** /orgs/{org}/codespaces/secrets/{secret_name}/repositories | List selected repositories for an organization secret
*CodespacesApi* | [**codespacesPreFlightWithRepoForAuthenticatedUser**](Apis/CodespacesApi.http#codespacespreflightwithrepoforauthenticateduser) | **GET** /repos/{owner}/{repo}/codespaces/new | Get default attributes for a codespace
*CodespacesApi* | [**codespacesPublishForAuthenticatedUser**](Apis/CodespacesApi.http#codespacespublishforauthenticateduser) | **POST** /user/codespaces/{codespace_name}/publish | Create a repository from an unpublished codespace
*CodespacesApi* | [**codespacesRemoveRepositoryForSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesremoverepositoryforsecretforauthenticateduser) | **DELETE** /user/codespaces/secrets/{secret_name}/repositories/{repository_id} | Remove a selected repository from a user secret
*CodespacesApi* | [**codespacesRemoveSelectedRepoFromOrgSecret**](Apis/CodespacesApi.http#codespacesremoveselectedrepofromorgsecret) | **DELETE** /orgs/{org}/codespaces/secrets/{secret_name}/repositories/{repository_id} | Remove selected repository from an organization secret
*CodespacesApi* | [**codespacesRepoMachinesForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesrepomachinesforauthenticateduser) | **GET** /repos/{owner}/{repo}/codespaces/machines | List available machine types for a repository
*CodespacesApi* | [**codespacesSetCodespacesAccess**](Apis/CodespacesApi.http#codespacessetcodespacesaccess) | **PUT** /orgs/{org}/codespaces/access | Manage access control for organization codespaces
*CodespacesApi* | [**codespacesSetCodespacesAccessUsers**](Apis/CodespacesApi.http#codespacessetcodespacesaccessusers) | **POST** /orgs/{org}/codespaces/access/selected_users | Add users to Codespaces access for an organization
*CodespacesApi* | [**codespacesSetRepositoriesForSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespacessetrepositoriesforsecretforauthenticateduser) | **PUT** /user/codespaces/secrets/{secret_name}/repositories | Set selected repositories for a user secret
*CodespacesApi* | [**codespacesSetSelectedReposForOrgSecret**](Apis/CodespacesApi.http#codespacessetselectedreposfororgsecret) | **PUT** /orgs/{org}/codespaces/secrets/{secret_name}/repositories | Set selected repositories for an organization secret
*CodespacesApi* | [**codespacesStartForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesstartforauthenticateduser) | **POST** /user/codespaces/{codespace_name}/start | Start a codespace for the authenticated user
*CodespacesApi* | [**codespacesStopForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesstopforauthenticateduser) | **POST** /user/codespaces/{codespace_name}/stop | Stop a codespace for the authenticated user
*CodespacesApi* | [**codespacesStopInOrganization**](Apis/CodespacesApi.http#codespacesstopinorganization) | **POST** /orgs/{org}/members/{username}/codespaces/{codespace_name}/stop | Stop a codespace for an organization user
*CodespacesApi* | [**codespacesUpdateForAuthenticatedUser**](Apis/CodespacesApi.http#codespacesupdateforauthenticateduser) | **PATCH** /user/codespaces/{codespace_name} | Update a codespace for the authenticated user
*CopilotApi* | [**copilotAddCopilotSeatsForTeams**](Apis/CopilotApi.http#copilotaddcopilotseatsforteams) | **POST** /orgs/{org}/copilot/billing/selected_teams | Add teams to the Copilot subscription for an organization
*CopilotApi* | [**copilotAddCopilotSeatsForUsers**](Apis/CopilotApi.http#copilotaddcopilotseatsforusers) | **POST** /orgs/{org}/copilot/billing/selected_users | Add users to the Copilot subscription for an organization
*CopilotApi* | [**copilotCancelCopilotSeatAssignmentForTeams**](Apis/CopilotApi.http#copilotcancelcopilotseatassignmentforteams) | **DELETE** /orgs/{org}/copilot/billing/selected_teams | Remove teams from the Copilot subscription for an organization
*CopilotApi* | [**copilotCancelCopilotSeatAssignmentForUsers**](Apis/CopilotApi.http#copilotcancelcopilotseatassignmentforusers) | **DELETE** /orgs/{org}/copilot/billing/selected_users | Remove users from the Copilot subscription for an organization
*CopilotApi* | [**copilotGetCopilotOrganizationDetails**](Apis/CopilotApi.http#copilotgetcopilotorganizationdetails) | **GET** /orgs/{org}/copilot/billing | Get Copilot seat information and settings for an organization
*CopilotApi* | [**copilotGetCopilotSeatDetailsForUser**](Apis/CopilotApi.http#copilotgetcopilotseatdetailsforuser) | **GET** /orgs/{org}/members/{username}/copilot | Get Copilot seat assignment details for a user
*CopilotApi* | [**copilotListCopilotSeats**](Apis/CopilotApi.http#copilotlistcopilotseats) | **GET** /orgs/{org}/copilot/billing/seats | List all Copilot seat assignments for an organization
*DependabotApi* | [**dependabotAddSelectedRepoToOrgSecret**](Apis/DependabotApi.http#dependabotaddselectedrepotoorgsecret) | **PUT** /orgs/{org}/dependabot/secrets/{secret_name}/repositories/{repository_id} | Add selected repository to an organization secret
*DependabotApi* | [**dependabotCreateOrUpdateOrgSecret**](Apis/DependabotApi.http#dependabotcreateorupdateorgsecret) | **PUT** /orgs/{org}/dependabot/secrets/{secret_name} | Create or update an organization secret
*DependabotApi* | [**dependabotCreateOrUpdateRepoSecret**](Apis/DependabotApi.http#dependabotcreateorupdatereposecret) | **PUT** /repos/{owner}/{repo}/dependabot/secrets/{secret_name} | Create or update a repository secret
*DependabotApi* | [**dependabotDeleteOrgSecret**](Apis/DependabotApi.http#dependabotdeleteorgsecret) | **DELETE** /orgs/{org}/dependabot/secrets/{secret_name} | Delete an organization secret
*DependabotApi* | [**dependabotDeleteRepoSecret**](Apis/DependabotApi.http#dependabotdeletereposecret) | **DELETE** /repos/{owner}/{repo}/dependabot/secrets/{secret_name} | Delete a repository secret
*DependabotApi* | [**dependabotGetAlert**](Apis/DependabotApi.http#dependabotgetalert) | **GET** /repos/{owner}/{repo}/dependabot/alerts/{alert_number} | Get a Dependabot alert
*DependabotApi* | [**dependabotGetOrgPublicKey**](Apis/DependabotApi.http#dependabotgetorgpublickey) | **GET** /orgs/{org}/dependabot/secrets/public-key | Get an organization public key
*DependabotApi* | [**dependabotGetOrgSecret**](Apis/DependabotApi.http#dependabotgetorgsecret) | **GET** /orgs/{org}/dependabot/secrets/{secret_name} | Get an organization secret
*DependabotApi* | [**dependabotGetRepoPublicKey**](Apis/DependabotApi.http#dependabotgetrepopublickey) | **GET** /repos/{owner}/{repo}/dependabot/secrets/public-key | Get a repository public key
*DependabotApi* | [**dependabotGetRepoSecret**](Apis/DependabotApi.http#dependabotgetreposecret) | **GET** /repos/{owner}/{repo}/dependabot/secrets/{secret_name} | Get a repository secret
*DependabotApi* | [**dependabotListAlertsForEnterprise**](Apis/DependabotApi.http#dependabotlistalertsforenterprise) | **GET** /enterprises/{enterprise}/dependabot/alerts | List Dependabot alerts for an enterprise
*DependabotApi* | [**dependabotListAlertsForOrg**](Apis/DependabotApi.http#dependabotlistalertsfororg) | **GET** /orgs/{org}/dependabot/alerts | List Dependabot alerts for an organization
*DependabotApi* | [**dependabotListAlertsForRepo**](Apis/DependabotApi.http#dependabotlistalertsforrepo) | **GET** /repos/{owner}/{repo}/dependabot/alerts | List Dependabot alerts for a repository
*DependabotApi* | [**dependabotListOrgSecrets**](Apis/DependabotApi.http#dependabotlistorgsecrets) | **GET** /orgs/{org}/dependabot/secrets | List organization secrets
*DependabotApi* | [**dependabotListRepoSecrets**](Apis/DependabotApi.http#dependabotlistreposecrets) | **GET** /repos/{owner}/{repo}/dependabot/secrets | List repository secrets
*DependabotApi* | [**dependabotListSelectedReposForOrgSecret**](Apis/DependabotApi.http#dependabotlistselectedreposfororgsecret) | **GET** /orgs/{org}/dependabot/secrets/{secret_name}/repositories | List selected repositories for an organization secret
*DependabotApi* | [**dependabotRemoveSelectedRepoFromOrgSecret**](Apis/DependabotApi.http#dependabotremoveselectedrepofromorgsecret) | **DELETE** /orgs/{org}/dependabot/secrets/{secret_name}/repositories/{repository_id} | Remove selected repository from an organization secret
*DependabotApi* | [**dependabotSetSelectedReposForOrgSecret**](Apis/DependabotApi.http#dependabotsetselectedreposfororgsecret) | **PUT** /orgs/{org}/dependabot/secrets/{secret_name}/repositories | Set selected repositories for an organization secret
*DependabotApi* | [**dependabotUpdateAlert**](Apis/DependabotApi.http#dependabotupdatealert) | **PATCH** /repos/{owner}/{repo}/dependabot/alerts/{alert_number} | Update a Dependabot alert
*DependencyGraphApi* | [**dependencyGraph/createRepositorySnapshot**](Apis/DependencyGraphApi.http#dependencygraph/createrepositorysnapshot) | **POST** /repos/{owner}/{repo}/dependency-graph/snapshots | Create a snapshot of dependencies for a repository
*DependencyGraphApi* | [**dependencyGraph/diffRange**](Apis/DependencyGraphApi.http#dependencygraph/diffrange) | **GET** /repos/{owner}/{repo}/dependency-graph/compare/{basehead} | Get a diff of the dependencies between commits
*DependencyGraphApi* | [**dependencyGraph/exportSbom**](Apis/DependencyGraphApi.http#dependencygraph/exportsbom) | **GET** /repos/{owner}/{repo}/dependency-graph/sbom | Export a software bill of materials (SBOM) for a repository.
*EmojisApi* | [**emojisGet**](Apis/EmojisApi.http#emojisget) | **GET** /emojis | Get emojis
*GistsApi* | [**gistsCheckIsStarred**](Apis/GistsApi.http#gistscheckisstarred) | **GET** /gists/{gist_id}/star | Check if a gist is starred
*GistsApi* | [**gistsCreate**](Apis/GistsApi.http#gistscreate) | **POST** /gists | Create a gist
*GistsApi* | [**gistsCreateComment**](Apis/GistsApi.http#gistscreatecomment) | **POST** /gists/{gist_id}/comments | Create a gist comment
*GistsApi* | [**gistsDelete**](Apis/GistsApi.http#gistsdelete) | **DELETE** /gists/{gist_id} | Delete a gist
*GistsApi* | [**gistsDeleteComment**](Apis/GistsApi.http#gistsdeletecomment) | **DELETE** /gists/{gist_id}/comments/{comment_id} | Delete a gist comment
*GistsApi* | [**gistsFork**](Apis/GistsApi.http#gistsfork) | **POST** /gists/{gist_id}/forks | Fork a gist
*GistsApi* | [**gistsGet**](Apis/GistsApi.http#gistsget) | **GET** /gists/{gist_id} | Get a gist
*GistsApi* | [**gistsGetComment**](Apis/GistsApi.http#gistsgetcomment) | **GET** /gists/{gist_id}/comments/{comment_id} | Get a gist comment
*GistsApi* | [**gistsGetRevision**](Apis/GistsApi.http#gistsgetrevision) | **GET** /gists/{gist_id}/{sha} | Get a gist revision
*GistsApi* | [**gistsList**](Apis/GistsApi.http#gistslist) | **GET** /gists | List gists for the authenticated user
*GistsApi* | [**gistsListComments**](Apis/GistsApi.http#gistslistcomments) | **GET** /gists/{gist_id}/comments | List gist comments
*GistsApi* | [**gistsListCommits**](Apis/GistsApi.http#gistslistcommits) | **GET** /gists/{gist_id}/commits | List gist commits
*GistsApi* | [**gistsListForUser**](Apis/GistsApi.http#gistslistforuser) | **GET** /users/{username}/gists | List gists for a user
*GistsApi* | [**gistsListForks**](Apis/GistsApi.http#gistslistforks) | **GET** /gists/{gist_id}/forks | List gist forks
*GistsApi* | [**gistsListPublic**](Apis/GistsApi.http#gistslistpublic) | **GET** /gists/public | List public gists
*GistsApi* | [**gistsListStarred**](Apis/GistsApi.http#gistsliststarred) | **GET** /gists/starred | List starred gists
*GistsApi* | [**gistsStar**](Apis/GistsApi.http#gistsstar) | **PUT** /gists/{gist_id}/star | Star a gist
*GistsApi* | [**gistsUnstar**](Apis/GistsApi.http#gistsunstar) | **DELETE** /gists/{gist_id}/star | Unstar a gist
*GistsApi* | [**gistsUpdate**](Apis/GistsApi.http#gistsupdate) | **PATCH** /gists/{gist_id} | Update a gist
*GistsApi* | [**gistsUpdateComment**](Apis/GistsApi.http#gistsupdatecomment) | **PATCH** /gists/{gist_id}/comments/{comment_id} | Update a gist comment
*GitApi* | [**gitCreateBlob**](Apis/GitApi.http#gitcreateblob) | **POST** /repos/{owner}/{repo}/git/blobs | Create a blob
*GitApi* | [**gitCreateCommit**](Apis/GitApi.http#gitcreatecommit) | **POST** /repos/{owner}/{repo}/git/commits | Create a commit
*GitApi* | [**gitCreateRef**](Apis/GitApi.http#gitcreateref) | **POST** /repos/{owner}/{repo}/git/refs | Create a reference
*GitApi* | [**gitCreateTag**](Apis/GitApi.http#gitcreatetag) | **POST** /repos/{owner}/{repo}/git/tags | Create a tag object
*GitApi* | [**gitCreateTree**](Apis/GitApi.http#gitcreatetree) | **POST** /repos/{owner}/{repo}/git/trees | Create a tree
*GitApi* | [**gitDeleteRef**](Apis/GitApi.http#gitdeleteref) | **DELETE** /repos/{owner}/{repo}/git/refs/{ref} | Delete a reference
*GitApi* | [**gitGetBlob**](Apis/GitApi.http#gitgetblob) | **GET** /repos/{owner}/{repo}/git/blobs/{file_sha} | Get a blob
*GitApi* | [**gitGetCommit**](Apis/GitApi.http#gitgetcommit) | **GET** /repos/{owner}/{repo}/git/commits/{commit_sha} | Get a commit object
*GitApi* | [**gitGetRef**](Apis/GitApi.http#gitgetref) | **GET** /repos/{owner}/{repo}/git/ref/{ref} | Get a reference
*GitApi* | [**gitGetTag**](Apis/GitApi.http#gitgettag) | **GET** /repos/{owner}/{repo}/git/tags/{tag_sha} | Get a tag
*GitApi* | [**gitGetTree**](Apis/GitApi.http#gitgettree) | **GET** /repos/{owner}/{repo}/git/trees/{tree_sha} | Get a tree
*GitApi* | [**gitListMatchingRefs**](Apis/GitApi.http#gitlistmatchingrefs) | **GET** /repos/{owner}/{repo}/git/matching-refs/{ref} | List matching references
*GitApi* | [**gitUpdateRef**](Apis/GitApi.http#gitupdateref) | **PATCH** /repos/{owner}/{repo}/git/refs/{ref} | Update a reference
*GitignoreApi* | [**gitignoreGetAllTemplates**](Apis/GitignoreApi.http#gitignoregetalltemplates) | **GET** /gitignore/templates | Get all gitignore templates
*GitignoreApi* | [**gitignoreGetTemplate**](Apis/GitignoreApi.http#gitignoregettemplate) | **GET** /gitignore/templates/{name} | Get a gitignore template
*InteractionsApi* | [**interactionsGetRestrictionsForAuthenticatedUser**](Apis/InteractionsApi.http#interactionsgetrestrictionsforauthenticateduser) | **GET** /user/interaction-limits | Get interaction restrictions for your public repositories
*InteractionsApi* | [**interactionsGetRestrictionsForOrg**](Apis/InteractionsApi.http#interactionsgetrestrictionsfororg) | **GET** /orgs/{org}/interaction-limits | Get interaction restrictions for an organization
*InteractionsApi* | [**interactionsGetRestrictionsForRepo**](Apis/InteractionsApi.http#interactionsgetrestrictionsforrepo) | **GET** /repos/{owner}/{repo}/interaction-limits | Get interaction restrictions for a repository
*InteractionsApi* | [**interactionsRemoveRestrictionsForAuthenticatedUser**](Apis/InteractionsApi.http#interactionsremoverestrictionsforauthenticateduser) | **DELETE** /user/interaction-limits | Remove interaction restrictions from your public repositories
*InteractionsApi* | [**interactionsRemoveRestrictionsForOrg**](Apis/InteractionsApi.http#interactionsremoverestrictionsfororg) | **DELETE** /orgs/{org}/interaction-limits | Remove interaction restrictions for an organization
*InteractionsApi* | [**interactionsRemoveRestrictionsForRepo**](Apis/InteractionsApi.http#interactionsremoverestrictionsforrepo) | **DELETE** /repos/{owner}/{repo}/interaction-limits | Remove interaction restrictions for a repository
*InteractionsApi* | [**interactionsSetRestrictionsForAuthenticatedUser**](Apis/InteractionsApi.http#interactionssetrestrictionsforauthenticateduser) | **PUT** /user/interaction-limits | Set interaction restrictions for your public repositories
*InteractionsApi* | [**interactionsSetRestrictionsForOrg**](Apis/InteractionsApi.http#interactionssetrestrictionsfororg) | **PUT** /orgs/{org}/interaction-limits | Set interaction restrictions for an organization
*InteractionsApi* | [**interactionsSetRestrictionsForRepo**](Apis/InteractionsApi.http#interactionssetrestrictionsforrepo) | **PUT** /repos/{owner}/{repo}/interaction-limits | Set interaction restrictions for a repository
*IssuesApi* | [**issuesAddAssignees**](Apis/IssuesApi.http#issuesaddassignees) | **POST** /repos/{owner}/{repo}/issues/{issue_number}/assignees | Add assignees to an issue
*IssuesApi* | [**issuesAddLabels**](Apis/IssuesApi.http#issuesaddlabels) | **POST** /repos/{owner}/{repo}/issues/{issue_number}/labels | Add labels to an issue
*IssuesApi* | [**issuesCheckUserCanBeAssigned**](Apis/IssuesApi.http#issuescheckusercanbeassigned) | **GET** /repos/{owner}/{repo}/assignees/{assignee} | Check if a user can be assigned
*IssuesApi* | [**issuesCheckUserCanBeAssignedToIssue**](Apis/IssuesApi.http#issuescheckusercanbeassignedtoissue) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/assignees/{assignee} | Check if a user can be assigned to a issue
*IssuesApi* | [**issuesCreate**](Apis/IssuesApi.http#issuescreate) | **POST** /repos/{owner}/{repo}/issues | Create an issue
*IssuesApi* | [**issuesCreateComment**](Apis/IssuesApi.http#issuescreatecomment) | **POST** /repos/{owner}/{repo}/issues/{issue_number}/comments | Create an issue comment
*IssuesApi* | [**issuesCreateLabel**](Apis/IssuesApi.http#issuescreatelabel) | **POST** /repos/{owner}/{repo}/labels | Create a label
*IssuesApi* | [**issuesCreateMilestone**](Apis/IssuesApi.http#issuescreatemilestone) | **POST** /repos/{owner}/{repo}/milestones | Create a milestone
*IssuesApi* | [**issuesDeleteComment**](Apis/IssuesApi.http#issuesdeletecomment) | **DELETE** /repos/{owner}/{repo}/issues/comments/{comment_id} | Delete an issue comment
*IssuesApi* | [**issuesDeleteLabel**](Apis/IssuesApi.http#issuesdeletelabel) | **DELETE** /repos/{owner}/{repo}/labels/{name} | Delete a label
*IssuesApi* | [**issuesDeleteMilestone**](Apis/IssuesApi.http#issuesdeletemilestone) | **DELETE** /repos/{owner}/{repo}/milestones/{milestone_number} | Delete a milestone
*IssuesApi* | [**issuesGet**](Apis/IssuesApi.http#issuesget) | **GET** /repos/{owner}/{repo}/issues/{issue_number} | Get an issue
*IssuesApi* | [**issuesGetComment**](Apis/IssuesApi.http#issuesgetcomment) | **GET** /repos/{owner}/{repo}/issues/comments/{comment_id} | Get an issue comment
*IssuesApi* | [**issuesGetEvent**](Apis/IssuesApi.http#issuesgetevent) | **GET** /repos/{owner}/{repo}/issues/events/{event_id} | Get an issue event
*IssuesApi* | [**issuesGetLabel**](Apis/IssuesApi.http#issuesgetlabel) | **GET** /repos/{owner}/{repo}/labels/{name} | Get a label
*IssuesApi* | [**issuesGetMilestone**](Apis/IssuesApi.http#issuesgetmilestone) | **GET** /repos/{owner}/{repo}/milestones/{milestone_number} | Get a milestone
*IssuesApi* | [**issuesList**](Apis/IssuesApi.http#issueslist) | **GET** /issues | List issues assigned to the authenticated user
*IssuesApi* | [**issuesListAssignees**](Apis/IssuesApi.http#issueslistassignees) | **GET** /repos/{owner}/{repo}/assignees | List assignees
*IssuesApi* | [**issuesListComments**](Apis/IssuesApi.http#issueslistcomments) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/comments | List issue comments
*IssuesApi* | [**issuesListCommentsForRepo**](Apis/IssuesApi.http#issueslistcommentsforrepo) | **GET** /repos/{owner}/{repo}/issues/comments | List issue comments for a repository
*IssuesApi* | [**issuesListEvents**](Apis/IssuesApi.http#issueslistevents) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/events | List issue events
*IssuesApi* | [**issuesListEventsForRepo**](Apis/IssuesApi.http#issueslisteventsforrepo) | **GET** /repos/{owner}/{repo}/issues/events | List issue events for a repository
*IssuesApi* | [**issuesListEventsForTimeline**](Apis/IssuesApi.http#issueslisteventsfortimeline) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/timeline | List timeline events for an issue
*IssuesApi* | [**issuesListForAuthenticatedUser**](Apis/IssuesApi.http#issueslistforauthenticateduser) | **GET** /user/issues | List user account issues assigned to the authenticated user
*IssuesApi* | [**issuesListForOrg**](Apis/IssuesApi.http#issueslistfororg) | **GET** /orgs/{org}/issues | List organization issues assigned to the authenticated user
*IssuesApi* | [**issuesListForRepo**](Apis/IssuesApi.http#issueslistforrepo) | **GET** /repos/{owner}/{repo}/issues | List repository issues
*IssuesApi* | [**issuesListLabelsForMilestone**](Apis/IssuesApi.http#issueslistlabelsformilestone) | **GET** /repos/{owner}/{repo}/milestones/{milestone_number}/labels | List labels for issues in a milestone
*IssuesApi* | [**issuesListLabelsForRepo**](Apis/IssuesApi.http#issueslistlabelsforrepo) | **GET** /repos/{owner}/{repo}/labels | List labels for a repository
*IssuesApi* | [**issuesListLabelsOnIssue**](Apis/IssuesApi.http#issueslistlabelsonissue) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/labels | List labels for an issue
*IssuesApi* | [**issuesListMilestones**](Apis/IssuesApi.http#issueslistmilestones) | **GET** /repos/{owner}/{repo}/milestones | List milestones
*IssuesApi* | [**issuesLock**](Apis/IssuesApi.http#issueslock) | **PUT** /repos/{owner}/{repo}/issues/{issue_number}/lock | Lock an issue
*IssuesApi* | [**issuesRemoveAllLabels**](Apis/IssuesApi.http#issuesremovealllabels) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/labels | Remove all labels from an issue
*IssuesApi* | [**issuesRemoveAssignees**](Apis/IssuesApi.http#issuesremoveassignees) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/assignees | Remove assignees from an issue
*IssuesApi* | [**issuesRemoveLabel**](Apis/IssuesApi.http#issuesremovelabel) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/labels/{name} | Remove a label from an issue
*IssuesApi* | [**issuesSetLabels**](Apis/IssuesApi.http#issuessetlabels) | **PUT** /repos/{owner}/{repo}/issues/{issue_number}/labels | Set labels for an issue
*IssuesApi* | [**issuesUnlock**](Apis/IssuesApi.http#issuesunlock) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/lock | Unlock an issue
*IssuesApi* | [**issuesUpdate**](Apis/IssuesApi.http#issuesupdate) | **PATCH** /repos/{owner}/{repo}/issues/{issue_number} | Update an issue
*IssuesApi* | [**issuesUpdateComment**](Apis/IssuesApi.http#issuesupdatecomment) | **PATCH** /repos/{owner}/{repo}/issues/comments/{comment_id} | Update an issue comment
*IssuesApi* | [**issuesUpdateLabel**](Apis/IssuesApi.http#issuesupdatelabel) | **PATCH** /repos/{owner}/{repo}/labels/{name} | Update a label
*IssuesApi* | [**issuesUpdateMilestone**](Apis/IssuesApi.http#issuesupdatemilestone) | **PATCH** /repos/{owner}/{repo}/milestones/{milestone_number} | Update a milestone
*LicensesApi* | [**licensesGet**](Apis/LicensesApi.http#licensesget) | **GET** /licenses/{license} | Get a license
*LicensesApi* | [**licensesGetAllCommonlyUsed**](Apis/LicensesApi.http#licensesgetallcommonlyused) | **GET** /licenses | Get all commonly used licenses
*LicensesApi* | [**licensesGetForRepo**](Apis/LicensesApi.http#licensesgetforrepo) | **GET** /repos/{owner}/{repo}/license | Get the license for a repository
*MarkdownApi* | [**markdownRender**](Apis/MarkdownApi.http#markdownrender) | **POST** /markdown | Render a Markdown document
*MarkdownApi* | [**markdownRenderRaw**](Apis/MarkdownApi.http#markdownrenderraw) | **POST** /markdown/raw | Render a Markdown document in raw mode
*MetaApi* | [**metaGet**](Apis/MetaApi.http#metaget) | **GET** /meta | Get GitHub meta information
*MetaApi* | [**metaGetAllVersions**](Apis/MetaApi.http#metagetallversions) | **GET** /versions | Get all API versions
*MetaApi* | [**metaGetOctocat**](Apis/MetaApi.http#metagetoctocat) | **GET** /octocat | Get Octocat
*MetaApi* | [**metaGetZen**](Apis/MetaApi.http#metagetzen) | **GET** /zen | Get the Zen of GitHub
*MetaApi* | [**metaRoot**](Apis/MetaApi.http#metaroot) | **GET** / | GitHub API Root
*MigrationsApi* | [**migrationsCancelImport**](Apis/MigrationsApi.http#migrationscancelimport) | **DELETE** /repos/{owner}/{repo}/import | Cancel an import
*MigrationsApi* | [**migrationsDeleteArchiveForAuthenticatedUser**](Apis/MigrationsApi.http#migrationsdeletearchiveforauthenticateduser) | **DELETE** /user/migrations/{migration_id}/archive | Delete a user migration archive
*MigrationsApi* | [**migrationsDeleteArchiveForOrg**](Apis/MigrationsApi.http#migrationsdeletearchivefororg) | **DELETE** /orgs/{org}/migrations/{migration_id}/archive | Delete an organization migration archive
*MigrationsApi* | [**migrationsDownloadArchiveForOrg**](Apis/MigrationsApi.http#migrationsdownloadarchivefororg) | **GET** /orgs/{org}/migrations/{migration_id}/archive | Download an organization migration archive
*MigrationsApi* | [**migrationsGetArchiveForAuthenticatedUser**](Apis/MigrationsApi.http#migrationsgetarchiveforauthenticateduser) | **GET** /user/migrations/{migration_id}/archive | Download a user migration archive
*MigrationsApi* | [**migrationsGetCommitAuthors**](Apis/MigrationsApi.http#migrationsgetcommitauthors) | **GET** /repos/{owner}/{repo}/import/authors | Get commit authors
*MigrationsApi* | [**migrationsGetImportStatus**](Apis/MigrationsApi.http#migrationsgetimportstatus) | **GET** /repos/{owner}/{repo}/import | Get an import status
*MigrationsApi* | [**migrationsGetLargeFiles**](Apis/MigrationsApi.http#migrationsgetlargefiles) | **GET** /repos/{owner}/{repo}/import/large_files | Get large files
*MigrationsApi* | [**migrationsGetStatusForAuthenticatedUser**](Apis/MigrationsApi.http#migrationsgetstatusforauthenticateduser) | **GET** /user/migrations/{migration_id} | Get a user migration status
*MigrationsApi* | [**migrationsGetStatusForOrg**](Apis/MigrationsApi.http#migrationsgetstatusfororg) | **GET** /orgs/{org}/migrations/{migration_id} | Get an organization migration status
*MigrationsApi* | [**migrationsListForAuthenticatedUser**](Apis/MigrationsApi.http#migrationslistforauthenticateduser) | **GET** /user/migrations | List user migrations
*MigrationsApi* | [**migrationsListForOrg**](Apis/MigrationsApi.http#migrationslistfororg) | **GET** /orgs/{org}/migrations | List organization migrations
*MigrationsApi* | [**migrationsListReposForAuthenticatedUser**](Apis/MigrationsApi.http#migrationslistreposforauthenticateduser) | **GET** /user/migrations/{migration_id}/repositories | List repositories for a user migration
*MigrationsApi* | [**migrationsListReposForOrg**](Apis/MigrationsApi.http#migrationslistreposfororg) | **GET** /orgs/{org}/migrations/{migration_id}/repositories | List repositories in an organization migration
*MigrationsApi* | [**migrationsMapCommitAuthor**](Apis/MigrationsApi.http#migrationsmapcommitauthor) | **PATCH** /repos/{owner}/{repo}/import/authors/{author_id} | Map a commit author
*MigrationsApi* | [**migrationsSetLfsPreference**](Apis/MigrationsApi.http#migrationssetlfspreference) | **PATCH** /repos/{owner}/{repo}/import/lfs | Update Git LFS preference
*MigrationsApi* | [**migrationsStartForAuthenticatedUser**](Apis/MigrationsApi.http#migrationsstartforauthenticateduser) | **POST** /user/migrations | Start a user migration
*MigrationsApi* | [**migrationsStartForOrg**](Apis/MigrationsApi.http#migrationsstartfororg) | **POST** /orgs/{org}/migrations | Start an organization migration
*MigrationsApi* | [**migrationsStartImport**](Apis/MigrationsApi.http#migrationsstartimport) | **PUT** /repos/{owner}/{repo}/import | Start an import
*MigrationsApi* | [**migrationsUnlockRepoForAuthenticatedUser**](Apis/MigrationsApi.http#migrationsunlockrepoforauthenticateduser) | **DELETE** /user/migrations/{migration_id}/repos/{repo_name}/lock | Unlock a user repository
*MigrationsApi* | [**migrationsUnlockRepoForOrg**](Apis/MigrationsApi.http#migrationsunlockrepofororg) | **DELETE** /orgs/{org}/migrations/{migration_id}/repos/{repo_name}/lock | Unlock an organization repository
*MigrationsApi* | [**migrationsUpdateImport**](Apis/MigrationsApi.http#migrationsupdateimport) | **PATCH** /repos/{owner}/{repo}/import | Update an import
*OidcApi* | [**oidcGetOidcCustomSubTemplateForOrg**](Apis/OidcApi.http#oidcgetoidccustomsubtemplatefororg) | **GET** /orgs/{org}/actions/oidc/customization/sub | Get the customization template for an OIDC subject claim for an organization
*OidcApi* | [**oidcUpdateOidcCustomSubTemplateForOrg**](Apis/OidcApi.http#oidcupdateoidccustomsubtemplatefororg) | **PUT** /orgs/{org}/actions/oidc/customization/sub | Set the customization template for an OIDC subject claim for an organization
*OrgsApi* | [**orgsAddSecurityManagerTeam**](Apis/OrgsApi.http#orgsaddsecuritymanagerteam) | **PUT** /orgs/{org}/security-managers/teams/{team_slug} | Add a security manager team
*OrgsApi* | [**orgsAssignTeamToOrgRole**](Apis/OrgsApi.http#orgsassignteamtoorgrole) | **PUT** /orgs/{org}/organization-roles/teams/{team_slug}/{role_id} | Assign an organization role to a team
*OrgsApi* | [**orgsAssignUserToOrgRole**](Apis/OrgsApi.http#orgsassignusertoorgrole) | **PUT** /orgs/{org}/organization-roles/users/{username}/{role_id} | Assign an organization role to a user
*OrgsApi* | [**orgsBlockUser**](Apis/OrgsApi.http#orgsblockuser) | **PUT** /orgs/{org}/blocks/{username} | Block a user from an organization
*OrgsApi* | [**orgsCancelInvitation**](Apis/OrgsApi.http#orgscancelinvitation) | **DELETE** /orgs/{org}/invitations/{invitation_id} | Cancel an organization invitation
*OrgsApi* | [**orgsCheckBlockedUser**](Apis/OrgsApi.http#orgscheckblockeduser) | **GET** /orgs/{org}/blocks/{username} | Check if a user is blocked by an organization
*OrgsApi* | [**orgsCheckMembershipForUser**](Apis/OrgsApi.http#orgscheckmembershipforuser) | **GET** /orgs/{org}/members/{username} | Check organization membership for a user
*OrgsApi* | [**orgsCheckPublicMembershipForUser**](Apis/OrgsApi.http#orgscheckpublicmembershipforuser) | **GET** /orgs/{org}/public_members/{username} | Check public organization membership for a user
*OrgsApi* | [**orgsConvertMemberToOutsideCollaborator**](Apis/OrgsApi.http#orgsconvertmembertooutsidecollaborator) | **PUT** /orgs/{org}/outside_collaborators/{username} | Convert an organization member to outside collaborator
*OrgsApi* | [**orgsCreateCustomOrganizationRole**](Apis/OrgsApi.http#orgscreatecustomorganizationrole) | **POST** /orgs/{org}/organization-roles | Create a custom organization role
*OrgsApi* | [**orgsCreateInvitation**](Apis/OrgsApi.http#orgscreateinvitation) | **POST** /orgs/{org}/invitations | Create an organization invitation
*OrgsApi* | [**orgsCreateOrUpdateCustomProperties**](Apis/OrgsApi.http#orgscreateorupdatecustomproperties) | **PATCH** /orgs/{org}/properties/schema | Create or update custom properties for an organization
*OrgsApi* | [**orgsCreateOrUpdateCustomPropertiesValuesForRepos**](Apis/OrgsApi.http#orgscreateorupdatecustompropertiesvaluesforrepos) | **PATCH** /orgs/{org}/properties/values | Create or update custom property values for organization repositories
*OrgsApi* | [**orgsCreateOrUpdateCustomProperty**](Apis/OrgsApi.http#orgscreateorupdatecustomproperty) | **PUT** /orgs/{org}/properties/schema/{custom_property_name} | Create or update a custom property for an organization
*OrgsApi* | [**orgsCreateWebhook**](Apis/OrgsApi.http#orgscreatewebhook) | **POST** /orgs/{org}/hooks | Create an organization webhook
*OrgsApi* | [**orgsDelete**](Apis/OrgsApi.http#orgsdelete) | **DELETE** /orgs/{org} | Delete an organization
*OrgsApi* | [**orgsDeleteCustomOrganizationRole**](Apis/OrgsApi.http#orgsdeletecustomorganizationrole) | **DELETE** /orgs/{org}/organization-roles/{role_id} | Delete a custom organization role.
*OrgsApi* | [**orgsDeleteWebhook**](Apis/OrgsApi.http#orgsdeletewebhook) | **DELETE** /orgs/{org}/hooks/{hook_id} | Delete an organization webhook
*OrgsApi* | [**orgsEnableOrDisableSecurityProductOnAllOrgRepos**](Apis/OrgsApi.http#orgsenableordisablesecurityproductonallorgrepos) | **POST** /orgs/{org}/{security_product}/{enablement} | Enable or disable a security feature for an organization
*OrgsApi* | [**orgsGet**](Apis/OrgsApi.http#orgsget) | **GET** /orgs/{org} | Get an organization
*OrgsApi* | [**orgsGetAllCustomProperties**](Apis/OrgsApi.http#orgsgetallcustomproperties) | **GET** /orgs/{org}/properties/schema | Get all custom properties for an organization
*OrgsApi* | [**orgsGetCustomProperty**](Apis/OrgsApi.http#orgsgetcustomproperty) | **GET** /orgs/{org}/properties/schema/{custom_property_name} | Get a custom property for an organization
*OrgsApi* | [**orgsGetMembershipForAuthenticatedUser**](Apis/OrgsApi.http#orgsgetmembershipforauthenticateduser) | **GET** /user/memberships/orgs/{org} | Get an organization membership for the authenticated user
*OrgsApi* | [**orgsGetMembershipForUser**](Apis/OrgsApi.http#orgsgetmembershipforuser) | **GET** /orgs/{org}/memberships/{username} | Get organization membership for a user
*OrgsApi* | [**orgsGetOrgRole**](Apis/OrgsApi.http#orgsgetorgrole) | **GET** /orgs/{org}/organization-roles/{role_id} | Get an organization role
*OrgsApi* | [**orgsGetWebhook**](Apis/OrgsApi.http#orgsgetwebhook) | **GET** /orgs/{org}/hooks/{hook_id} | Get an organization webhook
*OrgsApi* | [**orgsGetWebhookConfigForOrg**](Apis/OrgsApi.http#orgsgetwebhookconfigfororg) | **GET** /orgs/{org}/hooks/{hook_id}/config | Get a webhook configuration for an organization
*OrgsApi* | [**orgsGetWebhookDelivery**](Apis/OrgsApi.http#orgsgetwebhookdelivery) | **GET** /orgs/{org}/hooks/{hook_id}/deliveries/{delivery_id} | Get a webhook delivery for an organization webhook
*OrgsApi* | [**orgsList**](Apis/OrgsApi.http#orgslist) | **GET** /organizations | List organizations
*OrgsApi* | [**orgsListAppInstallations**](Apis/OrgsApi.http#orgslistappinstallations) | **GET** /orgs/{org}/installations | List app installations for an organization
*OrgsApi* | [**orgsListBlockedUsers**](Apis/OrgsApi.http#orgslistblockedusers) | **GET** /orgs/{org}/blocks | List users blocked by an organization
*OrgsApi* | [**orgsListCustomPropertiesValuesForRepos**](Apis/OrgsApi.http#orgslistcustompropertiesvaluesforrepos) | **GET** /orgs/{org}/properties/values | List custom property values for organization repositories
*OrgsApi* | [**orgsListFailedInvitations**](Apis/OrgsApi.http#orgslistfailedinvitations) | **GET** /orgs/{org}/failed_invitations | List failed organization invitations
*OrgsApi* | [**orgsListForAuthenticatedUser**](Apis/OrgsApi.http#orgslistforauthenticateduser) | **GET** /user/orgs | List organizations for the authenticated user
*OrgsApi* | [**orgsListForUser**](Apis/OrgsApi.http#orgslistforuser) | **GET** /users/{username}/orgs | List organizations for a user
*OrgsApi* | [**orgsListInvitationTeams**](Apis/OrgsApi.http#orgslistinvitationteams) | **GET** /orgs/{org}/invitations/{invitation_id}/teams | List organization invitation teams
*OrgsApi* | [**orgsListMembers**](Apis/OrgsApi.http#orgslistmembers) | **GET** /orgs/{org}/members | List organization members
*OrgsApi* | [**orgsListMembershipsForAuthenticatedUser**](Apis/OrgsApi.http#orgslistmembershipsforauthenticateduser) | **GET** /user/memberships/orgs | List organization memberships for the authenticated user
*OrgsApi* | [**orgsListOrgRoleTeams**](Apis/OrgsApi.http#orgslistorgroleteams) | **GET** /orgs/{org}/organization-roles/{role_id}/teams | List teams that are assigned to an organization role
*OrgsApi* | [**orgsListOrgRoleUsers**](Apis/OrgsApi.http#orgslistorgroleusers) | **GET** /orgs/{org}/organization-roles/{role_id}/users | List users that are assigned to an organization role
*OrgsApi* | [**orgsListOrgRoles**](Apis/OrgsApi.http#orgslistorgroles) | **GET** /orgs/{org}/organization-roles | Get all organization roles for an organization
*OrgsApi* | [**orgsListOrganizationFineGrainedPermissions**](Apis/OrgsApi.http#orgslistorganizationfinegrainedpermissions) | **GET** /orgs/{org}/organization-fine-grained-permissions | List organization fine-grained permissions for an organization
*OrgsApi* | [**orgsListOutsideCollaborators**](Apis/OrgsApi.http#orgslistoutsidecollaborators) | **GET** /orgs/{org}/outside_collaborators | List outside collaborators for an organization
*OrgsApi* | [**orgsListPatGrantRepositories**](Apis/OrgsApi.http#orgslistpatgrantrepositories) | **GET** /orgs/{org}/personal-access-tokens/{pat_id}/repositories | List repositories a fine-grained personal access token has access to
*OrgsApi* | [**orgsListPatGrantRequestRepositories**](Apis/OrgsApi.http#orgslistpatgrantrequestrepositories) | **GET** /orgs/{org}/personal-access-token-requests/{pat_request_id}/repositories | List repositories requested to be accessed by a fine-grained personal access token
*OrgsApi* | [**orgsListPatGrantRequests**](Apis/OrgsApi.http#orgslistpatgrantrequests) | **GET** /orgs/{org}/personal-access-token-requests | List requests to access organization resources with fine-grained personal access tokens
*OrgsApi* | [**orgsListPatGrants**](Apis/OrgsApi.http#orgslistpatgrants) | **GET** /orgs/{org}/personal-access-tokens | List fine-grained personal access tokens with access to organization resources
*OrgsApi* | [**orgsListPendingInvitations**](Apis/OrgsApi.http#orgslistpendinginvitations) | **GET** /orgs/{org}/invitations | List pending organization invitations
*OrgsApi* | [**orgsListPublicMembers**](Apis/OrgsApi.http#orgslistpublicmembers) | **GET** /orgs/{org}/public_members | List public organization members
*OrgsApi* | [**orgsListSecurityManagerTeams**](Apis/OrgsApi.http#orgslistsecuritymanagerteams) | **GET** /orgs/{org}/security-managers | List security manager teams
*OrgsApi* | [**orgsListWebhookDeliveries**](Apis/OrgsApi.http#orgslistwebhookdeliveries) | **GET** /orgs/{org}/hooks/{hook_id}/deliveries | List deliveries for an organization webhook
*OrgsApi* | [**orgsListWebhooks**](Apis/OrgsApi.http#orgslistwebhooks) | **GET** /orgs/{org}/hooks | List organization webhooks
*OrgsApi* | [**orgsPatchCustomOrganizationRole**](Apis/OrgsApi.http#orgspatchcustomorganizationrole) | **PATCH** /orgs/{org}/organization-roles/{role_id} | Update a custom organization role
*OrgsApi* | [**orgsPingWebhook**](Apis/OrgsApi.http#orgspingwebhook) | **POST** /orgs/{org}/hooks/{hook_id}/pings | Ping an organization webhook
*OrgsApi* | [**orgsRedeliverWebhookDelivery**](Apis/OrgsApi.http#orgsredeliverwebhookdelivery) | **POST** /orgs/{org}/hooks/{hook_id}/deliveries/{delivery_id}/attempts | Redeliver a delivery for an organization webhook
*OrgsApi* | [**orgsRemoveCustomProperty**](Apis/OrgsApi.http#orgsremovecustomproperty) | **DELETE** /orgs/{org}/properties/schema/{custom_property_name} | Remove a custom property for an organization
*OrgsApi* | [**orgsRemoveMember**](Apis/OrgsApi.http#orgsremovemember) | **DELETE** /orgs/{org}/members/{username} | Remove an organization member
*OrgsApi* | [**orgsRemoveMembershipForUser**](Apis/OrgsApi.http#orgsremovemembershipforuser) | **DELETE** /orgs/{org}/memberships/{username} | Remove organization membership for a user
*OrgsApi* | [**orgsRemoveOutsideCollaborator**](Apis/OrgsApi.http#orgsremoveoutsidecollaborator) | **DELETE** /orgs/{org}/outside_collaborators/{username} | Remove outside collaborator from an organization
*OrgsApi* | [**orgsRemovePublicMembershipForAuthenticatedUser**](Apis/OrgsApi.http#orgsremovepublicmembershipforauthenticateduser) | **DELETE** /orgs/{org}/public_members/{username} | Remove public organization membership for the authenticated user
*OrgsApi* | [**orgsRemoveSecurityManagerTeam**](Apis/OrgsApi.http#orgsremovesecuritymanagerteam) | **DELETE** /orgs/{org}/security-managers/teams/{team_slug} | Remove a security manager team
*OrgsApi* | [**orgsReviewPatGrantRequest**](Apis/OrgsApi.http#orgsreviewpatgrantrequest) | **POST** /orgs/{org}/personal-access-token-requests/{pat_request_id} | Review a request to access organization resources with a fine-grained personal access token
*OrgsApi* | [**orgsReviewPatGrantRequestsInBulk**](Apis/OrgsApi.http#orgsreviewpatgrantrequestsinbulk) | **POST** /orgs/{org}/personal-access-token-requests | Review requests to access organization resources with fine-grained personal access tokens
*OrgsApi* | [**orgsRevokeAllOrgRolesTeam**](Apis/OrgsApi.http#orgsrevokeallorgrolesteam) | **DELETE** /orgs/{org}/organization-roles/teams/{team_slug} | Remove all organization roles for a team
*OrgsApi* | [**orgsRevokeAllOrgRolesUser**](Apis/OrgsApi.http#orgsrevokeallorgrolesuser) | **DELETE** /orgs/{org}/organization-roles/users/{username} | Remove all organization roles for a user
*OrgsApi* | [**orgsRevokeOrgRoleTeam**](Apis/OrgsApi.http#orgsrevokeorgroleteam) | **DELETE** /orgs/{org}/organization-roles/teams/{team_slug}/{role_id} | Remove an organization role from a team
*OrgsApi* | [**orgsRevokeOrgRoleUser**](Apis/OrgsApi.http#orgsrevokeorgroleuser) | **DELETE** /orgs/{org}/organization-roles/users/{username}/{role_id} | Remove an organization role from a user
*OrgsApi* | [**orgsSetMembershipForUser**](Apis/OrgsApi.http#orgssetmembershipforuser) | **PUT** /orgs/{org}/memberships/{username} | Set organization membership for a user
*OrgsApi* | [**orgsSetPublicMembershipForAuthenticatedUser**](Apis/OrgsApi.http#orgssetpublicmembershipforauthenticateduser) | **PUT** /orgs/{org}/public_members/{username} | Set public organization membership for the authenticated user
*OrgsApi* | [**orgsUnblockUser**](Apis/OrgsApi.http#orgsunblockuser) | **DELETE** /orgs/{org}/blocks/{username} | Unblock a user from an organization
*OrgsApi* | [**orgsUpdate**](Apis/OrgsApi.http#orgsupdate) | **PATCH** /orgs/{org} | Update an organization
*OrgsApi* | [**orgsUpdateMembershipForAuthenticatedUser**](Apis/OrgsApi.http#orgsupdatemembershipforauthenticateduser) | **PATCH** /user/memberships/orgs/{org} | Update an organization membership for the authenticated user
*OrgsApi* | [**orgsUpdatePatAccess**](Apis/OrgsApi.http#orgsupdatepataccess) | **POST** /orgs/{org}/personal-access-tokens/{pat_id} | Update the access a fine-grained personal access token has to organization resources
*OrgsApi* | [**orgsUpdatePatAccesses**](Apis/OrgsApi.http#orgsupdatepataccesses) | **POST** /orgs/{org}/personal-access-tokens | Update the access to organization resources via fine-grained personal access tokens
*OrgsApi* | [**orgsUpdateWebhook**](Apis/OrgsApi.http#orgsupdatewebhook) | **PATCH** /orgs/{org}/hooks/{hook_id} | Update an organization webhook
*OrgsApi* | [**orgsUpdateWebhookConfigForOrg**](Apis/OrgsApi.http#orgsupdatewebhookconfigfororg) | **PATCH** /orgs/{org}/hooks/{hook_id}/config | Update a webhook configuration for an organization
*PackagesApi* | [**packagesDeletePackageForAuthenticatedUser**](Apis/PackagesApi.http#packagesdeletepackageforauthenticateduser) | **DELETE** /user/packages/{package_type}/{package_name} | Delete a package for the authenticated user
*PackagesApi* | [**packagesDeletePackageForOrg**](Apis/PackagesApi.http#packagesdeletepackagefororg) | **DELETE** /orgs/{org}/packages/{package_type}/{package_name} | Delete a package for an organization
*PackagesApi* | [**packagesDeletePackageForUser**](Apis/PackagesApi.http#packagesdeletepackageforuser) | **DELETE** /users/{username}/packages/{package_type}/{package_name} | Delete a package for a user
*PackagesApi* | [**packagesDeletePackageVersionForAuthenticatedUser**](Apis/PackagesApi.http#packagesdeletepackageversionforauthenticateduser) | **DELETE** /user/packages/{package_type}/{package_name}/versions/{package_version_id} | Delete a package version for the authenticated user
*PackagesApi* | [**packagesDeletePackageVersionForOrg**](Apis/PackagesApi.http#packagesdeletepackageversionfororg) | **DELETE** /orgs/{org}/packages/{package_type}/{package_name}/versions/{package_version_id} | Delete package version for an organization
*PackagesApi* | [**packagesDeletePackageVersionForUser**](Apis/PackagesApi.http#packagesdeletepackageversionforuser) | **DELETE** /users/{username}/packages/{package_type}/{package_name}/versions/{package_version_id} | Delete package version for a user
*PackagesApi* | [**packagesGetAllPackageVersionsForPackageOwnedByAuthenticatedUser**](Apis/PackagesApi.http#packagesgetallpackageversionsforpackageownedbyauthenticateduser) | **GET** /user/packages/{package_type}/{package_name}/versions | List package versions for a package owned by the authenticated user
*PackagesApi* | [**packagesGetAllPackageVersionsForPackageOwnedByOrg**](Apis/PackagesApi.http#packagesgetallpackageversionsforpackageownedbyorg) | **GET** /orgs/{org}/packages/{package_type}/{package_name}/versions | List package versions for a package owned by an organization
*PackagesApi* | [**packagesGetAllPackageVersionsForPackageOwnedByUser**](Apis/PackagesApi.http#packagesgetallpackageversionsforpackageownedbyuser) | **GET** /users/{username}/packages/{package_type}/{package_name}/versions | List package versions for a package owned by a user
*PackagesApi* | [**packagesGetPackageForAuthenticatedUser**](Apis/PackagesApi.http#packagesgetpackageforauthenticateduser) | **GET** /user/packages/{package_type}/{package_name} | Get a package for the authenticated user
*PackagesApi* | [**packagesGetPackageForOrganization**](Apis/PackagesApi.http#packagesgetpackagefororganization) | **GET** /orgs/{org}/packages/{package_type}/{package_name} | Get a package for an organization
*PackagesApi* | [**packagesGetPackageForUser**](Apis/PackagesApi.http#packagesgetpackageforuser) | **GET** /users/{username}/packages/{package_type}/{package_name} | Get a package for a user
*PackagesApi* | [**packagesGetPackageVersionForAuthenticatedUser**](Apis/PackagesApi.http#packagesgetpackageversionforauthenticateduser) | **GET** /user/packages/{package_type}/{package_name}/versions/{package_version_id} | Get a package version for the authenticated user
*PackagesApi* | [**packagesGetPackageVersionForOrganization**](Apis/PackagesApi.http#packagesgetpackageversionfororganization) | **GET** /orgs/{org}/packages/{package_type}/{package_name}/versions/{package_version_id} | Get a package version for an organization
*PackagesApi* | [**packagesGetPackageVersionForUser**](Apis/PackagesApi.http#packagesgetpackageversionforuser) | **GET** /users/{username}/packages/{package_type}/{package_name}/versions/{package_version_id} | Get a package version for a user
*PackagesApi* | [**packagesListDockerMigrationConflictingPackagesForAuthenticatedUser**](Apis/PackagesApi.http#packageslistdockermigrationconflictingpackagesforauthenticateduser) | **GET** /user/docker/conflicts | Get list of conflicting packages during Docker migration for authenticated-user
*PackagesApi* | [**packagesListDockerMigrationConflictingPackagesForOrganization**](Apis/PackagesApi.http#packageslistdockermigrationconflictingpackagesfororganization) | **GET** /orgs/{org}/docker/conflicts | Get list of conflicting packages during Docker migration for organization
*PackagesApi* | [**packagesListDockerMigrationConflictingPackagesForUser**](Apis/PackagesApi.http#packageslistdockermigrationconflictingpackagesforuser) | **GET** /users/{username}/docker/conflicts | Get list of conflicting packages during Docker migration for user
*PackagesApi* | [**packagesListPackagesForAuthenticatedUser**](Apis/PackagesApi.http#packageslistpackagesforauthenticateduser) | **GET** /user/packages | List packages for the authenticated user's namespace
*PackagesApi* | [**packagesListPackagesForOrganization**](Apis/PackagesApi.http#packageslistpackagesfororganization) | **GET** /orgs/{org}/packages | List packages for an organization
*PackagesApi* | [**packagesListPackagesForUser**](Apis/PackagesApi.http#packageslistpackagesforuser) | **GET** /users/{username}/packages | List packages for a user
*PackagesApi* | [**packagesRestorePackageForAuthenticatedUser**](Apis/PackagesApi.http#packagesrestorepackageforauthenticateduser) | **POST** /user/packages/{package_type}/{package_name}/restore | Restore a package for the authenticated user
*PackagesApi* | [**packagesRestorePackageForOrg**](Apis/PackagesApi.http#packagesrestorepackagefororg) | **POST** /orgs/{org}/packages/{package_type}/{package_name}/restore | Restore a package for an organization
*PackagesApi* | [**packagesRestorePackageForUser**](Apis/PackagesApi.http#packagesrestorepackageforuser) | **POST** /users/{username}/packages/{package_type}/{package_name}/restore | Restore a package for a user
*PackagesApi* | [**packagesRestorePackageVersionForAuthenticatedUser**](Apis/PackagesApi.http#packagesrestorepackageversionforauthenticateduser) | **POST** /user/packages/{package_type}/{package_name}/versions/{package_version_id}/restore | Restore a package version for the authenticated user
*PackagesApi* | [**packagesRestorePackageVersionForOrg**](Apis/PackagesApi.http#packagesrestorepackageversionfororg) | **POST** /orgs/{org}/packages/{package_type}/{package_name}/versions/{package_version_id}/restore | Restore package version for an organization
*PackagesApi* | [**packagesRestorePackageVersionForUser**](Apis/PackagesApi.http#packagesrestorepackageversionforuser) | **POST** /users/{username}/packages/{package_type}/{package_name}/versions/{package_version_id}/restore | Restore package version for a user
*ProjectsApi* | [**projectsAddCollaborator**](Apis/ProjectsApi.http#projectsaddcollaborator) | **PUT** /projects/{project_id}/collaborators/{username} | Add project collaborator
*ProjectsApi* | [**projectsCreateCard**](Apis/ProjectsApi.http#projectscreatecard) | **POST** /projects/columns/{column_id}/cards | Create a project card
*ProjectsApi* | [**projectsCreateColumn**](Apis/ProjectsApi.http#projectscreatecolumn) | **POST** /projects/{project_id}/columns | Create a project column
*ProjectsApi* | [**projectsCreateForAuthenticatedUser**](Apis/ProjectsApi.http#projectscreateforauthenticateduser) | **POST** /user/projects | Create a user project
*ProjectsApi* | [**projectsCreateForOrg**](Apis/ProjectsApi.http#projectscreatefororg) | **POST** /orgs/{org}/projects | Create an organization project
*ProjectsApi* | [**projectsCreateForRepo**](Apis/ProjectsApi.http#projectscreateforrepo) | **POST** /repos/{owner}/{repo}/projects | Create a repository project
*ProjectsApi* | [**projectsDelete**](Apis/ProjectsApi.http#projectsdelete) | **DELETE** /projects/{project_id} | Delete a project
*ProjectsApi* | [**projectsDeleteCard**](Apis/ProjectsApi.http#projectsdeletecard) | **DELETE** /projects/columns/cards/{card_id} | Delete a project card
*ProjectsApi* | [**projectsDeleteColumn**](Apis/ProjectsApi.http#projectsdeletecolumn) | **DELETE** /projects/columns/{column_id} | Delete a project column
*ProjectsApi* | [**projectsGet**](Apis/ProjectsApi.http#projectsget) | **GET** /projects/{project_id} | Get a project
*ProjectsApi* | [**projectsGetCard**](Apis/ProjectsApi.http#projectsgetcard) | **GET** /projects/columns/cards/{card_id} | Get a project card
*ProjectsApi* | [**projectsGetColumn**](Apis/ProjectsApi.http#projectsgetcolumn) | **GET** /projects/columns/{column_id} | Get a project column
*ProjectsApi* | [**projectsGetPermissionForUser**](Apis/ProjectsApi.http#projectsgetpermissionforuser) | **GET** /projects/{project_id}/collaborators/{username}/permission | Get project permission for a user
*ProjectsApi* | [**projectsListCards**](Apis/ProjectsApi.http#projectslistcards) | **GET** /projects/columns/{column_id}/cards | List project cards
*ProjectsApi* | [**projectsListCollaborators**](Apis/ProjectsApi.http#projectslistcollaborators) | **GET** /projects/{project_id}/collaborators | List project collaborators
*ProjectsApi* | [**projectsListColumns**](Apis/ProjectsApi.http#projectslistcolumns) | **GET** /projects/{project_id}/columns | List project columns
*ProjectsApi* | [**projectsListForOrg**](Apis/ProjectsApi.http#projectslistfororg) | **GET** /orgs/{org}/projects | List organization projects
*ProjectsApi* | [**projectsListForRepo**](Apis/ProjectsApi.http#projectslistforrepo) | **GET** /repos/{owner}/{repo}/projects | List repository projects
*ProjectsApi* | [**projectsListForUser**](Apis/ProjectsApi.http#projectslistforuser) | **GET** /users/{username}/projects | List user projects
*ProjectsApi* | [**projectsMoveCard**](Apis/ProjectsApi.http#projectsmovecard) | **POST** /projects/columns/cards/{card_id}/moves | Move a project card
*ProjectsApi* | [**projectsMoveColumn**](Apis/ProjectsApi.http#projectsmovecolumn) | **POST** /projects/columns/{column_id}/moves | Move a project column
*ProjectsApi* | [**projectsRemoveCollaborator**](Apis/ProjectsApi.http#projectsremovecollaborator) | **DELETE** /projects/{project_id}/collaborators/{username} | Remove user as a collaborator
*ProjectsApi* | [**projectsUpdate**](Apis/ProjectsApi.http#projectsupdate) | **PATCH** /projects/{project_id} | Update a project
*ProjectsApi* | [**projectsUpdateCard**](Apis/ProjectsApi.http#projectsupdatecard) | **PATCH** /projects/columns/cards/{card_id} | Update an existing project card
*ProjectsApi* | [**projectsUpdateColumn**](Apis/ProjectsApi.http#projectsupdatecolumn) | **PATCH** /projects/columns/{column_id} | Update an existing project column
*PullsApi* | [**pullsCheckIfMerged**](Apis/PullsApi.http#pullscheckifmerged) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/merge | Check if a pull request has been merged
*PullsApi* | [**pullsCreate**](Apis/PullsApi.http#pullscreate) | **POST** /repos/{owner}/{repo}/pulls | Create a pull request
*PullsApi* | [**pullsCreateReplyForReviewComment**](Apis/PullsApi.http#pullscreatereplyforreviewcomment) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies | Create a reply for a review comment
*PullsApi* | [**pullsCreateReview**](Apis/PullsApi.http#pullscreatereview) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/reviews | Create a review for a pull request
*PullsApi* | [**pullsCreateReviewComment**](Apis/PullsApi.http#pullscreatereviewcomment) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/comments | Create a review comment for a pull request
*PullsApi* | [**pullsDeletePendingReview**](Apis/PullsApi.http#pullsdeletependingreview) | **DELETE** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id} | Delete a pending review for a pull request
*PullsApi* | [**pullsDeleteReviewComment**](Apis/PullsApi.http#pullsdeletereviewcomment) | **DELETE** /repos/{owner}/{repo}/pulls/comments/{comment_id} | Delete a review comment for a pull request
*PullsApi* | [**pullsDismissReview**](Apis/PullsApi.http#pullsdismissreview) | **PUT** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id}/dismissals | Dismiss a review for a pull request
*PullsApi* | [**pullsGet**](Apis/PullsApi.http#pullsget) | **GET** /repos/{owner}/{repo}/pulls/{pull_number} | Get a pull request
*PullsApi* | [**pullsGetReview**](Apis/PullsApi.http#pullsgetreview) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id} | Get a review for a pull request
*PullsApi* | [**pullsGetReviewComment**](Apis/PullsApi.http#pullsgetreviewcomment) | **GET** /repos/{owner}/{repo}/pulls/comments/{comment_id} | Get a review comment for a pull request
*PullsApi* | [**pullsList**](Apis/PullsApi.http#pullslist) | **GET** /repos/{owner}/{repo}/pulls | List pull requests
*PullsApi* | [**pullsListCommentsForReview**](Apis/PullsApi.http#pullslistcommentsforreview) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id}/comments | List comments for a pull request review
*PullsApi* | [**pullsListCommits**](Apis/PullsApi.http#pullslistcommits) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/commits | List commits on a pull request
*PullsApi* | [**pullsListFiles**](Apis/PullsApi.http#pullslistfiles) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/files | List pull requests files
*PullsApi* | [**pullsListRequestedReviewers**](Apis/PullsApi.http#pullslistrequestedreviewers) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/requested_reviewers | Get all requested reviewers for a pull request
*PullsApi* | [**pullsListReviewComments**](Apis/PullsApi.http#pullslistreviewcomments) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/comments | List review comments on a pull request
*PullsApi* | [**pullsListReviewCommentsForRepo**](Apis/PullsApi.http#pullslistreviewcommentsforrepo) | **GET** /repos/{owner}/{repo}/pulls/comments | List review comments in a repository
*PullsApi* | [**pullsListReviews**](Apis/PullsApi.http#pullslistreviews) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/reviews | List reviews for a pull request
*PullsApi* | [**pullsMerge**](Apis/PullsApi.http#pullsmerge) | **PUT** /repos/{owner}/{repo}/pulls/{pull_number}/merge | Merge a pull request
*PullsApi* | [**pullsRemoveRequestedReviewers**](Apis/PullsApi.http#pullsremoverequestedreviewers) | **DELETE** /repos/{owner}/{repo}/pulls/{pull_number}/requested_reviewers | Remove requested reviewers from a pull request
*PullsApi* | [**pullsRequestReviewers**](Apis/PullsApi.http#pullsrequestreviewers) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/requested_reviewers | Request reviewers for a pull request
*PullsApi* | [**pullsSubmitReview**](Apis/PullsApi.http#pullssubmitreview) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id}/events | Submit a review for a pull request
*PullsApi* | [**pullsUpdate**](Apis/PullsApi.http#pullsupdate) | **PATCH** /repos/{owner}/{repo}/pulls/{pull_number} | Update a pull request
*PullsApi* | [**pullsUpdateBranch**](Apis/PullsApi.http#pullsupdatebranch) | **PUT** /repos/{owner}/{repo}/pulls/{pull_number}/update-branch | Update a pull request branch
*PullsApi* | [**pullsUpdateReview**](Apis/PullsApi.http#pullsupdatereview) | **PUT** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id} | Update a review for a pull request
*PullsApi* | [**pullsUpdateReviewComment**](Apis/PullsApi.http#pullsupdatereviewcomment) | **PATCH** /repos/{owner}/{repo}/pulls/comments/{comment_id} | Update a review comment for a pull request
*RateLimitApi* | [**rateLimit/get**](Apis/RateLimitApi.http#ratelimit/get) | **GET** /rate_limit | Get rate limit status for the authenticated user
*ReactionsApi* | [**reactionsCreateForCommitComment**](Apis/ReactionsApi.http#reactionscreateforcommitcomment) | **POST** /repos/{owner}/{repo}/comments/{comment_id}/reactions | Create reaction for a commit comment
*ReactionsApi* | [**reactionsCreateForIssue**](Apis/ReactionsApi.http#reactionscreateforissue) | **POST** /repos/{owner}/{repo}/issues/{issue_number}/reactions | Create reaction for an issue
*ReactionsApi* | [**reactionsCreateForIssueComment**](Apis/ReactionsApi.http#reactionscreateforissuecomment) | **POST** /repos/{owner}/{repo}/issues/comments/{comment_id}/reactions | Create reaction for an issue comment
*ReactionsApi* | [**reactionsCreateForPullRequestReviewComment**](Apis/ReactionsApi.http#reactionscreateforpullrequestreviewcomment) | **POST** /repos/{owner}/{repo}/pulls/comments/{comment_id}/reactions | Create reaction for a pull request review comment
*ReactionsApi* | [**reactionsCreateForRelease**](Apis/ReactionsApi.http#reactionscreateforrelease) | **POST** /repos/{owner}/{repo}/releases/{release_id}/reactions | Create reaction for a release
*ReactionsApi* | [**reactionsCreateForTeamDiscussionCommentInOrg**](Apis/ReactionsApi.http#reactionscreateforteamdiscussioncommentinorg) | **POST** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number}/reactions | Create reaction for a team discussion comment
*ReactionsApi* | [**reactionsCreateForTeamDiscussionCommentLegacy**](Apis/ReactionsApi.http#reactionscreateforteamdiscussioncommentlegacy) | **POST** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number}/reactions | Create reaction for a team discussion comment (Legacy)
*ReactionsApi* | [**reactionsCreateForTeamDiscussionInOrg**](Apis/ReactionsApi.http#reactionscreateforteamdiscussioninorg) | **POST** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/reactions | Create reaction for a team discussion
*ReactionsApi* | [**reactionsCreateForTeamDiscussionLegacy**](Apis/ReactionsApi.http#reactionscreateforteamdiscussionlegacy) | **POST** /teams/{team_id}/discussions/{discussion_number}/reactions | Create reaction for a team discussion (Legacy)
*ReactionsApi* | [**reactionsDeleteForCommitComment**](Apis/ReactionsApi.http#reactionsdeleteforcommitcomment) | **DELETE** /repos/{owner}/{repo}/comments/{comment_id}/reactions/{reaction_id} | Delete a commit comment reaction
*ReactionsApi* | [**reactionsDeleteForIssue**](Apis/ReactionsApi.http#reactionsdeleteforissue) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/reactions/{reaction_id} | Delete an issue reaction
*ReactionsApi* | [**reactionsDeleteForIssueComment**](Apis/ReactionsApi.http#reactionsdeleteforissuecomment) | **DELETE** /repos/{owner}/{repo}/issues/comments/{comment_id}/reactions/{reaction_id} | Delete an issue comment reaction
*ReactionsApi* | [**reactionsDeleteForPullRequestComment**](Apis/ReactionsApi.http#reactionsdeleteforpullrequestcomment) | **DELETE** /repos/{owner}/{repo}/pulls/comments/{comment_id}/reactions/{reaction_id} | Delete a pull request comment reaction
*ReactionsApi* | [**reactionsDeleteForRelease**](Apis/ReactionsApi.http#reactionsdeleteforrelease) | **DELETE** /repos/{owner}/{repo}/releases/{release_id}/reactions/{reaction_id} | Delete a release reaction
*ReactionsApi* | [**reactionsDeleteForTeamDiscussion**](Apis/ReactionsApi.http#reactionsdeleteforteamdiscussion) | **DELETE** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/reactions/{reaction_id} | Delete team discussion reaction
*ReactionsApi* | [**reactionsDeleteForTeamDiscussionComment**](Apis/ReactionsApi.http#reactionsdeleteforteamdiscussioncomment) | **DELETE** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number}/reactions/{reaction_id} | Delete team discussion comment reaction
*ReactionsApi* | [**reactionsListForCommitComment**](Apis/ReactionsApi.http#reactionslistforcommitcomment) | **GET** /repos/{owner}/{repo}/comments/{comment_id}/reactions | List reactions for a commit comment
*ReactionsApi* | [**reactionsListForIssue**](Apis/ReactionsApi.http#reactionslistforissue) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/reactions | List reactions for an issue
*ReactionsApi* | [**reactionsListForIssueComment**](Apis/ReactionsApi.http#reactionslistforissuecomment) | **GET** /repos/{owner}/{repo}/issues/comments/{comment_id}/reactions | List reactions for an issue comment
*ReactionsApi* | [**reactionsListForPullRequestReviewComment**](Apis/ReactionsApi.http#reactionslistforpullrequestreviewcomment) | **GET** /repos/{owner}/{repo}/pulls/comments/{comment_id}/reactions | List reactions for a pull request review comment
*ReactionsApi* | [**reactionsListForRelease**](Apis/ReactionsApi.http#reactionslistforrelease) | **GET** /repos/{owner}/{repo}/releases/{release_id}/reactions | List reactions for a release
*ReactionsApi* | [**reactionsListForTeamDiscussionCommentInOrg**](Apis/ReactionsApi.http#reactionslistforteamdiscussioncommentinorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number}/reactions | List reactions for a team discussion comment
*ReactionsApi* | [**reactionsListForTeamDiscussionCommentLegacy**](Apis/ReactionsApi.http#reactionslistforteamdiscussioncommentlegacy) | **GET** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number}/reactions | List reactions for a team discussion comment (Legacy)
*ReactionsApi* | [**reactionsListForTeamDiscussionInOrg**](Apis/ReactionsApi.http#reactionslistforteamdiscussioninorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/reactions | List reactions for a team discussion
*ReactionsApi* | [**reactionsListForTeamDiscussionLegacy**](Apis/ReactionsApi.http#reactionslistforteamdiscussionlegacy) | **GET** /teams/{team_id}/discussions/{discussion_number}/reactions | List reactions for a team discussion (Legacy)
*ReposApi* | [**reposAcceptInvitationForAuthenticatedUser**](Apis/ReposApi.http#reposacceptinvitationforauthenticateduser) | **PATCH** /user/repository_invitations/{invitation_id} | Accept a repository invitation
*ReposApi* | [**reposAddAppAccessRestrictions**](Apis/ReposApi.http#reposaddappaccessrestrictions) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/apps | Add app access restrictions
*ReposApi* | [**reposAddCollaborator**](Apis/ReposApi.http#reposaddcollaborator) | **PUT** /repos/{owner}/{repo}/collaborators/{username} | Add a repository collaborator
*ReposApi* | [**reposAddStatusCheckContexts**](Apis/ReposApi.http#reposaddstatuscheckcontexts) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks/contexts | Add status check contexts
*ReposApi* | [**reposAddTeamAccessRestrictions**](Apis/ReposApi.http#reposaddteamaccessrestrictions) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/teams | Add team access restrictions
*ReposApi* | [**reposAddUserAccessRestrictions**](Apis/ReposApi.http#reposadduseraccessrestrictions) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/users | Add user access restrictions
*ReposApi* | [**reposCancelPagesDeployment**](Apis/ReposApi.http#reposcancelpagesdeployment) | **POST** /repos/{owner}/{repo}/pages/deployments/{pages_deployment_id}/cancel | Cancel a GitHub Pages deployment
*ReposApi* | [**reposCheckAutomatedSecurityFixes**](Apis/ReposApi.http#reposcheckautomatedsecurityfixes) | **GET** /repos/{owner}/{repo}/automated-security-fixes | Check if automated security fixes are enabled for a repository
*ReposApi* | [**reposCheckCollaborator**](Apis/ReposApi.http#reposcheckcollaborator) | **GET** /repos/{owner}/{repo}/collaborators/{username} | Check if a user is a repository collaborator
*ReposApi* | [**reposCheckVulnerabilityAlerts**](Apis/ReposApi.http#reposcheckvulnerabilityalerts) | **GET** /repos/{owner}/{repo}/vulnerability-alerts | Check if vulnerability alerts are enabled for a repository
*ReposApi* | [**reposCodeownersErrors**](Apis/ReposApi.http#reposcodeownerserrors) | **GET** /repos/{owner}/{repo}/codeowners/errors | List CODEOWNERS errors
*ReposApi* | [**reposCompareCommits**](Apis/ReposApi.http#reposcomparecommits) | **GET** /repos/{owner}/{repo}/compare/{basehead} | Compare two commits
*ReposApi* | [**reposCreateAutolink**](Apis/ReposApi.http#reposcreateautolink) | **POST** /repos/{owner}/{repo}/autolinks | Create an autolink reference for a repository
*ReposApi* | [**reposCreateCommitComment**](Apis/ReposApi.http#reposcreatecommitcomment) | **POST** /repos/{owner}/{repo}/commits/{commit_sha}/comments | Create a commit comment
*ReposApi* | [**reposCreateCommitSignatureProtection**](Apis/ReposApi.http#reposcreatecommitsignatureprotection) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/required_signatures | Create commit signature protection
*ReposApi* | [**reposCreateCommitStatus**](Apis/ReposApi.http#reposcreatecommitstatus) | **POST** /repos/{owner}/{repo}/statuses/{sha} | Create a commit status
*ReposApi* | [**reposCreateDeployKey**](Apis/ReposApi.http#reposcreatedeploykey) | **POST** /repos/{owner}/{repo}/keys | Create a deploy key
*ReposApi* | [**reposCreateDeployment**](Apis/ReposApi.http#reposcreatedeployment) | **POST** /repos/{owner}/{repo}/deployments | Create a deployment
*ReposApi* | [**reposCreateDeploymentBranchPolicy**](Apis/ReposApi.http#reposcreatedeploymentbranchpolicy) | **POST** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies | Create a deployment branch policy
*ReposApi* | [**reposCreateDeploymentProtectionRule**](Apis/ReposApi.http#reposcreatedeploymentprotectionrule) | **POST** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules | Create a custom deployment protection rule on an environment
*ReposApi* | [**reposCreateDeploymentStatus**](Apis/ReposApi.http#reposcreatedeploymentstatus) | **POST** /repos/{owner}/{repo}/deployments/{deployment_id}/statuses | Create a deployment status
*ReposApi* | [**reposCreateDispatchEvent**](Apis/ReposApi.http#reposcreatedispatchevent) | **POST** /repos/{owner}/{repo}/dispatches | Create a repository dispatch event
*ReposApi* | [**reposCreateForAuthenticatedUser**](Apis/ReposApi.http#reposcreateforauthenticateduser) | **POST** /user/repos | Create a repository for the authenticated user
*ReposApi* | [**reposCreateFork**](Apis/ReposApi.http#reposcreatefork) | **POST** /repos/{owner}/{repo}/forks | Create a fork
*ReposApi* | [**reposCreateInOrg**](Apis/ReposApi.http#reposcreateinorg) | **POST** /orgs/{org}/repos | Create an organization repository
*ReposApi* | [**reposCreateOrUpdateCustomPropertiesValues**](Apis/ReposApi.http#reposcreateorupdatecustompropertiesvalues) | **PATCH** /repos/{owner}/{repo}/properties/values | Create or update custom property values for a repository
*ReposApi* | [**reposCreateOrUpdateEnvironment**](Apis/ReposApi.http#reposcreateorupdateenvironment) | **PUT** /repos/{owner}/{repo}/environments/{environment_name} | Create or update an environment
*ReposApi* | [**reposCreateOrUpdateFileContents**](Apis/ReposApi.http#reposcreateorupdatefilecontents) | **PUT** /repos/{owner}/{repo}/contents/{path} | Create or update file contents
*ReposApi* | [**reposCreateOrgRuleset**](Apis/ReposApi.http#reposcreateorgruleset) | **POST** /orgs/{org}/rulesets | Create an organization repository ruleset
*ReposApi* | [**reposCreatePagesDeployment**](Apis/ReposApi.http#reposcreatepagesdeployment) | **POST** /repos/{owner}/{repo}/pages/deployments | Create a GitHub Pages deployment
*ReposApi* | [**reposCreatePagesSite**](Apis/ReposApi.http#reposcreatepagessite) | **POST** /repos/{owner}/{repo}/pages | Create a GitHub Pages site
*ReposApi* | [**reposCreateRelease**](Apis/ReposApi.http#reposcreaterelease) | **POST** /repos/{owner}/{repo}/releases | Create a release
*ReposApi* | [**reposCreateRepoRuleset**](Apis/ReposApi.http#reposcreatereporuleset) | **POST** /repos/{owner}/{repo}/rulesets | Create a repository ruleset
*ReposApi* | [**reposCreateTagProtection**](Apis/ReposApi.http#reposcreatetagprotection) | **POST** /repos/{owner}/{repo}/tags/protection | Create a tag protection state for a repository
*ReposApi* | [**reposCreateUsingTemplate**](Apis/ReposApi.http#reposcreateusingtemplate) | **POST** /repos/{template_owner}/{template_repo}/generate | Create a repository using a template
*ReposApi* | [**reposCreateWebhook**](Apis/ReposApi.http#reposcreatewebhook) | **POST** /repos/{owner}/{repo}/hooks | Create a repository webhook
*ReposApi* | [**reposDeclineInvitationForAuthenticatedUser**](Apis/ReposApi.http#reposdeclineinvitationforauthenticateduser) | **DELETE** /user/repository_invitations/{invitation_id} | Decline a repository invitation
*ReposApi* | [**reposDelete**](Apis/ReposApi.http#reposdelete) | **DELETE** /repos/{owner}/{repo} | Delete a repository
*ReposApi* | [**reposDeleteAccessRestrictions**](Apis/ReposApi.http#reposdeleteaccessrestrictions) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions | Delete access restrictions
*ReposApi* | [**reposDeleteAdminBranchProtection**](Apis/ReposApi.http#reposdeleteadminbranchprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/enforce_admins | Delete admin branch protection
*ReposApi* | [**reposDeleteAnEnvironment**](Apis/ReposApi.http#reposdeleteanenvironment) | **DELETE** /repos/{owner}/{repo}/environments/{environment_name} | Delete an environment
*ReposApi* | [**reposDeleteAutolink**](Apis/ReposApi.http#reposdeleteautolink) | **DELETE** /repos/{owner}/{repo}/autolinks/{autolink_id} | Delete an autolink reference from a repository
*ReposApi* | [**reposDeleteBranchProtection**](Apis/ReposApi.http#reposdeletebranchprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection | Delete branch protection
*ReposApi* | [**reposDeleteCommitComment**](Apis/ReposApi.http#reposdeletecommitcomment) | **DELETE** /repos/{owner}/{repo}/comments/{comment_id} | Delete a commit comment
*ReposApi* | [**reposDeleteCommitSignatureProtection**](Apis/ReposApi.http#reposdeletecommitsignatureprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/required_signatures | Delete commit signature protection
*ReposApi* | [**reposDeleteDeployKey**](Apis/ReposApi.http#reposdeletedeploykey) | **DELETE** /repos/{owner}/{repo}/keys/{key_id} | Delete a deploy key
*ReposApi* | [**reposDeleteDeployment**](Apis/ReposApi.http#reposdeletedeployment) | **DELETE** /repos/{owner}/{repo}/deployments/{deployment_id} | Delete a deployment
*ReposApi* | [**reposDeleteDeploymentBranchPolicy**](Apis/ReposApi.http#reposdeletedeploymentbranchpolicy) | **DELETE** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies/{branch_policy_id} | Delete a deployment branch policy
*ReposApi* | [**reposDeleteFile**](Apis/ReposApi.http#reposdeletefile) | **DELETE** /repos/{owner}/{repo}/contents/{path} | Delete a file
*ReposApi* | [**reposDeleteInvitation**](Apis/ReposApi.http#reposdeleteinvitation) | **DELETE** /repos/{owner}/{repo}/invitations/{invitation_id} | Delete a repository invitation
*ReposApi* | [**reposDeleteOrgRuleset**](Apis/ReposApi.http#reposdeleteorgruleset) | **DELETE** /orgs/{org}/rulesets/{ruleset_id} | Delete an organization repository ruleset
*ReposApi* | [**reposDeletePagesSite**](Apis/ReposApi.http#reposdeletepagessite) | **DELETE** /repos/{owner}/{repo}/pages | Delete a GitHub Pages site
*ReposApi* | [**reposDeletePullRequestReviewProtection**](Apis/ReposApi.http#reposdeletepullrequestreviewprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/required_pull_request_reviews | Delete pull request review protection
*ReposApi* | [**reposDeleteRelease**](Apis/ReposApi.http#reposdeleterelease) | **DELETE** /repos/{owner}/{repo}/releases/{release_id} | Delete a release
*ReposApi* | [**reposDeleteReleaseAsset**](Apis/ReposApi.http#reposdeletereleaseasset) | **DELETE** /repos/{owner}/{repo}/releases/assets/{asset_id} | Delete a release asset
*ReposApi* | [**reposDeleteRepoRuleset**](Apis/ReposApi.http#reposdeletereporuleset) | **DELETE** /repos/{owner}/{repo}/rulesets/{ruleset_id} | Delete a repository ruleset
*ReposApi* | [**reposDeleteTagProtection**](Apis/ReposApi.http#reposdeletetagprotection) | **DELETE** /repos/{owner}/{repo}/tags/protection/{tag_protection_id} | Delete a tag protection state for a repository
*ReposApi* | [**reposDeleteWebhook**](Apis/ReposApi.http#reposdeletewebhook) | **DELETE** /repos/{owner}/{repo}/hooks/{hook_id} | Delete a repository webhook
*ReposApi* | [**reposDisableAutomatedSecurityFixes**](Apis/ReposApi.http#reposdisableautomatedsecurityfixes) | **DELETE** /repos/{owner}/{repo}/automated-security-fixes | Disable automated security fixes
*ReposApi* | [**reposDisableDeploymentProtectionRule**](Apis/ReposApi.http#reposdisabledeploymentprotectionrule) | **DELETE** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules/{protection_rule_id} | Disable a custom protection rule for an environment
*ReposApi* | [**reposDisablePrivateVulnerabilityReporting**](Apis/ReposApi.http#reposdisableprivatevulnerabilityreporting) | **DELETE** /repos/{owner}/{repo}/private-vulnerability-reporting | Disable private vulnerability reporting for a repository
*ReposApi* | [**reposDisableVulnerabilityAlerts**](Apis/ReposApi.http#reposdisablevulnerabilityalerts) | **DELETE** /repos/{owner}/{repo}/vulnerability-alerts | Disable vulnerability alerts
*ReposApi* | [**reposDownloadTarballArchive**](Apis/ReposApi.http#reposdownloadtarballarchive) | **GET** /repos/{owner}/{repo}/tarball/{ref} | Download a repository archive (tar)
*ReposApi* | [**reposDownloadZipballArchive**](Apis/ReposApi.http#reposdownloadzipballarchive) | **GET** /repos/{owner}/{repo}/zipball/{ref} | Download a repository archive (zip)
*ReposApi* | [**reposEnableAutomatedSecurityFixes**](Apis/ReposApi.http#reposenableautomatedsecurityfixes) | **PUT** /repos/{owner}/{repo}/automated-security-fixes | Enable automated security fixes
*ReposApi* | [**reposEnablePrivateVulnerabilityReporting**](Apis/ReposApi.http#reposenableprivatevulnerabilityreporting) | **PUT** /repos/{owner}/{repo}/private-vulnerability-reporting | Enable private vulnerability reporting for a repository
*ReposApi* | [**reposEnableVulnerabilityAlerts**](Apis/ReposApi.http#reposenablevulnerabilityalerts) | **PUT** /repos/{owner}/{repo}/vulnerability-alerts | Enable vulnerability alerts
*ReposApi* | [**reposGenerateReleaseNotes**](Apis/ReposApi.http#reposgeneratereleasenotes) | **POST** /repos/{owner}/{repo}/releases/generate-notes | Generate release notes content for a release
*ReposApi* | [**reposGet**](Apis/ReposApi.http#reposget) | **GET** /repos/{owner}/{repo} | Get a repository
*ReposApi* | [**reposGetAccessRestrictions**](Apis/ReposApi.http#reposgetaccessrestrictions) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions | Get access restrictions
*ReposApi* | [**reposGetAdminBranchProtection**](Apis/ReposApi.http#reposgetadminbranchprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/enforce_admins | Get admin branch protection
*ReposApi* | [**reposGetAllDeploymentProtectionRules**](Apis/ReposApi.http#reposgetalldeploymentprotectionrules) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules | Get all deployment protection rules for an environment
*ReposApi* | [**reposGetAllEnvironments**](Apis/ReposApi.http#reposgetallenvironments) | **GET** /repos/{owner}/{repo}/environments | List environments
*ReposApi* | [**reposGetAllStatusCheckContexts**](Apis/ReposApi.http#reposgetallstatuscheckcontexts) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks/contexts | Get all status check contexts
*ReposApi* | [**reposGetAllTopics**](Apis/ReposApi.http#reposgetalltopics) | **GET** /repos/{owner}/{repo}/topics | Get all repository topics
*ReposApi* | [**reposGetAppsWithAccessToProtectedBranch**](Apis/ReposApi.http#reposgetappswithaccesstoprotectedbranch) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/apps | Get apps with access to the protected branch
*ReposApi* | [**reposGetAutolink**](Apis/ReposApi.http#reposgetautolink) | **GET** /repos/{owner}/{repo}/autolinks/{autolink_id} | Get an autolink reference of a repository
*ReposApi* | [**reposGetBranch**](Apis/ReposApi.http#reposgetbranch) | **GET** /repos/{owner}/{repo}/branches/{branch} | Get a branch
*ReposApi* | [**reposGetBranchProtection**](Apis/ReposApi.http#reposgetbranchprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection | Get branch protection
*ReposApi* | [**reposGetBranchRules**](Apis/ReposApi.http#reposgetbranchrules) | **GET** /repos/{owner}/{repo}/rules/branches/{branch} | Get rules for a branch
*ReposApi* | [**reposGetClones**](Apis/ReposApi.http#reposgetclones) | **GET** /repos/{owner}/{repo}/traffic/clones | Get repository clones
*ReposApi* | [**reposGetCodeFrequencyStats**](Apis/ReposApi.http#reposgetcodefrequencystats) | **GET** /repos/{owner}/{repo}/stats/code_frequency | Get the weekly commit activity
*ReposApi* | [**reposGetCollaboratorPermissionLevel**](Apis/ReposApi.http#reposgetcollaboratorpermissionlevel) | **GET** /repos/{owner}/{repo}/collaborators/{username}/permission | Get repository permissions for a user
*ReposApi* | [**reposGetCombinedStatusForRef**](Apis/ReposApi.http#reposgetcombinedstatusforref) | **GET** /repos/{owner}/{repo}/commits/{ref}/status | Get the combined status for a specific reference
*ReposApi* | [**reposGetCommit**](Apis/ReposApi.http#reposgetcommit) | **GET** /repos/{owner}/{repo}/commits/{ref} | Get a commit
*ReposApi* | [**reposGetCommitActivityStats**](Apis/ReposApi.http#reposgetcommitactivitystats) | **GET** /repos/{owner}/{repo}/stats/commit_activity | Get the last year of commit activity
*ReposApi* | [**reposGetCommitComment**](Apis/ReposApi.http#reposgetcommitcomment) | **GET** /repos/{owner}/{repo}/comments/{comment_id} | Get a commit comment
*ReposApi* | [**reposGetCommitSignatureProtection**](Apis/ReposApi.http#reposgetcommitsignatureprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/required_signatures | Get commit signature protection
*ReposApi* | [**reposGetCommunityProfileMetrics**](Apis/ReposApi.http#reposgetcommunityprofilemetrics) | **GET** /repos/{owner}/{repo}/community/profile | Get community profile metrics
*ReposApi* | [**reposGetContent**](Apis/ReposApi.http#reposgetcontent) | **GET** /repos/{owner}/{repo}/contents/{path} | Get repository content
*ReposApi* | [**reposGetContributorsStats**](Apis/ReposApi.http#reposgetcontributorsstats) | **GET** /repos/{owner}/{repo}/stats/contributors | Get all contributor commit activity
*ReposApi* | [**reposGetCustomDeploymentProtectionRule**](Apis/ReposApi.http#reposgetcustomdeploymentprotectionrule) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules/{protection_rule_id} | Get a custom deployment protection rule
*ReposApi* | [**reposGetCustomPropertiesValues**](Apis/ReposApi.http#reposgetcustompropertiesvalues) | **GET** /repos/{owner}/{repo}/properties/values | Get all custom property values for a repository
*ReposApi* | [**reposGetDeployKey**](Apis/ReposApi.http#reposgetdeploykey) | **GET** /repos/{owner}/{repo}/keys/{key_id} | Get a deploy key
*ReposApi* | [**reposGetDeployment**](Apis/ReposApi.http#reposgetdeployment) | **GET** /repos/{owner}/{repo}/deployments/{deployment_id} | Get a deployment
*ReposApi* | [**reposGetDeploymentBranchPolicy**](Apis/ReposApi.http#reposgetdeploymentbranchpolicy) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies/{branch_policy_id} | Get a deployment branch policy
*ReposApi* | [**reposGetDeploymentStatus**](Apis/ReposApi.http#reposgetdeploymentstatus) | **GET** /repos/{owner}/{repo}/deployments/{deployment_id}/statuses/{status_id} | Get a deployment status
*ReposApi* | [**reposGetEnvironment**](Apis/ReposApi.http#reposgetenvironment) | **GET** /repos/{owner}/{repo}/environments/{environment_name} | Get an environment
*ReposApi* | [**reposGetLatestPagesBuild**](Apis/ReposApi.http#reposgetlatestpagesbuild) | **GET** /repos/{owner}/{repo}/pages/builds/latest | Get latest Pages build
*ReposApi* | [**reposGetLatestRelease**](Apis/ReposApi.http#reposgetlatestrelease) | **GET** /repos/{owner}/{repo}/releases/latest | Get the latest release
*ReposApi* | [**reposGetOrgRuleSuite**](Apis/ReposApi.http#reposgetorgrulesuite) | **GET** /orgs/{org}/rulesets/rule-suites/{rule_suite_id} | Get an organization rule suite
*ReposApi* | [**reposGetOrgRuleSuites**](Apis/ReposApi.http#reposgetorgrulesuites) | **GET** /orgs/{org}/rulesets/rule-suites | List organization rule suites
*ReposApi* | [**reposGetOrgRuleset**](Apis/ReposApi.http#reposgetorgruleset) | **GET** /orgs/{org}/rulesets/{ruleset_id} | Get an organization repository ruleset
*ReposApi* | [**reposGetOrgRulesets**](Apis/ReposApi.http#reposgetorgrulesets) | **GET** /orgs/{org}/rulesets | Get all organization repository rulesets
*ReposApi* | [**reposGetPages**](Apis/ReposApi.http#reposgetpages) | **GET** /repos/{owner}/{repo}/pages | Get a GitHub Pages site
*ReposApi* | [**reposGetPagesBuild**](Apis/ReposApi.http#reposgetpagesbuild) | **GET** /repos/{owner}/{repo}/pages/builds/{build_id} | Get GitHub Pages build
*ReposApi* | [**reposGetPagesDeployment**](Apis/ReposApi.http#reposgetpagesdeployment) | **GET** /repos/{owner}/{repo}/pages/deployments/{pages_deployment_id} | Get the status of a GitHub Pages deployment
*ReposApi* | [**reposGetPagesHealthCheck**](Apis/ReposApi.http#reposgetpageshealthcheck) | **GET** /repos/{owner}/{repo}/pages/health | Get a DNS health check for GitHub Pages
*ReposApi* | [**reposGetParticipationStats**](Apis/ReposApi.http#reposgetparticipationstats) | **GET** /repos/{owner}/{repo}/stats/participation | Get the weekly commit count
*ReposApi* | [**reposGetPullRequestReviewProtection**](Apis/ReposApi.http#reposgetpullrequestreviewprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/required_pull_request_reviews | Get pull request review protection
*ReposApi* | [**reposGetPunchCardStats**](Apis/ReposApi.http#reposgetpunchcardstats) | **GET** /repos/{owner}/{repo}/stats/punch_card | Get the hourly commit count for each day
*ReposApi* | [**reposGetReadme**](Apis/ReposApi.http#reposgetreadme) | **GET** /repos/{owner}/{repo}/readme | Get a repository README
*ReposApi* | [**reposGetReadmeInDirectory**](Apis/ReposApi.http#reposgetreadmeindirectory) | **GET** /repos/{owner}/{repo}/readme/{dir} | Get a repository README for a directory
*ReposApi* | [**reposGetRelease**](Apis/ReposApi.http#reposgetrelease) | **GET** /repos/{owner}/{repo}/releases/{release_id} | Get a release
*ReposApi* | [**reposGetReleaseAsset**](Apis/ReposApi.http#reposgetreleaseasset) | **GET** /repos/{owner}/{repo}/releases/assets/{asset_id} | Get a release asset
*ReposApi* | [**reposGetReleaseByTag**](Apis/ReposApi.http#reposgetreleasebytag) | **GET** /repos/{owner}/{repo}/releases/tags/{tag} | Get a release by tag name
*ReposApi* | [**reposGetRepoRuleSuite**](Apis/ReposApi.http#reposgetreporulesuite) | **GET** /repos/{owner}/{repo}/rulesets/rule-suites/{rule_suite_id} | Get a repository rule suite
*ReposApi* | [**reposGetRepoRuleSuites**](Apis/ReposApi.http#reposgetreporulesuites) | **GET** /repos/{owner}/{repo}/rulesets/rule-suites | List repository rule suites
*ReposApi* | [**reposGetRepoRuleset**](Apis/ReposApi.http#reposgetreporuleset) | **GET** /repos/{owner}/{repo}/rulesets/{ruleset_id} | Get a repository ruleset
*ReposApi* | [**reposGetRepoRulesets**](Apis/ReposApi.http#reposgetreporulesets) | **GET** /repos/{owner}/{repo}/rulesets | Get all repository rulesets
*ReposApi* | [**reposGetStatusChecksProtection**](Apis/ReposApi.http#reposgetstatuschecksprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks | Get status checks protection
*ReposApi* | [**reposGetTeamsWithAccessToProtectedBranch**](Apis/ReposApi.http#reposgetteamswithaccesstoprotectedbranch) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/teams | Get teams with access to the protected branch
*ReposApi* | [**reposGetTopPaths**](Apis/ReposApi.http#reposgettoppaths) | **GET** /repos/{owner}/{repo}/traffic/popular/paths | Get top referral paths
*ReposApi* | [**reposGetTopReferrers**](Apis/ReposApi.http#reposgettopreferrers) | **GET** /repos/{owner}/{repo}/traffic/popular/referrers | Get top referral sources
*ReposApi* | [**reposGetUsersWithAccessToProtectedBranch**](Apis/ReposApi.http#reposgetuserswithaccesstoprotectedbranch) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/users | Get users with access to the protected branch
*ReposApi* | [**reposGetViews**](Apis/ReposApi.http#reposgetviews) | **GET** /repos/{owner}/{repo}/traffic/views | Get page views
*ReposApi* | [**reposGetWebhook**](Apis/ReposApi.http#reposgetwebhook) | **GET** /repos/{owner}/{repo}/hooks/{hook_id} | Get a repository webhook
*ReposApi* | [**reposGetWebhookConfigForRepo**](Apis/ReposApi.http#reposgetwebhookconfigforrepo) | **GET** /repos/{owner}/{repo}/hooks/{hook_id}/config | Get a webhook configuration for a repository
*ReposApi* | [**reposGetWebhookDelivery**](Apis/ReposApi.http#reposgetwebhookdelivery) | **GET** /repos/{owner}/{repo}/hooks/{hook_id}/deliveries/{delivery_id} | Get a delivery for a repository webhook
*ReposApi* | [**reposListActivities**](Apis/ReposApi.http#reposlistactivities) | **GET** /repos/{owner}/{repo}/activity | List repository activities
*ReposApi* | [**reposListAutolinks**](Apis/ReposApi.http#reposlistautolinks) | **GET** /repos/{owner}/{repo}/autolinks | Get all autolinks of a repository
*ReposApi* | [**reposListBranches**](Apis/ReposApi.http#reposlistbranches) | **GET** /repos/{owner}/{repo}/branches | List branches
*ReposApi* | [**reposListBranchesForHeadCommit**](Apis/ReposApi.http#reposlistbranchesforheadcommit) | **GET** /repos/{owner}/{repo}/commits/{commit_sha}/branches-where-head | List branches for HEAD commit
*ReposApi* | [**reposListCollaborators**](Apis/ReposApi.http#reposlistcollaborators) | **GET** /repos/{owner}/{repo}/collaborators | List repository collaborators
*ReposApi* | [**reposListCommentsForCommit**](Apis/ReposApi.http#reposlistcommentsforcommit) | **GET** /repos/{owner}/{repo}/commits/{commit_sha}/comments | List commit comments
*ReposApi* | [**reposListCommitCommentsForRepo**](Apis/ReposApi.http#reposlistcommitcommentsforrepo) | **GET** /repos/{owner}/{repo}/comments | List commit comments for a repository
*ReposApi* | [**reposListCommitStatusesForRef**](Apis/ReposApi.http#reposlistcommitstatusesforref) | **GET** /repos/{owner}/{repo}/commits/{ref}/statuses | List commit statuses for a reference
*ReposApi* | [**reposListCommits**](Apis/ReposApi.http#reposlistcommits) | **GET** /repos/{owner}/{repo}/commits | List commits
*ReposApi* | [**reposListContributors**](Apis/ReposApi.http#reposlistcontributors) | **GET** /repos/{owner}/{repo}/contributors | List repository contributors
*ReposApi* | [**reposListCustomDeploymentRuleIntegrations**](Apis/ReposApi.http#reposlistcustomdeploymentruleintegrations) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules/apps | List custom deployment rule integrations available for an environment
*ReposApi* | [**reposListDeployKeys**](Apis/ReposApi.http#reposlistdeploykeys) | **GET** /repos/{owner}/{repo}/keys | List deploy keys
*ReposApi* | [**reposListDeploymentBranchPolicies**](Apis/ReposApi.http#reposlistdeploymentbranchpolicies) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies | List deployment branch policies
*ReposApi* | [**reposListDeploymentStatuses**](Apis/ReposApi.http#reposlistdeploymentstatuses) | **GET** /repos/{owner}/{repo}/deployments/{deployment_id}/statuses | List deployment statuses
*ReposApi* | [**reposListDeployments**](Apis/ReposApi.http#reposlistdeployments) | **GET** /repos/{owner}/{repo}/deployments | List deployments
*ReposApi* | [**reposListForAuthenticatedUser**](Apis/ReposApi.http#reposlistforauthenticateduser) | **GET** /user/repos | List repositories for the authenticated user
*ReposApi* | [**reposListForOrg**](Apis/ReposApi.http#reposlistfororg) | **GET** /orgs/{org}/repos | List organization repositories
*ReposApi* | [**reposListForUser**](Apis/ReposApi.http#reposlistforuser) | **GET** /users/{username}/repos | List repositories for a user
*ReposApi* | [**reposListForks**](Apis/ReposApi.http#reposlistforks) | **GET** /repos/{owner}/{repo}/forks | List forks
*ReposApi* | [**reposListInvitations**](Apis/ReposApi.http#reposlistinvitations) | **GET** /repos/{owner}/{repo}/invitations | List repository invitations
*ReposApi* | [**reposListInvitationsForAuthenticatedUser**](Apis/ReposApi.http#reposlistinvitationsforauthenticateduser) | **GET** /user/repository_invitations | List repository invitations for the authenticated user
*ReposApi* | [**reposListLanguages**](Apis/ReposApi.http#reposlistlanguages) | **GET** /repos/{owner}/{repo}/languages | List repository languages
*ReposApi* | [**reposListPagesBuilds**](Apis/ReposApi.http#reposlistpagesbuilds) | **GET** /repos/{owner}/{repo}/pages/builds | List GitHub Pages builds
*ReposApi* | [**reposListPublic**](Apis/ReposApi.http#reposlistpublic) | **GET** /repositories | List public repositories
*ReposApi* | [**reposListPullRequestsAssociatedWithCommit**](Apis/ReposApi.http#reposlistpullrequestsassociatedwithcommit) | **GET** /repos/{owner}/{repo}/commits/{commit_sha}/pulls | List pull requests associated with a commit
*ReposApi* | [**reposListReleaseAssets**](Apis/ReposApi.http#reposlistreleaseassets) | **GET** /repos/{owner}/{repo}/releases/{release_id}/assets | List release assets
*ReposApi* | [**reposListReleases**](Apis/ReposApi.http#reposlistreleases) | **GET** /repos/{owner}/{repo}/releases | List releases
*ReposApi* | [**reposListTagProtection**](Apis/ReposApi.http#reposlisttagprotection) | **GET** /repos/{owner}/{repo}/tags/protection | List tag protection states for a repository
*ReposApi* | [**reposListTags**](Apis/ReposApi.http#reposlisttags) | **GET** /repos/{owner}/{repo}/tags | List repository tags
*ReposApi* | [**reposListTeams**](Apis/ReposApi.http#reposlistteams) | **GET** /repos/{owner}/{repo}/teams | List repository teams
*ReposApi* | [**reposListWebhookDeliveries**](Apis/ReposApi.http#reposlistwebhookdeliveries) | **GET** /repos/{owner}/{repo}/hooks/{hook_id}/deliveries | List deliveries for a repository webhook
*ReposApi* | [**reposListWebhooks**](Apis/ReposApi.http#reposlistwebhooks) | **GET** /repos/{owner}/{repo}/hooks | List repository webhooks
*ReposApi* | [**reposMerge**](Apis/ReposApi.http#reposmerge) | **POST** /repos/{owner}/{repo}/merges | Merge a branch
*ReposApi* | [**reposMergeUpstream**](Apis/ReposApi.http#reposmergeupstream) | **POST** /repos/{owner}/{repo}/merge-upstream | Sync a fork branch with the upstream repository
*ReposApi* | [**reposPingWebhook**](Apis/ReposApi.http#repospingwebhook) | **POST** /repos/{owner}/{repo}/hooks/{hook_id}/pings | Ping a repository webhook
*ReposApi* | [**reposRedeliverWebhookDelivery**](Apis/ReposApi.http#reposredeliverwebhookdelivery) | **POST** /repos/{owner}/{repo}/hooks/{hook_id}/deliveries/{delivery_id}/attempts | Redeliver a delivery for a repository webhook
*ReposApi* | [**reposRemoveAppAccessRestrictions**](Apis/ReposApi.http#reposremoveappaccessrestrictions) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/apps | Remove app access restrictions
*ReposApi* | [**reposRemoveCollaborator**](Apis/ReposApi.http#reposremovecollaborator) | **DELETE** /repos/{owner}/{repo}/collaborators/{username} | Remove a repository collaborator
*ReposApi* | [**reposRemoveStatusCheckContexts**](Apis/ReposApi.http#reposremovestatuscheckcontexts) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks/contexts | Remove status check contexts
*ReposApi* | [**reposRemoveStatusCheckProtection**](Apis/ReposApi.http#reposremovestatuscheckprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks | Remove status check protection
*ReposApi* | [**reposRemoveTeamAccessRestrictions**](Apis/ReposApi.http#reposremoveteamaccessrestrictions) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/teams | Remove team access restrictions
*ReposApi* | [**reposRemoveUserAccessRestrictions**](Apis/ReposApi.http#reposremoveuseraccessrestrictions) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/users | Remove user access restrictions
*ReposApi* | [**reposRenameBranch**](Apis/ReposApi.http#reposrenamebranch) | **POST** /repos/{owner}/{repo}/branches/{branch}/rename | Rename a branch
*ReposApi* | [**reposReplaceAllTopics**](Apis/ReposApi.http#reposreplacealltopics) | **PUT** /repos/{owner}/{repo}/topics | Replace all repository topics
*ReposApi* | [**reposRequestPagesBuild**](Apis/ReposApi.http#reposrequestpagesbuild) | **POST** /repos/{owner}/{repo}/pages/builds | Request a GitHub Pages build
*ReposApi* | [**reposSetAdminBranchProtection**](Apis/ReposApi.http#repossetadminbranchprotection) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/enforce_admins | Set admin branch protection
*ReposApi* | [**reposSetAppAccessRestrictions**](Apis/ReposApi.http#repossetappaccessrestrictions) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/apps | Set app access restrictions
*ReposApi* | [**reposSetStatusCheckContexts**](Apis/ReposApi.http#repossetstatuscheckcontexts) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks/contexts | Set status check contexts
*ReposApi* | [**reposSetTeamAccessRestrictions**](Apis/ReposApi.http#repossetteamaccessrestrictions) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/teams | Set team access restrictions
*ReposApi* | [**reposSetUserAccessRestrictions**](Apis/ReposApi.http#repossetuseraccessrestrictions) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/users | Set user access restrictions
*ReposApi* | [**reposTestPushWebhook**](Apis/ReposApi.http#repostestpushwebhook) | **POST** /repos/{owner}/{repo}/hooks/{hook_id}/tests | Test the push repository webhook
*ReposApi* | [**reposTransfer**](Apis/ReposApi.http#repostransfer) | **POST** /repos/{owner}/{repo}/transfer | Transfer a repository
*ReposApi* | [**reposUpdate**](Apis/ReposApi.http#reposupdate) | **PATCH** /repos/{owner}/{repo} | Update a repository
*ReposApi* | [**reposUpdateBranchProtection**](Apis/ReposApi.http#reposupdatebranchprotection) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection | Update branch protection
*ReposApi* | [**reposUpdateCommitComment**](Apis/ReposApi.http#reposupdatecommitcomment) | **PATCH** /repos/{owner}/{repo}/comments/{comment_id} | Update a commit comment
*ReposApi* | [**reposUpdateDeploymentBranchPolicy**](Apis/ReposApi.http#reposupdatedeploymentbranchpolicy) | **PUT** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies/{branch_policy_id} | Update a deployment branch policy
*ReposApi* | [**reposUpdateInformationAboutPagesSite**](Apis/ReposApi.http#reposupdateinformationaboutpagessite) | **PUT** /repos/{owner}/{repo}/pages | Update information about a GitHub Pages site
*ReposApi* | [**reposUpdateInvitation**](Apis/ReposApi.http#reposupdateinvitation) | **PATCH** /repos/{owner}/{repo}/invitations/{invitation_id} | Update a repository invitation
*ReposApi* | [**reposUpdateOrgRuleset**](Apis/ReposApi.http#reposupdateorgruleset) | **PUT** /orgs/{org}/rulesets/{ruleset_id} | Update an organization repository ruleset
*ReposApi* | [**reposUpdatePullRequestReviewProtection**](Apis/ReposApi.http#reposupdatepullrequestreviewprotection) | **PATCH** /repos/{owner}/{repo}/branches/{branch}/protection/required_pull_request_reviews | Update pull request review protection
*ReposApi* | [**reposUpdateRelease**](Apis/ReposApi.http#reposupdaterelease) | **PATCH** /repos/{owner}/{repo}/releases/{release_id} | Update a release
*ReposApi* | [**reposUpdateReleaseAsset**](Apis/ReposApi.http#reposupdatereleaseasset) | **PATCH** /repos/{owner}/{repo}/releases/assets/{asset_id} | Update a release asset
*ReposApi* | [**reposUpdateRepoRuleset**](Apis/ReposApi.http#reposupdatereporuleset) | **PUT** /repos/{owner}/{repo}/rulesets/{ruleset_id} | Update a repository ruleset
*ReposApi* | [**reposUpdateStatusCheckProtection**](Apis/ReposApi.http#reposupdatestatuscheckprotection) | **PATCH** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks | Update status check protection
*ReposApi* | [**reposUpdateWebhook**](Apis/ReposApi.http#reposupdatewebhook) | **PATCH** /repos/{owner}/{repo}/hooks/{hook_id} | Update a repository webhook
*ReposApi* | [**reposUpdateWebhookConfigForRepo**](Apis/ReposApi.http#reposupdatewebhookconfigforrepo) | **PATCH** /repos/{owner}/{repo}/hooks/{hook_id}/config | Update a webhook configuration for a repository
*ReposApi* | [**reposUploadReleaseAsset**](Apis/ReposApi.http#reposuploadreleaseasset) | **POST** /repos/{owner}/{repo}/releases/{release_id}/assets | Upload a release asset
*SearchApi* | [**searchCode**](Apis/SearchApi.http#searchcode) | **GET** /search/code | Search code
*SearchApi* | [**searchCommits**](Apis/SearchApi.http#searchcommits) | **GET** /search/commits | Search commits
*SearchApi* | [**searchIssuesAndPullRequests**](Apis/SearchApi.http#searchissuesandpullrequests) | **GET** /search/issues | Search issues and pull requests
*SearchApi* | [**searchLabels**](Apis/SearchApi.http#searchlabels) | **GET** /search/labels | Search labels
*SearchApi* | [**searchRepos**](Apis/SearchApi.http#searchrepos) | **GET** /search/repositories | Search repositories
*SearchApi* | [**searchTopics**](Apis/SearchApi.http#searchtopics) | **GET** /search/topics | Search topics
*SearchApi* | [**searchUsers**](Apis/SearchApi.http#searchusers) | **GET** /search/users | Search users
*SecretScanningApi* | [**secretScanning/getAlert**](Apis/SecretScanningApi.http#secretscanning/getalert) | **GET** /repos/{owner}/{repo}/secret-scanning/alerts/{alert_number} | Get a secret scanning alert
*SecretScanningApi* | [**secretScanning/listAlertsForEnterprise**](Apis/SecretScanningApi.http#secretscanning/listalertsforenterprise) | **GET** /enterprises/{enterprise}/secret-scanning/alerts | List secret scanning alerts for an enterprise
*SecretScanningApi* | [**secretScanning/listAlertsForOrg**](Apis/SecretScanningApi.http#secretscanning/listalertsfororg) | **GET** /orgs/{org}/secret-scanning/alerts | List secret scanning alerts for an organization
*SecretScanningApi* | [**secretScanning/listAlertsForRepo**](Apis/SecretScanningApi.http#secretscanning/listalertsforrepo) | **GET** /repos/{owner}/{repo}/secret-scanning/alerts | List secret scanning alerts for a repository
*SecretScanningApi* | [**secretScanning/listLocationsForAlert**](Apis/SecretScanningApi.http#secretscanning/listlocationsforalert) | **GET** /repos/{owner}/{repo}/secret-scanning/alerts/{alert_number}/locations | List locations for a secret scanning alert
*SecretScanningApi* | [**secretScanning/updateAlert**](Apis/SecretScanningApi.http#secretscanning/updatealert) | **PATCH** /repos/{owner}/{repo}/secret-scanning/alerts/{alert_number} | Update a secret scanning alert
*SecurityAdvisoriesApi* | [**securityAdvisories/createFork**](Apis/SecurityAdvisoriesApi.http#securityadvisories/createfork) | **POST** /repos/{owner}/{repo}/security-advisories/{ghsa_id}/forks | Create a temporary private fork
*SecurityAdvisoriesApi* | [**securityAdvisories/createPrivateVulnerabilityReport**](Apis/SecurityAdvisoriesApi.http#securityadvisories/createprivatevulnerabilityreport) | **POST** /repos/{owner}/{repo}/security-advisories/reports | Privately report a security vulnerability
*SecurityAdvisoriesApi* | [**securityAdvisories/createRepositoryAdvisory**](Apis/SecurityAdvisoriesApi.http#securityadvisories/createrepositoryadvisory) | **POST** /repos/{owner}/{repo}/security-advisories | Create a repository security advisory
*SecurityAdvisoriesApi* | [**securityAdvisories/createRepositoryAdvisoryCveRequest**](Apis/SecurityAdvisoriesApi.http#securityadvisories/createrepositoryadvisorycverequest) | **POST** /repos/{owner}/{repo}/security-advisories/{ghsa_id}/cve | Request a CVE for a repository security advisory
*SecurityAdvisoriesApi* | [**securityAdvisories/getGlobalAdvisory**](Apis/SecurityAdvisoriesApi.http#securityadvisories/getglobaladvisory) | **GET** /advisories/{ghsa_id} | Get a global security advisory
*SecurityAdvisoriesApi* | [**securityAdvisories/getRepositoryAdvisory**](Apis/SecurityAdvisoriesApi.http#securityadvisories/getrepositoryadvisory) | **GET** /repos/{owner}/{repo}/security-advisories/{ghsa_id} | Get a repository security advisory
*SecurityAdvisoriesApi* | [**securityAdvisories/listGlobalAdvisories**](Apis/SecurityAdvisoriesApi.http#securityadvisories/listglobaladvisories) | **GET** /advisories | List global security advisories
*SecurityAdvisoriesApi* | [**securityAdvisories/listOrgRepositoryAdvisories**](Apis/SecurityAdvisoriesApi.http#securityadvisories/listorgrepositoryadvisories) | **GET** /orgs/{org}/security-advisories | List repository security advisories for an organization
*SecurityAdvisoriesApi* | [**securityAdvisories/listRepositoryAdvisories**](Apis/SecurityAdvisoriesApi.http#securityadvisories/listrepositoryadvisories) | **GET** /repos/{owner}/{repo}/security-advisories | List repository security advisories
*SecurityAdvisoriesApi* | [**securityAdvisories/updateRepositoryAdvisory**](Apis/SecurityAdvisoriesApi.http#securityadvisories/updaterepositoryadvisory) | **PATCH** /repos/{owner}/{repo}/security-advisories/{ghsa_id} | Update a repository security advisory
*TeamsApi* | [**teamsAddMemberLegacy**](Apis/TeamsApi.http#teamsaddmemberlegacy) | **PUT** /teams/{team_id}/members/{username} | Add team member (Legacy)
*TeamsApi* | [**teamsAddOrUpdateMembershipForUserInOrg**](Apis/TeamsApi.http#teamsaddorupdatemembershipforuserinorg) | **PUT** /orgs/{org}/teams/{team_slug}/memberships/{username} | Add or update team membership for a user
*TeamsApi* | [**teamsAddOrUpdateMembershipForUserLegacy**](Apis/TeamsApi.http#teamsaddorupdatemembershipforuserlegacy) | **PUT** /teams/{team_id}/memberships/{username} | Add or update team membership for a user (Legacy)
*TeamsApi* | [**teamsAddOrUpdateProjectPermissionsInOrg**](Apis/TeamsApi.http#teamsaddorupdateprojectpermissionsinorg) | **PUT** /orgs/{org}/teams/{team_slug}/projects/{project_id} | Add or update team project permissions
*TeamsApi* | [**teamsAddOrUpdateProjectPermissionsLegacy**](Apis/TeamsApi.http#teamsaddorupdateprojectpermissionslegacy) | **PUT** /teams/{team_id}/projects/{project_id} | Add or update team project permissions (Legacy)
*TeamsApi* | [**teamsAddOrUpdateRepoPermissionsInOrg**](Apis/TeamsApi.http#teamsaddorupdaterepopermissionsinorg) | **PUT** /orgs/{org}/teams/{team_slug}/repos/{owner}/{repo} | Add or update team repository permissions
*TeamsApi* | [**teamsAddOrUpdateRepoPermissionsLegacy**](Apis/TeamsApi.http#teamsaddorupdaterepopermissionslegacy) | **PUT** /teams/{team_id}/repos/{owner}/{repo} | Add or update team repository permissions (Legacy)
*TeamsApi* | [**teamsCheckPermissionsForProjectInOrg**](Apis/TeamsApi.http#teamscheckpermissionsforprojectinorg) | **GET** /orgs/{org}/teams/{team_slug}/projects/{project_id} | Check team permissions for a project
*TeamsApi* | [**teamsCheckPermissionsForProjectLegacy**](Apis/TeamsApi.http#teamscheckpermissionsforprojectlegacy) | **GET** /teams/{team_id}/projects/{project_id} | Check team permissions for a project (Legacy)
*TeamsApi* | [**teamsCheckPermissionsForRepoInOrg**](Apis/TeamsApi.http#teamscheckpermissionsforrepoinorg) | **GET** /orgs/{org}/teams/{team_slug}/repos/{owner}/{repo} | Check team permissions for a repository
*TeamsApi* | [**teamsCheckPermissionsForRepoLegacy**](Apis/TeamsApi.http#teamscheckpermissionsforrepolegacy) | **GET** /teams/{team_id}/repos/{owner}/{repo} | Check team permissions for a repository (Legacy)
*TeamsApi* | [**teamsCreate**](Apis/TeamsApi.http#teamscreate) | **POST** /orgs/{org}/teams | Create a team
*TeamsApi* | [**teamsCreateDiscussionCommentInOrg**](Apis/TeamsApi.http#teamscreatediscussioncommentinorg) | **POST** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments | Create a discussion comment
*TeamsApi* | [**teamsCreateDiscussionCommentLegacy**](Apis/TeamsApi.http#teamscreatediscussioncommentlegacy) | **POST** /teams/{team_id}/discussions/{discussion_number}/comments | Create a discussion comment (Legacy)
*TeamsApi* | [**teamsCreateDiscussionInOrg**](Apis/TeamsApi.http#teamscreatediscussioninorg) | **POST** /orgs/{org}/teams/{team_slug}/discussions | Create a discussion
*TeamsApi* | [**teamsCreateDiscussionLegacy**](Apis/TeamsApi.http#teamscreatediscussionlegacy) | **POST** /teams/{team_id}/discussions | Create a discussion (Legacy)
*TeamsApi* | [**teamsDeleteDiscussionCommentInOrg**](Apis/TeamsApi.http#teamsdeletediscussioncommentinorg) | **DELETE** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number} | Delete a discussion comment
*TeamsApi* | [**teamsDeleteDiscussionCommentLegacy**](Apis/TeamsApi.http#teamsdeletediscussioncommentlegacy) | **DELETE** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number} | Delete a discussion comment (Legacy)
*TeamsApi* | [**teamsDeleteDiscussionInOrg**](Apis/TeamsApi.http#teamsdeletediscussioninorg) | **DELETE** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number} | Delete a discussion
*TeamsApi* | [**teamsDeleteDiscussionLegacy**](Apis/TeamsApi.http#teamsdeletediscussionlegacy) | **DELETE** /teams/{team_id}/discussions/{discussion_number} | Delete a discussion (Legacy)
*TeamsApi* | [**teamsDeleteInOrg**](Apis/TeamsApi.http#teamsdeleteinorg) | **DELETE** /orgs/{org}/teams/{team_slug} | Delete a team
*TeamsApi* | [**teamsDeleteLegacy**](Apis/TeamsApi.http#teamsdeletelegacy) | **DELETE** /teams/{team_id} | Delete a team (Legacy)
*TeamsApi* | [**teamsGetByName**](Apis/TeamsApi.http#teamsgetbyname) | **GET** /orgs/{org}/teams/{team_slug} | Get a team by name
*TeamsApi* | [**teamsGetDiscussionCommentInOrg**](Apis/TeamsApi.http#teamsgetdiscussioncommentinorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number} | Get a discussion comment
*TeamsApi* | [**teamsGetDiscussionCommentLegacy**](Apis/TeamsApi.http#teamsgetdiscussioncommentlegacy) | **GET** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number} | Get a discussion comment (Legacy)
*TeamsApi* | [**teamsGetDiscussionInOrg**](Apis/TeamsApi.http#teamsgetdiscussioninorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number} | Get a discussion
*TeamsApi* | [**teamsGetDiscussionLegacy**](Apis/TeamsApi.http#teamsgetdiscussionlegacy) | **GET** /teams/{team_id}/discussions/{discussion_number} | Get a discussion (Legacy)
*TeamsApi* | [**teamsGetLegacy**](Apis/TeamsApi.http#teamsgetlegacy) | **GET** /teams/{team_id} | Get a team (Legacy)
*TeamsApi* | [**teamsGetMemberLegacy**](Apis/TeamsApi.http#teamsgetmemberlegacy) | **GET** /teams/{team_id}/members/{username} | Get team member (Legacy)
*TeamsApi* | [**teamsGetMembershipForUserInOrg**](Apis/TeamsApi.http#teamsgetmembershipforuserinorg) | **GET** /orgs/{org}/teams/{team_slug}/memberships/{username} | Get team membership for a user
*TeamsApi* | [**teamsGetMembershipForUserLegacy**](Apis/TeamsApi.http#teamsgetmembershipforuserlegacy) | **GET** /teams/{team_id}/memberships/{username} | Get team membership for a user (Legacy)
*TeamsApi* | [**teamsList**](Apis/TeamsApi.http#teamslist) | **GET** /orgs/{org}/teams | List teams
*TeamsApi* | [**teamsListChildInOrg**](Apis/TeamsApi.http#teamslistchildinorg) | **GET** /orgs/{org}/teams/{team_slug}/teams | List child teams
*TeamsApi* | [**teamsListChildLegacy**](Apis/TeamsApi.http#teamslistchildlegacy) | **GET** /teams/{team_id}/teams | List child teams (Legacy)
*TeamsApi* | [**teamsListDiscussionCommentsInOrg**](Apis/TeamsApi.http#teamslistdiscussioncommentsinorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments | List discussion comments
*TeamsApi* | [**teamsListDiscussionCommentsLegacy**](Apis/TeamsApi.http#teamslistdiscussioncommentslegacy) | **GET** /teams/{team_id}/discussions/{discussion_number}/comments | List discussion comments (Legacy)
*TeamsApi* | [**teamsListDiscussionsInOrg**](Apis/TeamsApi.http#teamslistdiscussionsinorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions | List discussions
*TeamsApi* | [**teamsListDiscussionsLegacy**](Apis/TeamsApi.http#teamslistdiscussionslegacy) | **GET** /teams/{team_id}/discussions | List discussions (Legacy)
*TeamsApi* | [**teamsListForAuthenticatedUser**](Apis/TeamsApi.http#teamslistforauthenticateduser) | **GET** /user/teams | List teams for the authenticated user
*TeamsApi* | [**teamsListMembersInOrg**](Apis/TeamsApi.http#teamslistmembersinorg) | **GET** /orgs/{org}/teams/{team_slug}/members | List team members
*TeamsApi* | [**teamsListMembersLegacy**](Apis/TeamsApi.http#teamslistmemberslegacy) | **GET** /teams/{team_id}/members | List team members (Legacy)
*TeamsApi* | [**teamsListPendingInvitationsInOrg**](Apis/TeamsApi.http#teamslistpendinginvitationsinorg) | **GET** /orgs/{org}/teams/{team_slug}/invitations | List pending team invitations
*TeamsApi* | [**teamsListPendingInvitationsLegacy**](Apis/TeamsApi.http#teamslistpendinginvitationslegacy) | **GET** /teams/{team_id}/invitations | List pending team invitations (Legacy)
*TeamsApi* | [**teamsListProjectsInOrg**](Apis/TeamsApi.http#teamslistprojectsinorg) | **GET** /orgs/{org}/teams/{team_slug}/projects | List team projects
*TeamsApi* | [**teamsListProjectsLegacy**](Apis/TeamsApi.http#teamslistprojectslegacy) | **GET** /teams/{team_id}/projects | List team projects (Legacy)
*TeamsApi* | [**teamsListReposInOrg**](Apis/TeamsApi.http#teamslistreposinorg) | **GET** /orgs/{org}/teams/{team_slug}/repos | List team repositories
*TeamsApi* | [**teamsListReposLegacy**](Apis/TeamsApi.http#teamslistreposlegacy) | **GET** /teams/{team_id}/repos | List team repositories (Legacy)
*TeamsApi* | [**teamsRemoveMemberLegacy**](Apis/TeamsApi.http#teamsremovememberlegacy) | **DELETE** /teams/{team_id}/members/{username} | Remove team member (Legacy)
*TeamsApi* | [**teamsRemoveMembershipForUserInOrg**](Apis/TeamsApi.http#teamsremovemembershipforuserinorg) | **DELETE** /orgs/{org}/teams/{team_slug}/memberships/{username} | Remove team membership for a user
*TeamsApi* | [**teamsRemoveMembershipForUserLegacy**](Apis/TeamsApi.http#teamsremovemembershipforuserlegacy) | **DELETE** /teams/{team_id}/memberships/{username} | Remove team membership for a user (Legacy)
*TeamsApi* | [**teamsRemoveProjectInOrg**](Apis/TeamsApi.http#teamsremoveprojectinorg) | **DELETE** /orgs/{org}/teams/{team_slug}/projects/{project_id} | Remove a project from a team
*TeamsApi* | [**teamsRemoveProjectLegacy**](Apis/TeamsApi.http#teamsremoveprojectlegacy) | **DELETE** /teams/{team_id}/projects/{project_id} | Remove a project from a team (Legacy)
*TeamsApi* | [**teamsRemoveRepoInOrg**](Apis/TeamsApi.http#teamsremoverepoinorg) | **DELETE** /orgs/{org}/teams/{team_slug}/repos/{owner}/{repo} | Remove a repository from a team
*TeamsApi* | [**teamsRemoveRepoLegacy**](Apis/TeamsApi.http#teamsremoverepolegacy) | **DELETE** /teams/{team_id}/repos/{owner}/{repo} | Remove a repository from a team (Legacy)
*TeamsApi* | [**teamsUpdateDiscussionCommentInOrg**](Apis/TeamsApi.http#teamsupdatediscussioncommentinorg) | **PATCH** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number} | Update a discussion comment
*TeamsApi* | [**teamsUpdateDiscussionCommentLegacy**](Apis/TeamsApi.http#teamsupdatediscussioncommentlegacy) | **PATCH** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number} | Update a discussion comment (Legacy)
*TeamsApi* | [**teamsUpdateDiscussionInOrg**](Apis/TeamsApi.http#teamsupdatediscussioninorg) | **PATCH** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number} | Update a discussion
*TeamsApi* | [**teamsUpdateDiscussionLegacy**](Apis/TeamsApi.http#teamsupdatediscussionlegacy) | **PATCH** /teams/{team_id}/discussions/{discussion_number} | Update a discussion (Legacy)
*TeamsApi* | [**teamsUpdateInOrg**](Apis/TeamsApi.http#teamsupdateinorg) | **PATCH** /orgs/{org}/teams/{team_slug} | Update a team
*TeamsApi* | [**teamsUpdateLegacy**](Apis/TeamsApi.http#teamsupdatelegacy) | **PATCH** /teams/{team_id} | Update a team (Legacy)
*UsersApi* | [**usersAddEmailForAuthenticatedUser**](Apis/UsersApi.http#usersaddemailforauthenticateduser) | **POST** /user/emails | Add an email address for the authenticated user
*UsersApi* | [**usersAddSocialAccountForAuthenticatedUser**](Apis/UsersApi.http#usersaddsocialaccountforauthenticateduser) | **POST** /user/social_accounts | Add social accounts for the authenticated user
*UsersApi* | [**usersBlock**](Apis/UsersApi.http#usersblock) | **PUT** /user/blocks/{username} | Block a user
*UsersApi* | [**usersCheckBlocked**](Apis/UsersApi.http#userscheckblocked) | **GET** /user/blocks/{username} | Check if a user is blocked by the authenticated user
*UsersApi* | [**usersCheckFollowingForUser**](Apis/UsersApi.http#userscheckfollowingforuser) | **GET** /users/{username}/following/{target_user} | Check if a user follows another user
*UsersApi* | [**usersCheckPersonIsFollowedByAuthenticated**](Apis/UsersApi.http#userscheckpersonisfollowedbyauthenticated) | **GET** /user/following/{username} | Check if a person is followed by the authenticated user
*UsersApi* | [**usersCreateGpgKeyForAuthenticatedUser**](Apis/UsersApi.http#userscreategpgkeyforauthenticateduser) | **POST** /user/gpg_keys | Create a GPG key for the authenticated user
*UsersApi* | [**usersCreatePublicSshKeyForAuthenticatedUser**](Apis/UsersApi.http#userscreatepublicsshkeyforauthenticateduser) | **POST** /user/keys | Create a public SSH key for the authenticated user
*UsersApi* | [**usersCreateSshSigningKeyForAuthenticatedUser**](Apis/UsersApi.http#userscreatesshsigningkeyforauthenticateduser) | **POST** /user/ssh_signing_keys | Create a SSH signing key for the authenticated user
*UsersApi* | [**usersDeleteEmailForAuthenticatedUser**](Apis/UsersApi.http#usersdeleteemailforauthenticateduser) | **DELETE** /user/emails | Delete an email address for the authenticated user
*UsersApi* | [**usersDeleteGpgKeyForAuthenticatedUser**](Apis/UsersApi.http#usersdeletegpgkeyforauthenticateduser) | **DELETE** /user/gpg_keys/{gpg_key_id} | Delete a GPG key for the authenticated user
*UsersApi* | [**usersDeletePublicSshKeyForAuthenticatedUser**](Apis/UsersApi.http#usersdeletepublicsshkeyforauthenticateduser) | **DELETE** /user/keys/{key_id} | Delete a public SSH key for the authenticated user
*UsersApi* | [**usersDeleteSocialAccountForAuthenticatedUser**](Apis/UsersApi.http#usersdeletesocialaccountforauthenticateduser) | **DELETE** /user/social_accounts | Delete social accounts for the authenticated user
*UsersApi* | [**usersDeleteSshSigningKeyForAuthenticatedUser**](Apis/UsersApi.http#usersdeletesshsigningkeyforauthenticateduser) | **DELETE** /user/ssh_signing_keys/{ssh_signing_key_id} | Delete an SSH signing key for the authenticated user
*UsersApi* | [**usersFollow**](Apis/UsersApi.http#usersfollow) | **PUT** /user/following/{username} | Follow a user
*UsersApi* | [**usersGetAuthenticated**](Apis/UsersApi.http#usersgetauthenticated) | **GET** /user | Get the authenticated user
*UsersApi* | [**usersGetByUsername**](Apis/UsersApi.http#usersgetbyusername) | **GET** /users/{username} | Get a user
*UsersApi* | [**usersGetContextForUser**](Apis/UsersApi.http#usersgetcontextforuser) | **GET** /users/{username}/hovercard | Get contextual information for a user
*UsersApi* | [**usersGetGpgKeyForAuthenticatedUser**](Apis/UsersApi.http#usersgetgpgkeyforauthenticateduser) | **GET** /user/gpg_keys/{gpg_key_id} | Get a GPG key for the authenticated user
*UsersApi* | [**usersGetPublicSshKeyForAuthenticatedUser**](Apis/UsersApi.http#usersgetpublicsshkeyforauthenticateduser) | **GET** /user/keys/{key_id} | Get a public SSH key for the authenticated user
*UsersApi* | [**usersGetSshSigningKeyForAuthenticatedUser**](Apis/UsersApi.http#usersgetsshsigningkeyforauthenticateduser) | **GET** /user/ssh_signing_keys/{ssh_signing_key_id} | Get an SSH signing key for the authenticated user
*UsersApi* | [**usersList**](Apis/UsersApi.http#userslist) | **GET** /users | List users
*UsersApi* | [**usersListBlockedByAuthenticatedUser**](Apis/UsersApi.http#userslistblockedbyauthenticateduser) | **GET** /user/blocks | List users blocked by the authenticated user
*UsersApi* | [**usersListEmailsForAuthenticatedUser**](Apis/UsersApi.http#userslistemailsforauthenticateduser) | **GET** /user/emails | List email addresses for the authenticated user
*UsersApi* | [**usersListFollowedByAuthenticatedUser**](Apis/UsersApi.http#userslistfollowedbyauthenticateduser) | **GET** /user/following | List the people the authenticated user follows
*UsersApi* | [**usersListFollowersForAuthenticatedUser**](Apis/UsersApi.http#userslistfollowersforauthenticateduser) | **GET** /user/followers | List followers of the authenticated user
*UsersApi* | [**usersListFollowersForUser**](Apis/UsersApi.http#userslistfollowersforuser) | **GET** /users/{username}/followers | List followers of a user
*UsersApi* | [**usersListFollowingForUser**](Apis/UsersApi.http#userslistfollowingforuser) | **GET** /users/{username}/following | List the people a user follows
*UsersApi* | [**usersListGpgKeysForAuthenticatedUser**](Apis/UsersApi.http#userslistgpgkeysforauthenticateduser) | **GET** /user/gpg_keys | List GPG keys for the authenticated user
*UsersApi* | [**usersListGpgKeysForUser**](Apis/UsersApi.http#userslistgpgkeysforuser) | **GET** /users/{username}/gpg_keys | List GPG keys for a user
*UsersApi* | [**usersListPublicEmailsForAuthenticatedUser**](Apis/UsersApi.http#userslistpublicemailsforauthenticateduser) | **GET** /user/public_emails | List public email addresses for the authenticated user
*UsersApi* | [**usersListPublicKeysForUser**](Apis/UsersApi.http#userslistpublickeysforuser) | **GET** /users/{username}/keys | List public keys for a user
*UsersApi* | [**usersListPublicSshKeysForAuthenticatedUser**](Apis/UsersApi.http#userslistpublicsshkeysforauthenticateduser) | **GET** /user/keys | List public SSH keys for the authenticated user
*UsersApi* | [**usersListSocialAccountsForAuthenticatedUser**](Apis/UsersApi.http#userslistsocialaccountsforauthenticateduser) | **GET** /user/social_accounts | List social accounts for the authenticated user
*UsersApi* | [**usersListSocialAccountsForUser**](Apis/UsersApi.http#userslistsocialaccountsforuser) | **GET** /users/{username}/social_accounts | List social accounts for a user
*UsersApi* | [**usersListSshSigningKeysForAuthenticatedUser**](Apis/UsersApi.http#userslistsshsigningkeysforauthenticateduser) | **GET** /user/ssh_signing_keys | List SSH signing keys for the authenticated user
*UsersApi* | [**usersListSshSigningKeysForUser**](Apis/UsersApi.http#userslistsshsigningkeysforuser) | **GET** /users/{username}/ssh_signing_keys | List SSH signing keys for a user
*UsersApi* | [**usersSetPrimaryEmailVisibilityForAuthenticatedUser**](Apis/UsersApi.http#userssetprimaryemailvisibilityforauthenticateduser) | **PATCH** /user/email/visibility | Set primary email visibility for the authenticated user
*UsersApi* | [**usersUnblock**](Apis/UsersApi.http#usersunblock) | **DELETE** /user/blocks/{username} | Unblock a user
*UsersApi* | [**usersUnfollow**](Apis/UsersApi.http#usersunfollow) | **DELETE** /user/following/{username} | Unfollow a user
*UsersApi* | [**usersUpdateAuthenticated**](Apis/UsersApi.http#usersupdateauthenticated) | **PATCH** /user | Update the authenticated user


## Usage

### Prerequisites

You need [IntelliJ](https://www.jetbrains.com/idea/) to be able to run those queries. More information can be found [here](https://www.jetbrains.com/help/idea/http-client-in-product-code-editor.html).
You may have some luck running queries using the [Code REST Client](https://marketplace.visualstudio.com/items?itemName=humao.rest-client) as well, but your mileage may vary.

### Variables and Environment files

* Generally speaking, you want queries to be specific using custom variables. All variables in the `.http` files have the `` format.
* You can create [public or private environment files](https://www.jetbrains.com/help/idea/exploring-http-syntax.html#environment-variables) to dynamically replace the variables at runtime.

_Note: don't commit private environment files! They typically will contain sensitive information like API Keys._

### Customizations

If you have control over the generation of the files here, there are two main things you can do

* Select elements to replace as variables during generation. The process is case-sensitive. For example, API_KEY -> 
    * For this, run the generation with the `bodyVariables` property, followed by a "-" separated list of variables
    * Example: `--additional-properties bodyVariables=YOUR_MERCHANT_ACCOUNT-YOUR_COMPANY_ACCOUNT-YOUR_BALANCE_PLATFORM`
* Add custom headers to _all_ requests. This can be useful for example if your specifications are missing [security schemes](https://github.com/github/rest-api-description/issues/237).
    * For this, run the generation with the `customHeaders` property, followed by a "&" separated list of variables
    * Example : `--additional-properties=customHeaders="Cookie:X-API-KEY="&"Accept-Encoding=gzip"`

_This client was generated by the [jetbrains-http-client](https://openapi-generator.tech/docs/generators/jetbrains-http-client) generator of OpenAPI Generator_