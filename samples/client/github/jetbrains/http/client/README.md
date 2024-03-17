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
*ActionsApi* | [**actions/addCustomLabelsToSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actions/addcustomlabelstoselfhostedrunnerfororg) | **POST** /orgs/{org}/actions/runners/{runner_id}/labels | Add custom labels to a self-hosted runner for an organization
*ActionsApi* | [**actions/addCustomLabelsToSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actions/addcustomlabelstoselfhostedrunnerforrepo) | **POST** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels | Add custom labels to a self-hosted runner for a repository
*ActionsApi* | [**actions/addSelectedRepoToOrgSecret**](Apis/ActionsApi.http#actions/addselectedrepotoorgsecret) | **PUT** /orgs/{org}/actions/secrets/{secret_name}/repositories/{repository_id} | Add selected repository to an organization secret
*ActionsApi* | [**actions/addSelectedRepoToOrgVariable**](Apis/ActionsApi.http#actions/addselectedrepotoorgvariable) | **PUT** /orgs/{org}/actions/variables/{name}/repositories/{repository_id} | Add selected repository to an organization variable
*ActionsApi* | [**actions/approveWorkflowRun**](Apis/ActionsApi.http#actions/approveworkflowrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/approve | Approve a workflow run for a fork pull request
*ActionsApi* | [**actions/cancelWorkflowRun**](Apis/ActionsApi.http#actions/cancelworkflowrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/cancel | Cancel a workflow run
*ActionsApi* | [**actions/createEnvironmentVariable**](Apis/ActionsApi.http#actions/createenvironmentvariable) | **POST** /repositories/{repository_id}/environments/{environment_name}/variables | Create an environment variable
*ActionsApi* | [**actions/createOrUpdateEnvironmentSecret**](Apis/ActionsApi.http#actions/createorupdateenvironmentsecret) | **PUT** /repositories/{repository_id}/environments/{environment_name}/secrets/{secret_name} | Create or update an environment secret
*ActionsApi* | [**actions/createOrUpdateOrgSecret**](Apis/ActionsApi.http#actions/createorupdateorgsecret) | **PUT** /orgs/{org}/actions/secrets/{secret_name} | Create or update an organization secret
*ActionsApi* | [**actions/createOrUpdateRepoSecret**](Apis/ActionsApi.http#actions/createorupdatereposecret) | **PUT** /repos/{owner}/{repo}/actions/secrets/{secret_name} | Create or update a repository secret
*ActionsApi* | [**actions/createOrgVariable**](Apis/ActionsApi.http#actions/createorgvariable) | **POST** /orgs/{org}/actions/variables | Create an organization variable
*ActionsApi* | [**actions/createRegistrationTokenForOrg**](Apis/ActionsApi.http#actions/createregistrationtokenfororg) | **POST** /orgs/{org}/actions/runners/registration-token | Create a registration token for an organization
*ActionsApi* | [**actions/createRegistrationTokenForRepo**](Apis/ActionsApi.http#actions/createregistrationtokenforrepo) | **POST** /repos/{owner}/{repo}/actions/runners/registration-token | Create a registration token for a repository
*ActionsApi* | [**actions/createRemoveTokenForOrg**](Apis/ActionsApi.http#actions/createremovetokenfororg) | **POST** /orgs/{org}/actions/runners/remove-token | Create a remove token for an organization
*ActionsApi* | [**actions/createRemoveTokenForRepo**](Apis/ActionsApi.http#actions/createremovetokenforrepo) | **POST** /repos/{owner}/{repo}/actions/runners/remove-token | Create a remove token for a repository
*ActionsApi* | [**actions/createRepoVariable**](Apis/ActionsApi.http#actions/createrepovariable) | **POST** /repos/{owner}/{repo}/actions/variables | Create a repository variable
*ActionsApi* | [**actions/createWorkflowDispatch**](Apis/ActionsApi.http#actions/createworkflowdispatch) | **POST** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/dispatches | Create a workflow dispatch event
*ActionsApi* | [**actions/deleteActionsCacheById**](Apis/ActionsApi.http#actions/deleteactionscachebyid) | **DELETE** /repos/{owner}/{repo}/actions/caches/{cache_id} | Delete a GitHub Actions cache for a repository (using a cache ID)
*ActionsApi* | [**actions/deleteActionsCacheByKey**](Apis/ActionsApi.http#actions/deleteactionscachebykey) | **DELETE** /repos/{owner}/{repo}/actions/caches | Delete GitHub Actions caches for a repository (using a cache key)
*ActionsApi* | [**actions/deleteArtifact**](Apis/ActionsApi.http#actions/deleteartifact) | **DELETE** /repos/{owner}/{repo}/actions/artifacts/{artifact_id} | Delete an artifact
*ActionsApi* | [**actions/deleteEnvironmentSecret**](Apis/ActionsApi.http#actions/deleteenvironmentsecret) | **DELETE** /repositories/{repository_id}/environments/{environment_name}/secrets/{secret_name} | Delete an environment secret
*ActionsApi* | [**actions/deleteEnvironmentVariable**](Apis/ActionsApi.http#actions/deleteenvironmentvariable) | **DELETE** /repositories/{repository_id}/environments/{environment_name}/variables/{name} | Delete an environment variable
*ActionsApi* | [**actions/deleteOrgSecret**](Apis/ActionsApi.http#actions/deleteorgsecret) | **DELETE** /orgs/{org}/actions/secrets/{secret_name} | Delete an organization secret
*ActionsApi* | [**actions/deleteOrgVariable**](Apis/ActionsApi.http#actions/deleteorgvariable) | **DELETE** /orgs/{org}/actions/variables/{name} | Delete an organization variable
*ActionsApi* | [**actions/deleteRepoSecret**](Apis/ActionsApi.http#actions/deletereposecret) | **DELETE** /repos/{owner}/{repo}/actions/secrets/{secret_name} | Delete a repository secret
*ActionsApi* | [**actions/deleteRepoVariable**](Apis/ActionsApi.http#actions/deleterepovariable) | **DELETE** /repos/{owner}/{repo}/actions/variables/{name} | Delete a repository variable
*ActionsApi* | [**actions/deleteSelfHostedRunnerFromOrg**](Apis/ActionsApi.http#actions/deleteselfhostedrunnerfromorg) | **DELETE** /orgs/{org}/actions/runners/{runner_id} | Delete a self-hosted runner from an organization
*ActionsApi* | [**actions/deleteSelfHostedRunnerFromRepo**](Apis/ActionsApi.http#actions/deleteselfhostedrunnerfromrepo) | **DELETE** /repos/{owner}/{repo}/actions/runners/{runner_id} | Delete a self-hosted runner from a repository
*ActionsApi* | [**actions/deleteWorkflowRun**](Apis/ActionsApi.http#actions/deleteworkflowrun) | **DELETE** /repos/{owner}/{repo}/actions/runs/{run_id} | Delete a workflow run
*ActionsApi* | [**actions/deleteWorkflowRunLogs**](Apis/ActionsApi.http#actions/deleteworkflowrunlogs) | **DELETE** /repos/{owner}/{repo}/actions/runs/{run_id}/logs | Delete workflow run logs
*ActionsApi* | [**actions/disableSelectedRepositoryGithubActionsOrganization**](Apis/ActionsApi.http#actions/disableselectedrepositorygithubactionsorganization) | **DELETE** /orgs/{org}/actions/permissions/repositories/{repository_id} | Disable a selected repository for GitHub Actions in an organization
*ActionsApi* | [**actions/disableWorkflow**](Apis/ActionsApi.http#actions/disableworkflow) | **PUT** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/disable | Disable a workflow
*ActionsApi* | [**actions/downloadArtifact**](Apis/ActionsApi.http#actions/downloadartifact) | **GET** /repos/{owner}/{repo}/actions/artifacts/{artifact_id}/{archive_format} | Download an artifact
*ActionsApi* | [**actions/downloadJobLogsForWorkflowRun**](Apis/ActionsApi.http#actions/downloadjoblogsforworkflowrun) | **GET** /repos/{owner}/{repo}/actions/jobs/{job_id}/logs | Download job logs for a workflow run
*ActionsApi* | [**actions/downloadWorkflowRunAttemptLogs**](Apis/ActionsApi.http#actions/downloadworkflowrunattemptlogs) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/attempts/{attempt_number}/logs | Download workflow run attempt logs
*ActionsApi* | [**actions/downloadWorkflowRunLogs**](Apis/ActionsApi.http#actions/downloadworkflowrunlogs) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/logs | Download workflow run logs
*ActionsApi* | [**actions/enableSelectedRepositoryGithubActionsOrganization**](Apis/ActionsApi.http#actions/enableselectedrepositorygithubactionsorganization) | **PUT** /orgs/{org}/actions/permissions/repositories/{repository_id} | Enable a selected repository for GitHub Actions in an organization
*ActionsApi* | [**actions/enableWorkflow**](Apis/ActionsApi.http#actions/enableworkflow) | **PUT** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/enable | Enable a workflow
*ActionsApi* | [**actions/forceCancelWorkflowRun**](Apis/ActionsApi.http#actions/forcecancelworkflowrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/force-cancel | Force cancel a workflow run
*ActionsApi* | [**actions/generateRunnerJitconfigForOrg**](Apis/ActionsApi.http#actions/generaterunnerjitconfigfororg) | **POST** /orgs/{org}/actions/runners/generate-jitconfig | Create configuration for a just-in-time runner for an organization
*ActionsApi* | [**actions/generateRunnerJitconfigForRepo**](Apis/ActionsApi.http#actions/generaterunnerjitconfigforrepo) | **POST** /repos/{owner}/{repo}/actions/runners/generate-jitconfig | Create configuration for a just-in-time runner for a repository
*ActionsApi* | [**actions/getActionsCacheList**](Apis/ActionsApi.http#actions/getactionscachelist) | **GET** /repos/{owner}/{repo}/actions/caches | List GitHub Actions caches for a repository
*ActionsApi* | [**actions/getActionsCacheUsage**](Apis/ActionsApi.http#actions/getactionscacheusage) | **GET** /repos/{owner}/{repo}/actions/cache/usage | Get GitHub Actions cache usage for a repository
*ActionsApi* | [**actions/getActionsCacheUsageByRepoForOrg**](Apis/ActionsApi.http#actions/getactionscacheusagebyrepofororg) | **GET** /orgs/{org}/actions/cache/usage-by-repository | List repositories with GitHub Actions cache usage for an organization
*ActionsApi* | [**actions/getActionsCacheUsageForOrg**](Apis/ActionsApi.http#actions/getactionscacheusagefororg) | **GET** /orgs/{org}/actions/cache/usage | Get GitHub Actions cache usage for an organization
*ActionsApi* | [**actions/getAllowedActionsOrganization**](Apis/ActionsApi.http#actions/getallowedactionsorganization) | **GET** /orgs/{org}/actions/permissions/selected-actions | Get allowed actions and reusable workflows for an organization
*ActionsApi* | [**actions/getAllowedActionsRepository**](Apis/ActionsApi.http#actions/getallowedactionsrepository) | **GET** /repos/{owner}/{repo}/actions/permissions/selected-actions | Get allowed actions and reusable workflows for a repository
*ActionsApi* | [**actions/getArtifact**](Apis/ActionsApi.http#actions/getartifact) | **GET** /repos/{owner}/{repo}/actions/artifacts/{artifact_id} | Get an artifact
*ActionsApi* | [**actions/getCustomOidcSubClaimForRepo**](Apis/ActionsApi.http#actions/getcustomoidcsubclaimforrepo) | **GET** /repos/{owner}/{repo}/actions/oidc/customization/sub | Get the customization template for an OIDC subject claim for a repository
*ActionsApi* | [**actions/getEnvironmentPublicKey**](Apis/ActionsApi.http#actions/getenvironmentpublickey) | **GET** /repositories/{repository_id}/environments/{environment_name}/secrets/public-key | Get an environment public key
*ActionsApi* | [**actions/getEnvironmentSecret**](Apis/ActionsApi.http#actions/getenvironmentsecret) | **GET** /repositories/{repository_id}/environments/{environment_name}/secrets/{secret_name} | Get an environment secret
*ActionsApi* | [**actions/getEnvironmentVariable**](Apis/ActionsApi.http#actions/getenvironmentvariable) | **GET** /repositories/{repository_id}/environments/{environment_name}/variables/{name} | Get an environment variable
*ActionsApi* | [**actions/getGithubActionsDefaultWorkflowPermissionsOrganization**](Apis/ActionsApi.http#actions/getgithubactionsdefaultworkflowpermissionsorganization) | **GET** /orgs/{org}/actions/permissions/workflow | Get default workflow permissions for an organization
*ActionsApi* | [**actions/getGithubActionsDefaultWorkflowPermissionsRepository**](Apis/ActionsApi.http#actions/getgithubactionsdefaultworkflowpermissionsrepository) | **GET** /repos/{owner}/{repo}/actions/permissions/workflow | Get default workflow permissions for a repository
*ActionsApi* | [**actions/getGithubActionsPermissionsOrganization**](Apis/ActionsApi.http#actions/getgithubactionspermissionsorganization) | **GET** /orgs/{org}/actions/permissions | Get GitHub Actions permissions for an organization
*ActionsApi* | [**actions/getGithubActionsPermissionsRepository**](Apis/ActionsApi.http#actions/getgithubactionspermissionsrepository) | **GET** /repos/{owner}/{repo}/actions/permissions | Get GitHub Actions permissions for a repository
*ActionsApi* | [**actions/getJobForWorkflowRun**](Apis/ActionsApi.http#actions/getjobforworkflowrun) | **GET** /repos/{owner}/{repo}/actions/jobs/{job_id} | Get a job for a workflow run
*ActionsApi* | [**actions/getOrgPublicKey**](Apis/ActionsApi.http#actions/getorgpublickey) | **GET** /orgs/{org}/actions/secrets/public-key | Get an organization public key
*ActionsApi* | [**actions/getOrgSecret**](Apis/ActionsApi.http#actions/getorgsecret) | **GET** /orgs/{org}/actions/secrets/{secret_name} | Get an organization secret
*ActionsApi* | [**actions/getOrgVariable**](Apis/ActionsApi.http#actions/getorgvariable) | **GET** /orgs/{org}/actions/variables/{name} | Get an organization variable
*ActionsApi* | [**actions/getPendingDeploymentsForRun**](Apis/ActionsApi.http#actions/getpendingdeploymentsforrun) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/pending_deployments | Get pending deployments for a workflow run
*ActionsApi* | [**actions/getRepoPublicKey**](Apis/ActionsApi.http#actions/getrepopublickey) | **GET** /repos/{owner}/{repo}/actions/secrets/public-key | Get a repository public key
*ActionsApi* | [**actions/getRepoSecret**](Apis/ActionsApi.http#actions/getreposecret) | **GET** /repos/{owner}/{repo}/actions/secrets/{secret_name} | Get a repository secret
*ActionsApi* | [**actions/getRepoVariable**](Apis/ActionsApi.http#actions/getrepovariable) | **GET** /repos/{owner}/{repo}/actions/variables/{name} | Get a repository variable
*ActionsApi* | [**actions/getReviewsForRun**](Apis/ActionsApi.http#actions/getreviewsforrun) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/approvals | Get the review history for a workflow run
*ActionsApi* | [**actions/getSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actions/getselfhostedrunnerfororg) | **GET** /orgs/{org}/actions/runners/{runner_id} | Get a self-hosted runner for an organization
*ActionsApi* | [**actions/getSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actions/getselfhostedrunnerforrepo) | **GET** /repos/{owner}/{repo}/actions/runners/{runner_id} | Get a self-hosted runner for a repository
*ActionsApi* | [**actions/getWorkflow**](Apis/ActionsApi.http#actions/getworkflow) | **GET** /repos/{owner}/{repo}/actions/workflows/{workflow_id} | Get a workflow
*ActionsApi* | [**actions/getWorkflowAccessToRepository**](Apis/ActionsApi.http#actions/getworkflowaccesstorepository) | **GET** /repos/{owner}/{repo}/actions/permissions/access | Get the level of access for workflows outside of the repository
*ActionsApi* | [**actions/getWorkflowRun**](Apis/ActionsApi.http#actions/getworkflowrun) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id} | Get a workflow run
*ActionsApi* | [**actions/getWorkflowRunAttempt**](Apis/ActionsApi.http#actions/getworkflowrunattempt) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/attempts/{attempt_number} | Get a workflow run attempt
*ActionsApi* | [**actions/getWorkflowRunUsage**](Apis/ActionsApi.http#actions/getworkflowrunusage) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/timing | Get workflow run usage
*ActionsApi* | [**actions/getWorkflowUsage**](Apis/ActionsApi.http#actions/getworkflowusage) | **GET** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/timing | Get workflow usage
*ActionsApi* | [**actions/listArtifactsForRepo**](Apis/ActionsApi.http#actions/listartifactsforrepo) | **GET** /repos/{owner}/{repo}/actions/artifacts | List artifacts for a repository
*ActionsApi* | [**actions/listEnvironmentSecrets**](Apis/ActionsApi.http#actions/listenvironmentsecrets) | **GET** /repositories/{repository_id}/environments/{environment_name}/secrets | List environment secrets
*ActionsApi* | [**actions/listEnvironmentVariables**](Apis/ActionsApi.http#actions/listenvironmentvariables) | **GET** /repositories/{repository_id}/environments/{environment_name}/variables | List environment variables
*ActionsApi* | [**actions/listJobsForWorkflowRun**](Apis/ActionsApi.http#actions/listjobsforworkflowrun) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/jobs | List jobs for a workflow run
*ActionsApi* | [**actions/listJobsForWorkflowRunAttempt**](Apis/ActionsApi.http#actions/listjobsforworkflowrunattempt) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/attempts/{attempt_number}/jobs | List jobs for a workflow run attempt
*ActionsApi* | [**actions/listLabelsForSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actions/listlabelsforselfhostedrunnerfororg) | **GET** /orgs/{org}/actions/runners/{runner_id}/labels | List labels for a self-hosted runner for an organization
*ActionsApi* | [**actions/listLabelsForSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actions/listlabelsforselfhostedrunnerforrepo) | **GET** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels | List labels for a self-hosted runner for a repository
*ActionsApi* | [**actions/listOrgSecrets**](Apis/ActionsApi.http#actions/listorgsecrets) | **GET** /orgs/{org}/actions/secrets | List organization secrets
*ActionsApi* | [**actions/listOrgVariables**](Apis/ActionsApi.http#actions/listorgvariables) | **GET** /orgs/{org}/actions/variables | List organization variables
*ActionsApi* | [**actions/listRepoOrganizationSecrets**](Apis/ActionsApi.http#actions/listrepoorganizationsecrets) | **GET** /repos/{owner}/{repo}/actions/organization-secrets | List repository organization secrets
*ActionsApi* | [**actions/listRepoOrganizationVariables**](Apis/ActionsApi.http#actions/listrepoorganizationvariables) | **GET** /repos/{owner}/{repo}/actions/organization-variables | List repository organization variables
*ActionsApi* | [**actions/listRepoSecrets**](Apis/ActionsApi.http#actions/listreposecrets) | **GET** /repos/{owner}/{repo}/actions/secrets | List repository secrets
*ActionsApi* | [**actions/listRepoVariables**](Apis/ActionsApi.http#actions/listrepovariables) | **GET** /repos/{owner}/{repo}/actions/variables | List repository variables
*ActionsApi* | [**actions/listRepoWorkflows**](Apis/ActionsApi.http#actions/listrepoworkflows) | **GET** /repos/{owner}/{repo}/actions/workflows | List repository workflows
*ActionsApi* | [**actions/listRunnerApplicationsForOrg**](Apis/ActionsApi.http#actions/listrunnerapplicationsfororg) | **GET** /orgs/{org}/actions/runners/downloads | List runner applications for an organization
*ActionsApi* | [**actions/listRunnerApplicationsForRepo**](Apis/ActionsApi.http#actions/listrunnerapplicationsforrepo) | **GET** /repos/{owner}/{repo}/actions/runners/downloads | List runner applications for a repository
*ActionsApi* | [**actions/listSelectedReposForOrgSecret**](Apis/ActionsApi.http#actions/listselectedreposfororgsecret) | **GET** /orgs/{org}/actions/secrets/{secret_name}/repositories | List selected repositories for an organization secret
*ActionsApi* | [**actions/listSelectedReposForOrgVariable**](Apis/ActionsApi.http#actions/listselectedreposfororgvariable) | **GET** /orgs/{org}/actions/variables/{name}/repositories | List selected repositories for an organization variable
*ActionsApi* | [**actions/listSelectedRepositoriesEnabledGithubActionsOrganization**](Apis/ActionsApi.http#actions/listselectedrepositoriesenabledgithubactionsorganization) | **GET** /orgs/{org}/actions/permissions/repositories | List selected repositories enabled for GitHub Actions in an organization
*ActionsApi* | [**actions/listSelfHostedRunnersForOrg**](Apis/ActionsApi.http#actions/listselfhostedrunnersfororg) | **GET** /orgs/{org}/actions/runners | List self-hosted runners for an organization
*ActionsApi* | [**actions/listSelfHostedRunnersForRepo**](Apis/ActionsApi.http#actions/listselfhostedrunnersforrepo) | **GET** /repos/{owner}/{repo}/actions/runners | List self-hosted runners for a repository
*ActionsApi* | [**actions/listWorkflowRunArtifacts**](Apis/ActionsApi.http#actions/listworkflowrunartifacts) | **GET** /repos/{owner}/{repo}/actions/runs/{run_id}/artifacts | List workflow run artifacts
*ActionsApi* | [**actions/listWorkflowRuns**](Apis/ActionsApi.http#actions/listworkflowruns) | **GET** /repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs | List workflow runs for a workflow
*ActionsApi* | [**actions/listWorkflowRunsForRepo**](Apis/ActionsApi.http#actions/listworkflowrunsforrepo) | **GET** /repos/{owner}/{repo}/actions/runs | List workflow runs for a repository
*ActionsApi* | [**actions/reRunJobForWorkflowRun**](Apis/ActionsApi.http#actions/rerunjobforworkflowrun) | **POST** /repos/{owner}/{repo}/actions/jobs/{job_id}/rerun | Re-run a job from a workflow run
*ActionsApi* | [**actions/reRunWorkflow**](Apis/ActionsApi.http#actions/rerunworkflow) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/rerun | Re-run a workflow
*ActionsApi* | [**actions/reRunWorkflowFailedJobs**](Apis/ActionsApi.http#actions/rerunworkflowfailedjobs) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/rerun-failed-jobs | Re-run failed jobs from a workflow run
*ActionsApi* | [**actions/removeAllCustomLabelsFromSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actions/removeallcustomlabelsfromselfhostedrunnerfororg) | **DELETE** /orgs/{org}/actions/runners/{runner_id}/labels | Remove all custom labels from a self-hosted runner for an organization
*ActionsApi* | [**actions/removeAllCustomLabelsFromSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actions/removeallcustomlabelsfromselfhostedrunnerforrepo) | **DELETE** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels | Remove all custom labels from a self-hosted runner for a repository
*ActionsApi* | [**actions/removeCustomLabelFromSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actions/removecustomlabelfromselfhostedrunnerfororg) | **DELETE** /orgs/{org}/actions/runners/{runner_id}/labels/{name} | Remove a custom label from a self-hosted runner for an organization
*ActionsApi* | [**actions/removeCustomLabelFromSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actions/removecustomlabelfromselfhostedrunnerforrepo) | **DELETE** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels/{name} | Remove a custom label from a self-hosted runner for a repository
*ActionsApi* | [**actions/removeSelectedRepoFromOrgSecret**](Apis/ActionsApi.http#actions/removeselectedrepofromorgsecret) | **DELETE** /orgs/{org}/actions/secrets/{secret_name}/repositories/{repository_id} | Remove selected repository from an organization secret
*ActionsApi* | [**actions/removeSelectedRepoFromOrgVariable**](Apis/ActionsApi.http#actions/removeselectedrepofromorgvariable) | **DELETE** /orgs/{org}/actions/variables/{name}/repositories/{repository_id} | Remove selected repository from an organization variable
*ActionsApi* | [**actions/reviewCustomGatesForRun**](Apis/ActionsApi.http#actions/reviewcustomgatesforrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/deployment_protection_rule | Review custom deployment protection rules for a workflow run
*ActionsApi* | [**actions/reviewPendingDeploymentsForRun**](Apis/ActionsApi.http#actions/reviewpendingdeploymentsforrun) | **POST** /repos/{owner}/{repo}/actions/runs/{run_id}/pending_deployments | Review pending deployments for a workflow run
*ActionsApi* | [**actions/setAllowedActionsOrganization**](Apis/ActionsApi.http#actions/setallowedactionsorganization) | **PUT** /orgs/{org}/actions/permissions/selected-actions | Set allowed actions and reusable workflows for an organization
*ActionsApi* | [**actions/setAllowedActionsRepository**](Apis/ActionsApi.http#actions/setallowedactionsrepository) | **PUT** /repos/{owner}/{repo}/actions/permissions/selected-actions | Set allowed actions and reusable workflows for a repository
*ActionsApi* | [**actions/setCustomLabelsForSelfHostedRunnerForOrg**](Apis/ActionsApi.http#actions/setcustomlabelsforselfhostedrunnerfororg) | **PUT** /orgs/{org}/actions/runners/{runner_id}/labels | Set custom labels for a self-hosted runner for an organization
*ActionsApi* | [**actions/setCustomLabelsForSelfHostedRunnerForRepo**](Apis/ActionsApi.http#actions/setcustomlabelsforselfhostedrunnerforrepo) | **PUT** /repos/{owner}/{repo}/actions/runners/{runner_id}/labels | Set custom labels for a self-hosted runner for a repository
*ActionsApi* | [**actions/setCustomOidcSubClaimForRepo**](Apis/ActionsApi.http#actions/setcustomoidcsubclaimforrepo) | **PUT** /repos/{owner}/{repo}/actions/oidc/customization/sub | Set the customization template for an OIDC subject claim for a repository
*ActionsApi* | [**actions/setGithubActionsDefaultWorkflowPermissionsOrganization**](Apis/ActionsApi.http#actions/setgithubactionsdefaultworkflowpermissionsorganization) | **PUT** /orgs/{org}/actions/permissions/workflow | Set default workflow permissions for an organization
*ActionsApi* | [**actions/setGithubActionsDefaultWorkflowPermissionsRepository**](Apis/ActionsApi.http#actions/setgithubactionsdefaultworkflowpermissionsrepository) | **PUT** /repos/{owner}/{repo}/actions/permissions/workflow | Set default workflow permissions for a repository
*ActionsApi* | [**actions/setGithubActionsPermissionsOrganization**](Apis/ActionsApi.http#actions/setgithubactionspermissionsorganization) | **PUT** /orgs/{org}/actions/permissions | Set GitHub Actions permissions for an organization
*ActionsApi* | [**actions/setGithubActionsPermissionsRepository**](Apis/ActionsApi.http#actions/setgithubactionspermissionsrepository) | **PUT** /repos/{owner}/{repo}/actions/permissions | Set GitHub Actions permissions for a repository
*ActionsApi* | [**actions/setSelectedReposForOrgSecret**](Apis/ActionsApi.http#actions/setselectedreposfororgsecret) | **PUT** /orgs/{org}/actions/secrets/{secret_name}/repositories | Set selected repositories for an organization secret
*ActionsApi* | [**actions/setSelectedReposForOrgVariable**](Apis/ActionsApi.http#actions/setselectedreposfororgvariable) | **PUT** /orgs/{org}/actions/variables/{name}/repositories | Set selected repositories for an organization variable
*ActionsApi* | [**actions/setSelectedRepositoriesEnabledGithubActionsOrganization**](Apis/ActionsApi.http#actions/setselectedrepositoriesenabledgithubactionsorganization) | **PUT** /orgs/{org}/actions/permissions/repositories | Set selected repositories enabled for GitHub Actions in an organization
*ActionsApi* | [**actions/setWorkflowAccessToRepository**](Apis/ActionsApi.http#actions/setworkflowaccesstorepository) | **PUT** /repos/{owner}/{repo}/actions/permissions/access | Set the level of access for workflows outside of the repository
*ActionsApi* | [**actions/updateEnvironmentVariable**](Apis/ActionsApi.http#actions/updateenvironmentvariable) | **PATCH** /repositories/{repository_id}/environments/{environment_name}/variables/{name} | Update an environment variable
*ActionsApi* | [**actions/updateOrgVariable**](Apis/ActionsApi.http#actions/updateorgvariable) | **PATCH** /orgs/{org}/actions/variables/{name} | Update an organization variable
*ActionsApi* | [**actions/updateRepoVariable**](Apis/ActionsApi.http#actions/updaterepovariable) | **PATCH** /repos/{owner}/{repo}/actions/variables/{name} | Update a repository variable
*ActivityApi* | [**activity/checkRepoIsStarredByAuthenticatedUser**](Apis/ActivityApi.http#activity/checkrepoisstarredbyauthenticateduser) | **GET** /user/starred/{owner}/{repo} | Check if a repository is starred by the authenticated user
*ActivityApi* | [**activity/deleteRepoSubscription**](Apis/ActivityApi.http#activity/deletereposubscription) | **DELETE** /repos/{owner}/{repo}/subscription | Delete a repository subscription
*ActivityApi* | [**activity/deleteThreadSubscription**](Apis/ActivityApi.http#activity/deletethreadsubscription) | **DELETE** /notifications/threads/{thread_id}/subscription | Delete a thread subscription
*ActivityApi* | [**activity/getFeeds**](Apis/ActivityApi.http#activity/getfeeds) | **GET** /feeds | Get feeds
*ActivityApi* | [**activity/getRepoSubscription**](Apis/ActivityApi.http#activity/getreposubscription) | **GET** /repos/{owner}/{repo}/subscription | Get a repository subscription
*ActivityApi* | [**activity/getThread**](Apis/ActivityApi.http#activity/getthread) | **GET** /notifications/threads/{thread_id} | Get a thread
*ActivityApi* | [**activity/getThreadSubscriptionForAuthenticatedUser**](Apis/ActivityApi.http#activity/getthreadsubscriptionforauthenticateduser) | **GET** /notifications/threads/{thread_id}/subscription | Get a thread subscription for the authenticated user
*ActivityApi* | [**activity/listEventsForAuthenticatedUser**](Apis/ActivityApi.http#activity/listeventsforauthenticateduser) | **GET** /users/{username}/events | List events for the authenticated user
*ActivityApi* | [**activity/listNotificationsForAuthenticatedUser**](Apis/ActivityApi.http#activity/listnotificationsforauthenticateduser) | **GET** /notifications | List notifications for the authenticated user
*ActivityApi* | [**activity/listOrgEventsForAuthenticatedUser**](Apis/ActivityApi.http#activity/listorgeventsforauthenticateduser) | **GET** /users/{username}/events/orgs/{org} | List organization events for the authenticated user
*ActivityApi* | [**activity/listPublicEvents**](Apis/ActivityApi.http#activity/listpublicevents) | **GET** /events | List public events
*ActivityApi* | [**activity/listPublicEventsForRepoNetwork**](Apis/ActivityApi.http#activity/listpubliceventsforreponetwork) | **GET** /networks/{owner}/{repo}/events | List public events for a network of repositories
*ActivityApi* | [**activity/listPublicEventsForUser**](Apis/ActivityApi.http#activity/listpubliceventsforuser) | **GET** /users/{username}/events/public | List public events for a user
*ActivityApi* | [**activity/listPublicOrgEvents**](Apis/ActivityApi.http#activity/listpublicorgevents) | **GET** /orgs/{org}/events | List public organization events
*ActivityApi* | [**activity/listReceivedEventsForUser**](Apis/ActivityApi.http#activity/listreceivedeventsforuser) | **GET** /users/{username}/received_events | List events received by the authenticated user
*ActivityApi* | [**activity/listReceivedPublicEventsForUser**](Apis/ActivityApi.http#activity/listreceivedpubliceventsforuser) | **GET** /users/{username}/received_events/public | List public events received by a user
*ActivityApi* | [**activity/listRepoEvents**](Apis/ActivityApi.http#activity/listrepoevents) | **GET** /repos/{owner}/{repo}/events | List repository events
*ActivityApi* | [**activity/listRepoNotificationsForAuthenticatedUser**](Apis/ActivityApi.http#activity/listreponotificationsforauthenticateduser) | **GET** /repos/{owner}/{repo}/notifications | List repository notifications for the authenticated user
*ActivityApi* | [**activity/listReposStarredByAuthenticatedUser**](Apis/ActivityApi.http#activity/listreposstarredbyauthenticateduser) | **GET** /user/starred | List repositories starred by the authenticated user
*ActivityApi* | [**activity/listReposStarredByUser**](Apis/ActivityApi.http#activity/listreposstarredbyuser) | **GET** /users/{username}/starred | List repositories starred by a user
*ActivityApi* | [**activity/listReposWatchedByUser**](Apis/ActivityApi.http#activity/listreposwatchedbyuser) | **GET** /users/{username}/subscriptions | List repositories watched by a user
*ActivityApi* | [**activity/listStargazersForRepo**](Apis/ActivityApi.http#activity/liststargazersforrepo) | **GET** /repos/{owner}/{repo}/stargazers | List stargazers
*ActivityApi* | [**activity/listWatchedReposForAuthenticatedUser**](Apis/ActivityApi.http#activity/listwatchedreposforauthenticateduser) | **GET** /user/subscriptions | List repositories watched by the authenticated user
*ActivityApi* | [**activity/listWatchersForRepo**](Apis/ActivityApi.http#activity/listwatchersforrepo) | **GET** /repos/{owner}/{repo}/subscribers | List watchers
*ActivityApi* | [**activity/markNotificationsAsRead**](Apis/ActivityApi.http#activity/marknotificationsasread) | **PUT** /notifications | Mark notifications as read
*ActivityApi* | [**activity/markRepoNotificationsAsRead**](Apis/ActivityApi.http#activity/markreponotificationsasread) | **PUT** /repos/{owner}/{repo}/notifications | Mark repository notifications as read
*ActivityApi* | [**activity/markThreadAsDone**](Apis/ActivityApi.http#activity/markthreadasdone) | **DELETE** /notifications/threads/{thread_id} | Mark a thread as done
*ActivityApi* | [**activity/markThreadAsRead**](Apis/ActivityApi.http#activity/markthreadasread) | **PATCH** /notifications/threads/{thread_id} | Mark a thread as read
*ActivityApi* | [**activity/setRepoSubscription**](Apis/ActivityApi.http#activity/setreposubscription) | **PUT** /repos/{owner}/{repo}/subscription | Set a repository subscription
*ActivityApi* | [**activity/setThreadSubscription**](Apis/ActivityApi.http#activity/setthreadsubscription) | **PUT** /notifications/threads/{thread_id}/subscription | Set a thread subscription
*ActivityApi* | [**activity/starRepoForAuthenticatedUser**](Apis/ActivityApi.http#activity/starrepoforauthenticateduser) | **PUT** /user/starred/{owner}/{repo} | Star a repository for the authenticated user
*ActivityApi* | [**activity/unstarRepoForAuthenticatedUser**](Apis/ActivityApi.http#activity/unstarrepoforauthenticateduser) | **DELETE** /user/starred/{owner}/{repo} | Unstar a repository for the authenticated user
*AppsApi* | [**apps/addRepoToInstallationForAuthenticatedUser**](Apis/AppsApi.http#apps/addrepotoinstallationforauthenticateduser) | **PUT** /user/installations/{installation_id}/repositories/{repository_id} | Add a repository to an app installation
*AppsApi* | [**apps/checkToken**](Apis/AppsApi.http#apps/checktoken) | **POST** /applications/{client_id}/token | Check a token
*AppsApi* | [**apps/createFromManifest**](Apis/AppsApi.http#apps/createfrommanifest) | **POST** /app-manifests/{code}/conversions | Create a GitHub App from a manifest
*AppsApi* | [**apps/createInstallationAccessToken**](Apis/AppsApi.http#apps/createinstallationaccesstoken) | **POST** /app/installations/{installation_id}/access_tokens | Create an installation access token for an app
*AppsApi* | [**apps/deleteAuthorization**](Apis/AppsApi.http#apps/deleteauthorization) | **DELETE** /applications/{client_id}/grant | Delete an app authorization
*AppsApi* | [**apps/deleteInstallation**](Apis/AppsApi.http#apps/deleteinstallation) | **DELETE** /app/installations/{installation_id} | Delete an installation for the authenticated app
*AppsApi* | [**apps/deleteToken**](Apis/AppsApi.http#apps/deletetoken) | **DELETE** /applications/{client_id}/token | Delete an app token
*AppsApi* | [**apps/getAuthenticated**](Apis/AppsApi.http#apps/getauthenticated) | **GET** /app | Get the authenticated app
*AppsApi* | [**apps/getBySlug**](Apis/AppsApi.http#apps/getbyslug) | **GET** /apps/{app_slug} | Get an app
*AppsApi* | [**apps/getInstallation**](Apis/AppsApi.http#apps/getinstallation) | **GET** /app/installations/{installation_id} | Get an installation for the authenticated app
*AppsApi* | [**apps/getOrgInstallation**](Apis/AppsApi.http#apps/getorginstallation) | **GET** /orgs/{org}/installation | Get an organization installation for the authenticated app
*AppsApi* | [**apps/getRepoInstallation**](Apis/AppsApi.http#apps/getrepoinstallation) | **GET** /repos/{owner}/{repo}/installation | Get a repository installation for the authenticated app
*AppsApi* | [**apps/getSubscriptionPlanForAccount**](Apis/AppsApi.http#apps/getsubscriptionplanforaccount) | **GET** /marketplace_listing/accounts/{account_id} | Get a subscription plan for an account
*AppsApi* | [**apps/getSubscriptionPlanForAccountStubbed**](Apis/AppsApi.http#apps/getsubscriptionplanforaccountstubbed) | **GET** /marketplace_listing/stubbed/accounts/{account_id} | Get a subscription plan for an account (stubbed)
*AppsApi* | [**apps/getUserInstallation**](Apis/AppsApi.http#apps/getuserinstallation) | **GET** /users/{username}/installation | Get a user installation for the authenticated app
*AppsApi* | [**apps/getWebhookConfigForApp**](Apis/AppsApi.http#apps/getwebhookconfigforapp) | **GET** /app/hook/config | Get a webhook configuration for an app
*AppsApi* | [**apps/getWebhookDelivery**](Apis/AppsApi.http#apps/getwebhookdelivery) | **GET** /app/hook/deliveries/{delivery_id} | Get a delivery for an app webhook
*AppsApi* | [**apps/listAccountsForPlan**](Apis/AppsApi.http#apps/listaccountsforplan) | **GET** /marketplace_listing/plans/{plan_id}/accounts | List accounts for a plan
*AppsApi* | [**apps/listAccountsForPlanStubbed**](Apis/AppsApi.http#apps/listaccountsforplanstubbed) | **GET** /marketplace_listing/stubbed/plans/{plan_id}/accounts | List accounts for a plan (stubbed)
*AppsApi* | [**apps/listInstallationReposForAuthenticatedUser**](Apis/AppsApi.http#apps/listinstallationreposforauthenticateduser) | **GET** /user/installations/{installation_id}/repositories | List repositories accessible to the user access token
*AppsApi* | [**apps/listInstallationRequestsForAuthenticatedApp**](Apis/AppsApi.http#apps/listinstallationrequestsforauthenticatedapp) | **GET** /app/installation-requests | List installation requests for the authenticated app
*AppsApi* | [**apps/listInstallations**](Apis/AppsApi.http#apps/listinstallations) | **GET** /app/installations | List installations for the authenticated app
*AppsApi* | [**apps/listInstallationsForAuthenticatedUser**](Apis/AppsApi.http#apps/listinstallationsforauthenticateduser) | **GET** /user/installations | List app installations accessible to the user access token
*AppsApi* | [**apps/listPlans**](Apis/AppsApi.http#apps/listplans) | **GET** /marketplace_listing/plans | List plans
*AppsApi* | [**apps/listPlansStubbed**](Apis/AppsApi.http#apps/listplansstubbed) | **GET** /marketplace_listing/stubbed/plans | List plans (stubbed)
*AppsApi* | [**apps/listReposAccessibleToInstallation**](Apis/AppsApi.http#apps/listreposaccessibletoinstallation) | **GET** /installation/repositories | List repositories accessible to the app installation
*AppsApi* | [**apps/listSubscriptionsForAuthenticatedUser**](Apis/AppsApi.http#apps/listsubscriptionsforauthenticateduser) | **GET** /user/marketplace_purchases | List subscriptions for the authenticated user
*AppsApi* | [**apps/listSubscriptionsForAuthenticatedUserStubbed**](Apis/AppsApi.http#apps/listsubscriptionsforauthenticateduserstubbed) | **GET** /user/marketplace_purchases/stubbed | List subscriptions for the authenticated user (stubbed)
*AppsApi* | [**apps/listWebhookDeliveries**](Apis/AppsApi.http#apps/listwebhookdeliveries) | **GET** /app/hook/deliveries | List deliveries for an app webhook
*AppsApi* | [**apps/redeliverWebhookDelivery**](Apis/AppsApi.http#apps/redeliverwebhookdelivery) | **POST** /app/hook/deliveries/{delivery_id}/attempts | Redeliver a delivery for an app webhook
*AppsApi* | [**apps/removeRepoFromInstallationForAuthenticatedUser**](Apis/AppsApi.http#apps/removerepofrominstallationforauthenticateduser) | **DELETE** /user/installations/{installation_id}/repositories/{repository_id} | Remove a repository from an app installation
*AppsApi* | [**apps/resetToken**](Apis/AppsApi.http#apps/resettoken) | **PATCH** /applications/{client_id}/token | Reset a token
*AppsApi* | [**apps/revokeInstallationAccessToken**](Apis/AppsApi.http#apps/revokeinstallationaccesstoken) | **DELETE** /installation/token | Revoke an installation access token
*AppsApi* | [**apps/scopeToken**](Apis/AppsApi.http#apps/scopetoken) | **POST** /applications/{client_id}/token/scoped | Create a scoped access token
*AppsApi* | [**apps/suspendInstallation**](Apis/AppsApi.http#apps/suspendinstallation) | **PUT** /app/installations/{installation_id}/suspended | Suspend an app installation
*AppsApi* | [**apps/unsuspendInstallation**](Apis/AppsApi.http#apps/unsuspendinstallation) | **DELETE** /app/installations/{installation_id}/suspended | Unsuspend an app installation
*AppsApi* | [**apps/updateWebhookConfigForApp**](Apis/AppsApi.http#apps/updatewebhookconfigforapp) | **PATCH** /app/hook/config | Update a webhook configuration for an app
*BillingApi* | [**billing/getGithubActionsBillingOrg**](Apis/BillingApi.http#billing/getgithubactionsbillingorg) | **GET** /orgs/{org}/settings/billing/actions | Get GitHub Actions billing for an organization
*BillingApi* | [**billing/getGithubActionsBillingUser**](Apis/BillingApi.http#billing/getgithubactionsbillinguser) | **GET** /users/{username}/settings/billing/actions | Get GitHub Actions billing for a user
*BillingApi* | [**billing/getGithubPackagesBillingOrg**](Apis/BillingApi.http#billing/getgithubpackagesbillingorg) | **GET** /orgs/{org}/settings/billing/packages | Get GitHub Packages billing for an organization
*BillingApi* | [**billing/getGithubPackagesBillingUser**](Apis/BillingApi.http#billing/getgithubpackagesbillinguser) | **GET** /users/{username}/settings/billing/packages | Get GitHub Packages billing for a user
*BillingApi* | [**billing/getSharedStorageBillingOrg**](Apis/BillingApi.http#billing/getsharedstoragebillingorg) | **GET** /orgs/{org}/settings/billing/shared-storage | Get shared storage billing for an organization
*BillingApi* | [**billing/getSharedStorageBillingUser**](Apis/BillingApi.http#billing/getsharedstoragebillinguser) | **GET** /users/{username}/settings/billing/shared-storage | Get shared storage billing for a user
*ChecksApi* | [**checks/create**](Apis/ChecksApi.http#checks/create) | **POST** /repos/{owner}/{repo}/check-runs | Create a check run
*ChecksApi* | [**checks/createSuite**](Apis/ChecksApi.http#checks/createsuite) | **POST** /repos/{owner}/{repo}/check-suites | Create a check suite
*ChecksApi* | [**checks/get**](Apis/ChecksApi.http#checks/get) | **GET** /repos/{owner}/{repo}/check-runs/{check_run_id} | Get a check run
*ChecksApi* | [**checks/getSuite**](Apis/ChecksApi.http#checks/getsuite) | **GET** /repos/{owner}/{repo}/check-suites/{check_suite_id} | Get a check suite
*ChecksApi* | [**checks/listAnnotations**](Apis/ChecksApi.http#checks/listannotations) | **GET** /repos/{owner}/{repo}/check-runs/{check_run_id}/annotations | List check run annotations
*ChecksApi* | [**checks/listForRef**](Apis/ChecksApi.http#checks/listforref) | **GET** /repos/{owner}/{repo}/commits/{ref}/check-runs | List check runs for a Git reference
*ChecksApi* | [**checks/listForSuite**](Apis/ChecksApi.http#checks/listforsuite) | **GET** /repos/{owner}/{repo}/check-suites/{check_suite_id}/check-runs | List check runs in a check suite
*ChecksApi* | [**checks/listSuitesForRef**](Apis/ChecksApi.http#checks/listsuitesforref) | **GET** /repos/{owner}/{repo}/commits/{ref}/check-suites | List check suites for a Git reference
*ChecksApi* | [**checks/rerequestRun**](Apis/ChecksApi.http#checks/rerequestrun) | **POST** /repos/{owner}/{repo}/check-runs/{check_run_id}/rerequest | Rerequest a check run
*ChecksApi* | [**checks/rerequestSuite**](Apis/ChecksApi.http#checks/rerequestsuite) | **POST** /repos/{owner}/{repo}/check-suites/{check_suite_id}/rerequest | Rerequest a check suite
*ChecksApi* | [**checks/setSuitesPreferences**](Apis/ChecksApi.http#checks/setsuitespreferences) | **PATCH** /repos/{owner}/{repo}/check-suites/preferences | Update repository preferences for check suites
*ChecksApi* | [**checks/update**](Apis/ChecksApi.http#checks/update) | **PATCH** /repos/{owner}/{repo}/check-runs/{check_run_id} | Update a check run
*ClassroomApi* | [**classroom/getAClassroom**](Apis/ClassroomApi.http#classroom/getaclassroom) | **GET** /classrooms/{classroom_id} | Get a classroom
*ClassroomApi* | [**classroom/getAnAssignment**](Apis/ClassroomApi.http#classroom/getanassignment) | **GET** /assignments/{assignment_id} | Get an assignment
*ClassroomApi* | [**classroom/getAssignmentGrades**](Apis/ClassroomApi.http#classroom/getassignmentgrades) | **GET** /assignments/{assignment_id}/grades | Get assignment grades
*ClassroomApi* | [**classroom/listAcceptedAssigmentsForAnAssignment**](Apis/ClassroomApi.http#classroom/listacceptedassigmentsforanassignment) | **GET** /assignments/{assignment_id}/accepted_assignments | List accepted assignments for an assignment
*ClassroomApi* | [**classroom/listAssignmentsForAClassroom**](Apis/ClassroomApi.http#classroom/listassignmentsforaclassroom) | **GET** /classrooms/{classroom_id}/assignments | List assignments for a classroom
*ClassroomApi* | [**classroom/listClassrooms**](Apis/ClassroomApi.http#classroom/listclassrooms) | **GET** /classrooms | List classrooms
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
*CodespacesApi* | [**codespaces/addRepositoryForSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/addrepositoryforsecretforauthenticateduser) | **PUT** /user/codespaces/secrets/{secret_name}/repositories/{repository_id} | Add a selected repository to a user secret
*CodespacesApi* | [**codespaces/addSelectedRepoToOrgSecret**](Apis/CodespacesApi.http#codespaces/addselectedrepotoorgsecret) | **PUT** /orgs/{org}/codespaces/secrets/{secret_name}/repositories/{repository_id} | Add selected repository to an organization secret
*CodespacesApi* | [**codespaces/checkPermissionsForDevcontainer**](Apis/CodespacesApi.http#codespaces/checkpermissionsfordevcontainer) | **GET** /repos/{owner}/{repo}/codespaces/permissions_check | Check if permissions defined by a devcontainer have been accepted by the authenticated user
*CodespacesApi* | [**codespaces/codespaceMachinesForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/codespacemachinesforauthenticateduser) | **GET** /user/codespaces/{codespace_name}/machines | List machine types for a codespace
*CodespacesApi* | [**codespaces/createForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/createforauthenticateduser) | **POST** /user/codespaces | Create a codespace for the authenticated user
*CodespacesApi* | [**codespaces/createOrUpdateOrgSecret**](Apis/CodespacesApi.http#codespaces/createorupdateorgsecret) | **PUT** /orgs/{org}/codespaces/secrets/{secret_name} | Create or update an organization secret
*CodespacesApi* | [**codespaces/createOrUpdateRepoSecret**](Apis/CodespacesApi.http#codespaces/createorupdatereposecret) | **PUT** /repos/{owner}/{repo}/codespaces/secrets/{secret_name} | Create or update a repository secret
*CodespacesApi* | [**codespaces/createOrUpdateSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/createorupdatesecretforauthenticateduser) | **PUT** /user/codespaces/secrets/{secret_name} | Create or update a secret for the authenticated user
*CodespacesApi* | [**codespaces/createWithPrForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/createwithprforauthenticateduser) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/codespaces | Create a codespace from a pull request
*CodespacesApi* | [**codespaces/createWithRepoForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/createwithrepoforauthenticateduser) | **POST** /repos/{owner}/{repo}/codespaces | Create a codespace in a repository
*CodespacesApi* | [**codespaces/deleteCodespacesAccessUsers**](Apis/CodespacesApi.http#codespaces/deletecodespacesaccessusers) | **DELETE** /orgs/{org}/codespaces/access/selected_users | Remove users from Codespaces access for an organization
*CodespacesApi* | [**codespaces/deleteForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/deleteforauthenticateduser) | **DELETE** /user/codespaces/{codespace_name} | Delete a codespace for the authenticated user
*CodespacesApi* | [**codespaces/deleteFromOrganization**](Apis/CodespacesApi.http#codespaces/deletefromorganization) | **DELETE** /orgs/{org}/members/{username}/codespaces/{codespace_name} | Delete a codespace from the organization
*CodespacesApi* | [**codespaces/deleteOrgSecret**](Apis/CodespacesApi.http#codespaces/deleteorgsecret) | **DELETE** /orgs/{org}/codespaces/secrets/{secret_name} | Delete an organization secret
*CodespacesApi* | [**codespaces/deleteRepoSecret**](Apis/CodespacesApi.http#codespaces/deletereposecret) | **DELETE** /repos/{owner}/{repo}/codespaces/secrets/{secret_name} | Delete a repository secret
*CodespacesApi* | [**codespaces/deleteSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/deletesecretforauthenticateduser) | **DELETE** /user/codespaces/secrets/{secret_name} | Delete a secret for the authenticated user
*CodespacesApi* | [**codespaces/exportForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/exportforauthenticateduser) | **POST** /user/codespaces/{codespace_name}/exports | Export a codespace for the authenticated user
*CodespacesApi* | [**codespaces/getCodespacesForUserInOrg**](Apis/CodespacesApi.http#codespaces/getcodespacesforuserinorg) | **GET** /orgs/{org}/members/{username}/codespaces | List codespaces for a user in organization
*CodespacesApi* | [**codespaces/getExportDetailsForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/getexportdetailsforauthenticateduser) | **GET** /user/codespaces/{codespace_name}/exports/{export_id} | Get details about a codespace export
*CodespacesApi* | [**codespaces/getForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/getforauthenticateduser) | **GET** /user/codespaces/{codespace_name} | Get a codespace for the authenticated user
*CodespacesApi* | [**codespaces/getOrgPublicKey**](Apis/CodespacesApi.http#codespaces/getorgpublickey) | **GET** /orgs/{org}/codespaces/secrets/public-key | Get an organization public key
*CodespacesApi* | [**codespaces/getOrgSecret**](Apis/CodespacesApi.http#codespaces/getorgsecret) | **GET** /orgs/{org}/codespaces/secrets/{secret_name} | Get an organization secret
*CodespacesApi* | [**codespaces/getPublicKeyForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/getpublickeyforauthenticateduser) | **GET** /user/codespaces/secrets/public-key | Get public key for the authenticated user
*CodespacesApi* | [**codespaces/getRepoPublicKey**](Apis/CodespacesApi.http#codespaces/getrepopublickey) | **GET** /repos/{owner}/{repo}/codespaces/secrets/public-key | Get a repository public key
*CodespacesApi* | [**codespaces/getRepoSecret**](Apis/CodespacesApi.http#codespaces/getreposecret) | **GET** /repos/{owner}/{repo}/codespaces/secrets/{secret_name} | Get a repository secret
*CodespacesApi* | [**codespaces/getSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/getsecretforauthenticateduser) | **GET** /user/codespaces/secrets/{secret_name} | Get a secret for the authenticated user
*CodespacesApi* | [**codespaces/listDevcontainersInRepositoryForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/listdevcontainersinrepositoryforauthenticateduser) | **GET** /repos/{owner}/{repo}/codespaces/devcontainers | List devcontainer configurations in a repository for the authenticated user
*CodespacesApi* | [**codespaces/listForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/listforauthenticateduser) | **GET** /user/codespaces | List codespaces for the authenticated user
*CodespacesApi* | [**codespaces/listInOrganization**](Apis/CodespacesApi.http#codespaces/listinorganization) | **GET** /orgs/{org}/codespaces | List codespaces for the organization
*CodespacesApi* | [**codespaces/listInRepositoryForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/listinrepositoryforauthenticateduser) | **GET** /repos/{owner}/{repo}/codespaces | List codespaces in a repository for the authenticated user
*CodespacesApi* | [**codespaces/listOrgSecrets**](Apis/CodespacesApi.http#codespaces/listorgsecrets) | **GET** /orgs/{org}/codespaces/secrets | List organization secrets
*CodespacesApi* | [**codespaces/listRepoSecrets**](Apis/CodespacesApi.http#codespaces/listreposecrets) | **GET** /repos/{owner}/{repo}/codespaces/secrets | List repository secrets
*CodespacesApi* | [**codespaces/listRepositoriesForSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/listrepositoriesforsecretforauthenticateduser) | **GET** /user/codespaces/secrets/{secret_name}/repositories | List selected repositories for a user secret
*CodespacesApi* | [**codespaces/listSecretsForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/listsecretsforauthenticateduser) | **GET** /user/codespaces/secrets | List secrets for the authenticated user
*CodespacesApi* | [**codespaces/listSelectedReposForOrgSecret**](Apis/CodespacesApi.http#codespaces/listselectedreposfororgsecret) | **GET** /orgs/{org}/codespaces/secrets/{secret_name}/repositories | List selected repositories for an organization secret
*CodespacesApi* | [**codespaces/preFlightWithRepoForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/preflightwithrepoforauthenticateduser) | **GET** /repos/{owner}/{repo}/codespaces/new | Get default attributes for a codespace
*CodespacesApi* | [**codespaces/publishForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/publishforauthenticateduser) | **POST** /user/codespaces/{codespace_name}/publish | Create a repository from an unpublished codespace
*CodespacesApi* | [**codespaces/removeRepositoryForSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/removerepositoryforsecretforauthenticateduser) | **DELETE** /user/codespaces/secrets/{secret_name}/repositories/{repository_id} | Remove a selected repository from a user secret
*CodespacesApi* | [**codespaces/removeSelectedRepoFromOrgSecret**](Apis/CodespacesApi.http#codespaces/removeselectedrepofromorgsecret) | **DELETE** /orgs/{org}/codespaces/secrets/{secret_name}/repositories/{repository_id} | Remove selected repository from an organization secret
*CodespacesApi* | [**codespaces/repoMachinesForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/repomachinesforauthenticateduser) | **GET** /repos/{owner}/{repo}/codespaces/machines | List available machine types for a repository
*CodespacesApi* | [**codespaces/setCodespacesAccess**](Apis/CodespacesApi.http#codespaces/setcodespacesaccess) | **PUT** /orgs/{org}/codespaces/access | Manage access control for organization codespaces
*CodespacesApi* | [**codespaces/setCodespacesAccessUsers**](Apis/CodespacesApi.http#codespaces/setcodespacesaccessusers) | **POST** /orgs/{org}/codespaces/access/selected_users | Add users to Codespaces access for an organization
*CodespacesApi* | [**codespaces/setRepositoriesForSecretForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/setrepositoriesforsecretforauthenticateduser) | **PUT** /user/codespaces/secrets/{secret_name}/repositories | Set selected repositories for a user secret
*CodespacesApi* | [**codespaces/setSelectedReposForOrgSecret**](Apis/CodespacesApi.http#codespaces/setselectedreposfororgsecret) | **PUT** /orgs/{org}/codespaces/secrets/{secret_name}/repositories | Set selected repositories for an organization secret
*CodespacesApi* | [**codespaces/startForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/startforauthenticateduser) | **POST** /user/codespaces/{codespace_name}/start | Start a codespace for the authenticated user
*CodespacesApi* | [**codespaces/stopForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/stopforauthenticateduser) | **POST** /user/codespaces/{codespace_name}/stop | Stop a codespace for the authenticated user
*CodespacesApi* | [**codespaces/stopInOrganization**](Apis/CodespacesApi.http#codespaces/stopinorganization) | **POST** /orgs/{org}/members/{username}/codespaces/{codespace_name}/stop | Stop a codespace for an organization user
*CodespacesApi* | [**codespaces/updateForAuthenticatedUser**](Apis/CodespacesApi.http#codespaces/updateforauthenticateduser) | **PATCH** /user/codespaces/{codespace_name} | Update a codespace for the authenticated user
*CopilotApi* | [**copilot/addCopilotSeatsForTeams**](Apis/CopilotApi.http#copilot/addcopilotseatsforteams) | **POST** /orgs/{org}/copilot/billing/selected_teams | Add teams to the Copilot subscription for an organization
*CopilotApi* | [**copilot/addCopilotSeatsForUsers**](Apis/CopilotApi.http#copilot/addcopilotseatsforusers) | **POST** /orgs/{org}/copilot/billing/selected_users | Add users to the Copilot subscription for an organization
*CopilotApi* | [**copilot/cancelCopilotSeatAssignmentForTeams**](Apis/CopilotApi.http#copilot/cancelcopilotseatassignmentforteams) | **DELETE** /orgs/{org}/copilot/billing/selected_teams | Remove teams from the Copilot subscription for an organization
*CopilotApi* | [**copilot/cancelCopilotSeatAssignmentForUsers**](Apis/CopilotApi.http#copilot/cancelcopilotseatassignmentforusers) | **DELETE** /orgs/{org}/copilot/billing/selected_users | Remove users from the Copilot subscription for an organization
*CopilotApi* | [**copilot/getCopilotOrganizationDetails**](Apis/CopilotApi.http#copilot/getcopilotorganizationdetails) | **GET** /orgs/{org}/copilot/billing | Get Copilot seat information and settings for an organization
*CopilotApi* | [**copilot/getCopilotSeatDetailsForUser**](Apis/CopilotApi.http#copilot/getcopilotseatdetailsforuser) | **GET** /orgs/{org}/members/{username}/copilot | Get Copilot seat assignment details for a user
*CopilotApi* | [**copilot/listCopilotSeats**](Apis/CopilotApi.http#copilot/listcopilotseats) | **GET** /orgs/{org}/copilot/billing/seats | List all Copilot seat assignments for an organization
*DependabotApi* | [**dependabot/addSelectedRepoToOrgSecret**](Apis/DependabotApi.http#dependabot/addselectedrepotoorgsecret) | **PUT** /orgs/{org}/dependabot/secrets/{secret_name}/repositories/{repository_id} | Add selected repository to an organization secret
*DependabotApi* | [**dependabot/createOrUpdateOrgSecret**](Apis/DependabotApi.http#dependabot/createorupdateorgsecret) | **PUT** /orgs/{org}/dependabot/secrets/{secret_name} | Create or update an organization secret
*DependabotApi* | [**dependabot/createOrUpdateRepoSecret**](Apis/DependabotApi.http#dependabot/createorupdatereposecret) | **PUT** /repos/{owner}/{repo}/dependabot/secrets/{secret_name} | Create or update a repository secret
*DependabotApi* | [**dependabot/deleteOrgSecret**](Apis/DependabotApi.http#dependabot/deleteorgsecret) | **DELETE** /orgs/{org}/dependabot/secrets/{secret_name} | Delete an organization secret
*DependabotApi* | [**dependabot/deleteRepoSecret**](Apis/DependabotApi.http#dependabot/deletereposecret) | **DELETE** /repos/{owner}/{repo}/dependabot/secrets/{secret_name} | Delete a repository secret
*DependabotApi* | [**dependabot/getAlert**](Apis/DependabotApi.http#dependabot/getalert) | **GET** /repos/{owner}/{repo}/dependabot/alerts/{alert_number} | Get a Dependabot alert
*DependabotApi* | [**dependabot/getOrgPublicKey**](Apis/DependabotApi.http#dependabot/getorgpublickey) | **GET** /orgs/{org}/dependabot/secrets/public-key | Get an organization public key
*DependabotApi* | [**dependabot/getOrgSecret**](Apis/DependabotApi.http#dependabot/getorgsecret) | **GET** /orgs/{org}/dependabot/secrets/{secret_name} | Get an organization secret
*DependabotApi* | [**dependabot/getRepoPublicKey**](Apis/DependabotApi.http#dependabot/getrepopublickey) | **GET** /repos/{owner}/{repo}/dependabot/secrets/public-key | Get a repository public key
*DependabotApi* | [**dependabot/getRepoSecret**](Apis/DependabotApi.http#dependabot/getreposecret) | **GET** /repos/{owner}/{repo}/dependabot/secrets/{secret_name} | Get a repository secret
*DependabotApi* | [**dependabot/listAlertsForEnterprise**](Apis/DependabotApi.http#dependabot/listalertsforenterprise) | **GET** /enterprises/{enterprise}/dependabot/alerts | List Dependabot alerts for an enterprise
*DependabotApi* | [**dependabot/listAlertsForOrg**](Apis/DependabotApi.http#dependabot/listalertsfororg) | **GET** /orgs/{org}/dependabot/alerts | List Dependabot alerts for an organization
*DependabotApi* | [**dependabot/listAlertsForRepo**](Apis/DependabotApi.http#dependabot/listalertsforrepo) | **GET** /repos/{owner}/{repo}/dependabot/alerts | List Dependabot alerts for a repository
*DependabotApi* | [**dependabot/listOrgSecrets**](Apis/DependabotApi.http#dependabot/listorgsecrets) | **GET** /orgs/{org}/dependabot/secrets | List organization secrets
*DependabotApi* | [**dependabot/listRepoSecrets**](Apis/DependabotApi.http#dependabot/listreposecrets) | **GET** /repos/{owner}/{repo}/dependabot/secrets | List repository secrets
*DependabotApi* | [**dependabot/listSelectedReposForOrgSecret**](Apis/DependabotApi.http#dependabot/listselectedreposfororgsecret) | **GET** /orgs/{org}/dependabot/secrets/{secret_name}/repositories | List selected repositories for an organization secret
*DependabotApi* | [**dependabot/removeSelectedRepoFromOrgSecret**](Apis/DependabotApi.http#dependabot/removeselectedrepofromorgsecret) | **DELETE** /orgs/{org}/dependabot/secrets/{secret_name}/repositories/{repository_id} | Remove selected repository from an organization secret
*DependabotApi* | [**dependabot/setSelectedReposForOrgSecret**](Apis/DependabotApi.http#dependabot/setselectedreposfororgsecret) | **PUT** /orgs/{org}/dependabot/secrets/{secret_name}/repositories | Set selected repositories for an organization secret
*DependabotApi* | [**dependabot/updateAlert**](Apis/DependabotApi.http#dependabot/updatealert) | **PATCH** /repos/{owner}/{repo}/dependabot/alerts/{alert_number} | Update a Dependabot alert
*DependencyGraphApi* | [**dependencyGraph/createRepositorySnapshot**](Apis/DependencyGraphApi.http#dependencygraph/createrepositorysnapshot) | **POST** /repos/{owner}/{repo}/dependency-graph/snapshots | Create a snapshot of dependencies for a repository
*DependencyGraphApi* | [**dependencyGraph/diffRange**](Apis/DependencyGraphApi.http#dependencygraph/diffrange) | **GET** /repos/{owner}/{repo}/dependency-graph/compare/{basehead} | Get a diff of the dependencies between commits
*DependencyGraphApi* | [**dependencyGraph/exportSbom**](Apis/DependencyGraphApi.http#dependencygraph/exportsbom) | **GET** /repos/{owner}/{repo}/dependency-graph/sbom | Export a software bill of materials (SBOM) for a repository.
*EmojisApi* | [**emojis/get**](Apis/EmojisApi.http#emojis/get) | **GET** /emojis | Get emojis
*GistsApi* | [**gists/checkIsStarred**](Apis/GistsApi.http#gists/checkisstarred) | **GET** /gists/{gist_id}/star | Check if a gist is starred
*GistsApi* | [**gists/create**](Apis/GistsApi.http#gists/create) | **POST** /gists | Create a gist
*GistsApi* | [**gists/createComment**](Apis/GistsApi.http#gists/createcomment) | **POST** /gists/{gist_id}/comments | Create a gist comment
*GistsApi* | [**gists/delete**](Apis/GistsApi.http#gists/delete) | **DELETE** /gists/{gist_id} | Delete a gist
*GistsApi* | [**gists/deleteComment**](Apis/GistsApi.http#gists/deletecomment) | **DELETE** /gists/{gist_id}/comments/{comment_id} | Delete a gist comment
*GistsApi* | [**gists/fork**](Apis/GistsApi.http#gists/fork) | **POST** /gists/{gist_id}/forks | Fork a gist
*GistsApi* | [**gists/get**](Apis/GistsApi.http#gists/get) | **GET** /gists/{gist_id} | Get a gist
*GistsApi* | [**gists/getComment**](Apis/GistsApi.http#gists/getcomment) | **GET** /gists/{gist_id}/comments/{comment_id} | Get a gist comment
*GistsApi* | [**gists/getRevision**](Apis/GistsApi.http#gists/getrevision) | **GET** /gists/{gist_id}/{sha} | Get a gist revision
*GistsApi* | [**gists/list**](Apis/GistsApi.http#gists/list) | **GET** /gists | List gists for the authenticated user
*GistsApi* | [**gists/listComments**](Apis/GistsApi.http#gists/listcomments) | **GET** /gists/{gist_id}/comments | List gist comments
*GistsApi* | [**gists/listCommits**](Apis/GistsApi.http#gists/listcommits) | **GET** /gists/{gist_id}/commits | List gist commits
*GistsApi* | [**gists/listForUser**](Apis/GistsApi.http#gists/listforuser) | **GET** /users/{username}/gists | List gists for a user
*GistsApi* | [**gists/listForks**](Apis/GistsApi.http#gists/listforks) | **GET** /gists/{gist_id}/forks | List gist forks
*GistsApi* | [**gists/listPublic**](Apis/GistsApi.http#gists/listpublic) | **GET** /gists/public | List public gists
*GistsApi* | [**gists/listStarred**](Apis/GistsApi.http#gists/liststarred) | **GET** /gists/starred | List starred gists
*GistsApi* | [**gists/star**](Apis/GistsApi.http#gists/star) | **PUT** /gists/{gist_id}/star | Star a gist
*GistsApi* | [**gists/unstar**](Apis/GistsApi.http#gists/unstar) | **DELETE** /gists/{gist_id}/star | Unstar a gist
*GistsApi* | [**gists/update**](Apis/GistsApi.http#gists/update) | **PATCH** /gists/{gist_id} | Update a gist
*GistsApi* | [**gists/updateComment**](Apis/GistsApi.http#gists/updatecomment) | **PATCH** /gists/{gist_id}/comments/{comment_id} | Update a gist comment
*GitApi* | [**git/createBlob**](Apis/GitApi.http#git/createblob) | **POST** /repos/{owner}/{repo}/git/blobs | Create a blob
*GitApi* | [**git/createCommit**](Apis/GitApi.http#git/createcommit) | **POST** /repos/{owner}/{repo}/git/commits | Create a commit
*GitApi* | [**git/createRef**](Apis/GitApi.http#git/createref) | **POST** /repos/{owner}/{repo}/git/refs | Create a reference
*GitApi* | [**git/createTag**](Apis/GitApi.http#git/createtag) | **POST** /repos/{owner}/{repo}/git/tags | Create a tag object
*GitApi* | [**git/createTree**](Apis/GitApi.http#git/createtree) | **POST** /repos/{owner}/{repo}/git/trees | Create a tree
*GitApi* | [**git/deleteRef**](Apis/GitApi.http#git/deleteref) | **DELETE** /repos/{owner}/{repo}/git/refs/{ref} | Delete a reference
*GitApi* | [**git/getBlob**](Apis/GitApi.http#git/getblob) | **GET** /repos/{owner}/{repo}/git/blobs/{file_sha} | Get a blob
*GitApi* | [**git/getCommit**](Apis/GitApi.http#git/getcommit) | **GET** /repos/{owner}/{repo}/git/commits/{commit_sha} | Get a commit object
*GitApi* | [**git/getRef**](Apis/GitApi.http#git/getref) | **GET** /repos/{owner}/{repo}/git/ref/{ref} | Get a reference
*GitApi* | [**git/getTag**](Apis/GitApi.http#git/gettag) | **GET** /repos/{owner}/{repo}/git/tags/{tag_sha} | Get a tag
*GitApi* | [**git/getTree**](Apis/GitApi.http#git/gettree) | **GET** /repos/{owner}/{repo}/git/trees/{tree_sha} | Get a tree
*GitApi* | [**git/listMatchingRefs**](Apis/GitApi.http#git/listmatchingrefs) | **GET** /repos/{owner}/{repo}/git/matching-refs/{ref} | List matching references
*GitApi* | [**git/updateRef**](Apis/GitApi.http#git/updateref) | **PATCH** /repos/{owner}/{repo}/git/refs/{ref} | Update a reference
*GitignoreApi* | [**gitignore/getAllTemplates**](Apis/GitignoreApi.http#gitignore/getalltemplates) | **GET** /gitignore/templates | Get all gitignore templates
*GitignoreApi* | [**gitignore/getTemplate**](Apis/GitignoreApi.http#gitignore/gettemplate) | **GET** /gitignore/templates/{name} | Get a gitignore template
*InteractionsApi* | [**interactions/getRestrictionsForAuthenticatedUser**](Apis/InteractionsApi.http#interactions/getrestrictionsforauthenticateduser) | **GET** /user/interaction-limits | Get interaction restrictions for your public repositories
*InteractionsApi* | [**interactions/getRestrictionsForOrg**](Apis/InteractionsApi.http#interactions/getrestrictionsfororg) | **GET** /orgs/{org}/interaction-limits | Get interaction restrictions for an organization
*InteractionsApi* | [**interactions/getRestrictionsForRepo**](Apis/InteractionsApi.http#interactions/getrestrictionsforrepo) | **GET** /repos/{owner}/{repo}/interaction-limits | Get interaction restrictions for a repository
*InteractionsApi* | [**interactions/removeRestrictionsForAuthenticatedUser**](Apis/InteractionsApi.http#interactions/removerestrictionsforauthenticateduser) | **DELETE** /user/interaction-limits | Remove interaction restrictions from your public repositories
*InteractionsApi* | [**interactions/removeRestrictionsForOrg**](Apis/InteractionsApi.http#interactions/removerestrictionsfororg) | **DELETE** /orgs/{org}/interaction-limits | Remove interaction restrictions for an organization
*InteractionsApi* | [**interactions/removeRestrictionsForRepo**](Apis/InteractionsApi.http#interactions/removerestrictionsforrepo) | **DELETE** /repos/{owner}/{repo}/interaction-limits | Remove interaction restrictions for a repository
*InteractionsApi* | [**interactions/setRestrictionsForAuthenticatedUser**](Apis/InteractionsApi.http#interactions/setrestrictionsforauthenticateduser) | **PUT** /user/interaction-limits | Set interaction restrictions for your public repositories
*InteractionsApi* | [**interactions/setRestrictionsForOrg**](Apis/InteractionsApi.http#interactions/setrestrictionsfororg) | **PUT** /orgs/{org}/interaction-limits | Set interaction restrictions for an organization
*InteractionsApi* | [**interactions/setRestrictionsForRepo**](Apis/InteractionsApi.http#interactions/setrestrictionsforrepo) | **PUT** /repos/{owner}/{repo}/interaction-limits | Set interaction restrictions for a repository
*IssuesApi* | [**issues/addAssignees**](Apis/IssuesApi.http#issues/addassignees) | **POST** /repos/{owner}/{repo}/issues/{issue_number}/assignees | Add assignees to an issue
*IssuesApi* | [**issues/addLabels**](Apis/IssuesApi.http#issues/addlabels) | **POST** /repos/{owner}/{repo}/issues/{issue_number}/labels | Add labels to an issue
*IssuesApi* | [**issues/checkUserCanBeAssigned**](Apis/IssuesApi.http#issues/checkusercanbeassigned) | **GET** /repos/{owner}/{repo}/assignees/{assignee} | Check if a user can be assigned
*IssuesApi* | [**issues/checkUserCanBeAssignedToIssue**](Apis/IssuesApi.http#issues/checkusercanbeassignedtoissue) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/assignees/{assignee} | Check if a user can be assigned to a issue
*IssuesApi* | [**issues/create**](Apis/IssuesApi.http#issues/create) | **POST** /repos/{owner}/{repo}/issues | Create an issue
*IssuesApi* | [**issues/createComment**](Apis/IssuesApi.http#issues/createcomment) | **POST** /repos/{owner}/{repo}/issues/{issue_number}/comments | Create an issue comment
*IssuesApi* | [**issues/createLabel**](Apis/IssuesApi.http#issues/createlabel) | **POST** /repos/{owner}/{repo}/labels | Create a label
*IssuesApi* | [**issues/createMilestone**](Apis/IssuesApi.http#issues/createmilestone) | **POST** /repos/{owner}/{repo}/milestones | Create a milestone
*IssuesApi* | [**issues/deleteComment**](Apis/IssuesApi.http#issues/deletecomment) | **DELETE** /repos/{owner}/{repo}/issues/comments/{comment_id} | Delete an issue comment
*IssuesApi* | [**issues/deleteLabel**](Apis/IssuesApi.http#issues/deletelabel) | **DELETE** /repos/{owner}/{repo}/labels/{name} | Delete a label
*IssuesApi* | [**issues/deleteMilestone**](Apis/IssuesApi.http#issues/deletemilestone) | **DELETE** /repos/{owner}/{repo}/milestones/{milestone_number} | Delete a milestone
*IssuesApi* | [**issues/get**](Apis/IssuesApi.http#issues/get) | **GET** /repos/{owner}/{repo}/issues/{issue_number} | Get an issue
*IssuesApi* | [**issues/getComment**](Apis/IssuesApi.http#issues/getcomment) | **GET** /repos/{owner}/{repo}/issues/comments/{comment_id} | Get an issue comment
*IssuesApi* | [**issues/getEvent**](Apis/IssuesApi.http#issues/getevent) | **GET** /repos/{owner}/{repo}/issues/events/{event_id} | Get an issue event
*IssuesApi* | [**issues/getLabel**](Apis/IssuesApi.http#issues/getlabel) | **GET** /repos/{owner}/{repo}/labels/{name} | Get a label
*IssuesApi* | [**issues/getMilestone**](Apis/IssuesApi.http#issues/getmilestone) | **GET** /repos/{owner}/{repo}/milestones/{milestone_number} | Get a milestone
*IssuesApi* | [**issues/list**](Apis/IssuesApi.http#issues/list) | **GET** /issues | List issues assigned to the authenticated user
*IssuesApi* | [**issues/listAssignees**](Apis/IssuesApi.http#issues/listassignees) | **GET** /repos/{owner}/{repo}/assignees | List assignees
*IssuesApi* | [**issues/listComments**](Apis/IssuesApi.http#issues/listcomments) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/comments | List issue comments
*IssuesApi* | [**issues/listCommentsForRepo**](Apis/IssuesApi.http#issues/listcommentsforrepo) | **GET** /repos/{owner}/{repo}/issues/comments | List issue comments for a repository
*IssuesApi* | [**issues/listEvents**](Apis/IssuesApi.http#issues/listevents) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/events | List issue events
*IssuesApi* | [**issues/listEventsForRepo**](Apis/IssuesApi.http#issues/listeventsforrepo) | **GET** /repos/{owner}/{repo}/issues/events | List issue events for a repository
*IssuesApi* | [**issues/listEventsForTimeline**](Apis/IssuesApi.http#issues/listeventsfortimeline) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/timeline | List timeline events for an issue
*IssuesApi* | [**issues/listForAuthenticatedUser**](Apis/IssuesApi.http#issues/listforauthenticateduser) | **GET** /user/issues | List user account issues assigned to the authenticated user
*IssuesApi* | [**issues/listForOrg**](Apis/IssuesApi.http#issues/listfororg) | **GET** /orgs/{org}/issues | List organization issues assigned to the authenticated user
*IssuesApi* | [**issues/listForRepo**](Apis/IssuesApi.http#issues/listforrepo) | **GET** /repos/{owner}/{repo}/issues | List repository issues
*IssuesApi* | [**issues/listLabelsForMilestone**](Apis/IssuesApi.http#issues/listlabelsformilestone) | **GET** /repos/{owner}/{repo}/milestones/{milestone_number}/labels | List labels for issues in a milestone
*IssuesApi* | [**issues/listLabelsForRepo**](Apis/IssuesApi.http#issues/listlabelsforrepo) | **GET** /repos/{owner}/{repo}/labels | List labels for a repository
*IssuesApi* | [**issues/listLabelsOnIssue**](Apis/IssuesApi.http#issues/listlabelsonissue) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/labels | List labels for an issue
*IssuesApi* | [**issues/listMilestones**](Apis/IssuesApi.http#issues/listmilestones) | **GET** /repos/{owner}/{repo}/milestones | List milestones
*IssuesApi* | [**issues/lock**](Apis/IssuesApi.http#issues/lock) | **PUT** /repos/{owner}/{repo}/issues/{issue_number}/lock | Lock an issue
*IssuesApi* | [**issues/removeAllLabels**](Apis/IssuesApi.http#issues/removealllabels) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/labels | Remove all labels from an issue
*IssuesApi* | [**issues/removeAssignees**](Apis/IssuesApi.http#issues/removeassignees) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/assignees | Remove assignees from an issue
*IssuesApi* | [**issues/removeLabel**](Apis/IssuesApi.http#issues/removelabel) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/labels/{name} | Remove a label from an issue
*IssuesApi* | [**issues/setLabels**](Apis/IssuesApi.http#issues/setlabels) | **PUT** /repos/{owner}/{repo}/issues/{issue_number}/labels | Set labels for an issue
*IssuesApi* | [**issues/unlock**](Apis/IssuesApi.http#issues/unlock) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/lock | Unlock an issue
*IssuesApi* | [**issues/update**](Apis/IssuesApi.http#issues/update) | **PATCH** /repos/{owner}/{repo}/issues/{issue_number} | Update an issue
*IssuesApi* | [**issues/updateComment**](Apis/IssuesApi.http#issues/updatecomment) | **PATCH** /repos/{owner}/{repo}/issues/comments/{comment_id} | Update an issue comment
*IssuesApi* | [**issues/updateLabel**](Apis/IssuesApi.http#issues/updatelabel) | **PATCH** /repos/{owner}/{repo}/labels/{name} | Update a label
*IssuesApi* | [**issues/updateMilestone**](Apis/IssuesApi.http#issues/updatemilestone) | **PATCH** /repos/{owner}/{repo}/milestones/{milestone_number} | Update a milestone
*LicensesApi* | [**licenses/get**](Apis/LicensesApi.http#licenses/get) | **GET** /licenses/{license} | Get a license
*LicensesApi* | [**licenses/getAllCommonlyUsed**](Apis/LicensesApi.http#licenses/getallcommonlyused) | **GET** /licenses | Get all commonly used licenses
*LicensesApi* | [**licenses/getForRepo**](Apis/LicensesApi.http#licenses/getforrepo) | **GET** /repos/{owner}/{repo}/license | Get the license for a repository
*MarkdownApi* | [**markdown/render**](Apis/MarkdownApi.http#markdown/render) | **POST** /markdown | Render a Markdown document
*MarkdownApi* | [**markdown/renderRaw**](Apis/MarkdownApi.http#markdown/renderraw) | **POST** /markdown/raw | Render a Markdown document in raw mode
*MetaApi* | [**meta/get**](Apis/MetaApi.http#meta/get) | **GET** /meta | Get GitHub meta information
*MetaApi* | [**meta/getAllVersions**](Apis/MetaApi.http#meta/getallversions) | **GET** /versions | Get all API versions
*MetaApi* | [**meta/getOctocat**](Apis/MetaApi.http#meta/getoctocat) | **GET** /octocat | Get Octocat
*MetaApi* | [**meta/getZen**](Apis/MetaApi.http#meta/getzen) | **GET** /zen | Get the Zen of GitHub
*MetaApi* | [**meta/root**](Apis/MetaApi.http#meta/root) | **GET** / | GitHub API Root
*MigrationsApi* | [**migrations/cancelImport**](Apis/MigrationsApi.http#migrations/cancelimport) | **DELETE** /repos/{owner}/{repo}/import | Cancel an import
*MigrationsApi* | [**migrations/deleteArchiveForAuthenticatedUser**](Apis/MigrationsApi.http#migrations/deletearchiveforauthenticateduser) | **DELETE** /user/migrations/{migration_id}/archive | Delete a user migration archive
*MigrationsApi* | [**migrations/deleteArchiveForOrg**](Apis/MigrationsApi.http#migrations/deletearchivefororg) | **DELETE** /orgs/{org}/migrations/{migration_id}/archive | Delete an organization migration archive
*MigrationsApi* | [**migrations/downloadArchiveForOrg**](Apis/MigrationsApi.http#migrations/downloadarchivefororg) | **GET** /orgs/{org}/migrations/{migration_id}/archive | Download an organization migration archive
*MigrationsApi* | [**migrations/getArchiveForAuthenticatedUser**](Apis/MigrationsApi.http#migrations/getarchiveforauthenticateduser) | **GET** /user/migrations/{migration_id}/archive | Download a user migration archive
*MigrationsApi* | [**migrations/getCommitAuthors**](Apis/MigrationsApi.http#migrations/getcommitauthors) | **GET** /repos/{owner}/{repo}/import/authors | Get commit authors
*MigrationsApi* | [**migrations/getImportStatus**](Apis/MigrationsApi.http#migrations/getimportstatus) | **GET** /repos/{owner}/{repo}/import | Get an import status
*MigrationsApi* | [**migrations/getLargeFiles**](Apis/MigrationsApi.http#migrations/getlargefiles) | **GET** /repos/{owner}/{repo}/import/large_files | Get large files
*MigrationsApi* | [**migrations/getStatusForAuthenticatedUser**](Apis/MigrationsApi.http#migrations/getstatusforauthenticateduser) | **GET** /user/migrations/{migration_id} | Get a user migration status
*MigrationsApi* | [**migrations/getStatusForOrg**](Apis/MigrationsApi.http#migrations/getstatusfororg) | **GET** /orgs/{org}/migrations/{migration_id} | Get an organization migration status
*MigrationsApi* | [**migrations/listForAuthenticatedUser**](Apis/MigrationsApi.http#migrations/listforauthenticateduser) | **GET** /user/migrations | List user migrations
*MigrationsApi* | [**migrations/listForOrg**](Apis/MigrationsApi.http#migrations/listfororg) | **GET** /orgs/{org}/migrations | List organization migrations
*MigrationsApi* | [**migrations/listReposForAuthenticatedUser**](Apis/MigrationsApi.http#migrations/listreposforauthenticateduser) | **GET** /user/migrations/{migration_id}/repositories | List repositories for a user migration
*MigrationsApi* | [**migrations/listReposForOrg**](Apis/MigrationsApi.http#migrations/listreposfororg) | **GET** /orgs/{org}/migrations/{migration_id}/repositories | List repositories in an organization migration
*MigrationsApi* | [**migrations/mapCommitAuthor**](Apis/MigrationsApi.http#migrations/mapcommitauthor) | **PATCH** /repos/{owner}/{repo}/import/authors/{author_id} | Map a commit author
*MigrationsApi* | [**migrations/setLfsPreference**](Apis/MigrationsApi.http#migrations/setlfspreference) | **PATCH** /repos/{owner}/{repo}/import/lfs | Update Git LFS preference
*MigrationsApi* | [**migrations/startForAuthenticatedUser**](Apis/MigrationsApi.http#migrations/startforauthenticateduser) | **POST** /user/migrations | Start a user migration
*MigrationsApi* | [**migrations/startForOrg**](Apis/MigrationsApi.http#migrations/startfororg) | **POST** /orgs/{org}/migrations | Start an organization migration
*MigrationsApi* | [**migrations/startImport**](Apis/MigrationsApi.http#migrations/startimport) | **PUT** /repos/{owner}/{repo}/import | Start an import
*MigrationsApi* | [**migrations/unlockRepoForAuthenticatedUser**](Apis/MigrationsApi.http#migrations/unlockrepoforauthenticateduser) | **DELETE** /user/migrations/{migration_id}/repos/{repo_name}/lock | Unlock a user repository
*MigrationsApi* | [**migrations/unlockRepoForOrg**](Apis/MigrationsApi.http#migrations/unlockrepofororg) | **DELETE** /orgs/{org}/migrations/{migration_id}/repos/{repo_name}/lock | Unlock an organization repository
*MigrationsApi* | [**migrations/updateImport**](Apis/MigrationsApi.http#migrations/updateimport) | **PATCH** /repos/{owner}/{repo}/import | Update an import
*OidcApi* | [**oidc/getOidcCustomSubTemplateForOrg**](Apis/OidcApi.http#oidc/getoidccustomsubtemplatefororg) | **GET** /orgs/{org}/actions/oidc/customization/sub | Get the customization template for an OIDC subject claim for an organization
*OidcApi* | [**oidc/updateOidcCustomSubTemplateForOrg**](Apis/OidcApi.http#oidc/updateoidccustomsubtemplatefororg) | **PUT** /orgs/{org}/actions/oidc/customization/sub | Set the customization template for an OIDC subject claim for an organization
*OrgsApi* | [**orgs/addSecurityManagerTeam**](Apis/OrgsApi.http#orgs/addsecuritymanagerteam) | **PUT** /orgs/{org}/security-managers/teams/{team_slug} | Add a security manager team
*OrgsApi* | [**orgs/assignTeamToOrgRole**](Apis/OrgsApi.http#orgs/assignteamtoorgrole) | **PUT** /orgs/{org}/organization-roles/teams/{team_slug}/{role_id} | Assign an organization role to a team
*OrgsApi* | [**orgs/assignUserToOrgRole**](Apis/OrgsApi.http#orgs/assignusertoorgrole) | **PUT** /orgs/{org}/organization-roles/users/{username}/{role_id} | Assign an organization role to a user
*OrgsApi* | [**orgs/blockUser**](Apis/OrgsApi.http#orgs/blockuser) | **PUT** /orgs/{org}/blocks/{username} | Block a user from an organization
*OrgsApi* | [**orgs/cancelInvitation**](Apis/OrgsApi.http#orgs/cancelinvitation) | **DELETE** /orgs/{org}/invitations/{invitation_id} | Cancel an organization invitation
*OrgsApi* | [**orgs/checkBlockedUser**](Apis/OrgsApi.http#orgs/checkblockeduser) | **GET** /orgs/{org}/blocks/{username} | Check if a user is blocked by an organization
*OrgsApi* | [**orgs/checkMembershipForUser**](Apis/OrgsApi.http#orgs/checkmembershipforuser) | **GET** /orgs/{org}/members/{username} | Check organization membership for a user
*OrgsApi* | [**orgs/checkPublicMembershipForUser**](Apis/OrgsApi.http#orgs/checkpublicmembershipforuser) | **GET** /orgs/{org}/public_members/{username} | Check public organization membership for a user
*OrgsApi* | [**orgs/convertMemberToOutsideCollaborator**](Apis/OrgsApi.http#orgs/convertmembertooutsidecollaborator) | **PUT** /orgs/{org}/outside_collaborators/{username} | Convert an organization member to outside collaborator
*OrgsApi* | [**orgs/createCustomOrganizationRole**](Apis/OrgsApi.http#orgs/createcustomorganizationrole) | **POST** /orgs/{org}/organization-roles | Create a custom organization role
*OrgsApi* | [**orgs/createInvitation**](Apis/OrgsApi.http#orgs/createinvitation) | **POST** /orgs/{org}/invitations | Create an organization invitation
*OrgsApi* | [**orgs/createOrUpdateCustomProperties**](Apis/OrgsApi.http#orgs/createorupdatecustomproperties) | **PATCH** /orgs/{org}/properties/schema | Create or update custom properties for an organization
*OrgsApi* | [**orgs/createOrUpdateCustomPropertiesValuesForRepos**](Apis/OrgsApi.http#orgs/createorupdatecustompropertiesvaluesforrepos) | **PATCH** /orgs/{org}/properties/values | Create or update custom property values for organization repositories
*OrgsApi* | [**orgs/createOrUpdateCustomProperty**](Apis/OrgsApi.http#orgs/createorupdatecustomproperty) | **PUT** /orgs/{org}/properties/schema/{custom_property_name} | Create or update a custom property for an organization
*OrgsApi* | [**orgs/createWebhook**](Apis/OrgsApi.http#orgs/createwebhook) | **POST** /orgs/{org}/hooks | Create an organization webhook
*OrgsApi* | [**orgs/delete**](Apis/OrgsApi.http#orgs/delete) | **DELETE** /orgs/{org} | Delete an organization
*OrgsApi* | [**orgs/deleteCustomOrganizationRole**](Apis/OrgsApi.http#orgs/deletecustomorganizationrole) | **DELETE** /orgs/{org}/organization-roles/{role_id} | Delete a custom organization role.
*OrgsApi* | [**orgs/deleteWebhook**](Apis/OrgsApi.http#orgs/deletewebhook) | **DELETE** /orgs/{org}/hooks/{hook_id} | Delete an organization webhook
*OrgsApi* | [**orgs/enableOrDisableSecurityProductOnAllOrgRepos**](Apis/OrgsApi.http#orgs/enableordisablesecurityproductonallorgrepos) | **POST** /orgs/{org}/{security_product}/{enablement} | Enable or disable a security feature for an organization
*OrgsApi* | [**orgs/get**](Apis/OrgsApi.http#orgs/get) | **GET** /orgs/{org} | Get an organization
*OrgsApi* | [**orgs/getAllCustomProperties**](Apis/OrgsApi.http#orgs/getallcustomproperties) | **GET** /orgs/{org}/properties/schema | Get all custom properties for an organization
*OrgsApi* | [**orgs/getCustomProperty**](Apis/OrgsApi.http#orgs/getcustomproperty) | **GET** /orgs/{org}/properties/schema/{custom_property_name} | Get a custom property for an organization
*OrgsApi* | [**orgs/getMembershipForAuthenticatedUser**](Apis/OrgsApi.http#orgs/getmembershipforauthenticateduser) | **GET** /user/memberships/orgs/{org} | Get an organization membership for the authenticated user
*OrgsApi* | [**orgs/getMembershipForUser**](Apis/OrgsApi.http#orgs/getmembershipforuser) | **GET** /orgs/{org}/memberships/{username} | Get organization membership for a user
*OrgsApi* | [**orgs/getOrgRole**](Apis/OrgsApi.http#orgs/getorgrole) | **GET** /orgs/{org}/organization-roles/{role_id} | Get an organization role
*OrgsApi* | [**orgs/getWebhook**](Apis/OrgsApi.http#orgs/getwebhook) | **GET** /orgs/{org}/hooks/{hook_id} | Get an organization webhook
*OrgsApi* | [**orgs/getWebhookConfigForOrg**](Apis/OrgsApi.http#orgs/getwebhookconfigfororg) | **GET** /orgs/{org}/hooks/{hook_id}/config | Get a webhook configuration for an organization
*OrgsApi* | [**orgs/getWebhookDelivery**](Apis/OrgsApi.http#orgs/getwebhookdelivery) | **GET** /orgs/{org}/hooks/{hook_id}/deliveries/{delivery_id} | Get a webhook delivery for an organization webhook
*OrgsApi* | [**orgs/list**](Apis/OrgsApi.http#orgs/list) | **GET** /organizations | List organizations
*OrgsApi* | [**orgs/listAppInstallations**](Apis/OrgsApi.http#orgs/listappinstallations) | **GET** /orgs/{org}/installations | List app installations for an organization
*OrgsApi* | [**orgs/listBlockedUsers**](Apis/OrgsApi.http#orgs/listblockedusers) | **GET** /orgs/{org}/blocks | List users blocked by an organization
*OrgsApi* | [**orgs/listCustomPropertiesValuesForRepos**](Apis/OrgsApi.http#orgs/listcustompropertiesvaluesforrepos) | **GET** /orgs/{org}/properties/values | List custom property values for organization repositories
*OrgsApi* | [**orgs/listFailedInvitations**](Apis/OrgsApi.http#orgs/listfailedinvitations) | **GET** /orgs/{org}/failed_invitations | List failed organization invitations
*OrgsApi* | [**orgs/listForAuthenticatedUser**](Apis/OrgsApi.http#orgs/listforauthenticateduser) | **GET** /user/orgs | List organizations for the authenticated user
*OrgsApi* | [**orgs/listForUser**](Apis/OrgsApi.http#orgs/listforuser) | **GET** /users/{username}/orgs | List organizations for a user
*OrgsApi* | [**orgs/listInvitationTeams**](Apis/OrgsApi.http#orgs/listinvitationteams) | **GET** /orgs/{org}/invitations/{invitation_id}/teams | List organization invitation teams
*OrgsApi* | [**orgs/listMembers**](Apis/OrgsApi.http#orgs/listmembers) | **GET** /orgs/{org}/members | List organization members
*OrgsApi* | [**orgs/listMembershipsForAuthenticatedUser**](Apis/OrgsApi.http#orgs/listmembershipsforauthenticateduser) | **GET** /user/memberships/orgs | List organization memberships for the authenticated user
*OrgsApi* | [**orgs/listOrgRoleTeams**](Apis/OrgsApi.http#orgs/listorgroleteams) | **GET** /orgs/{org}/organization-roles/{role_id}/teams | List teams that are assigned to an organization role
*OrgsApi* | [**orgs/listOrgRoleUsers**](Apis/OrgsApi.http#orgs/listorgroleusers) | **GET** /orgs/{org}/organization-roles/{role_id}/users | List users that are assigned to an organization role
*OrgsApi* | [**orgs/listOrgRoles**](Apis/OrgsApi.http#orgs/listorgroles) | **GET** /orgs/{org}/organization-roles | Get all organization roles for an organization
*OrgsApi* | [**orgs/listOrganizationFineGrainedPermissions**](Apis/OrgsApi.http#orgs/listorganizationfinegrainedpermissions) | **GET** /orgs/{org}/organization-fine-grained-permissions | List organization fine-grained permissions for an organization
*OrgsApi* | [**orgs/listOutsideCollaborators**](Apis/OrgsApi.http#orgs/listoutsidecollaborators) | **GET** /orgs/{org}/outside_collaborators | List outside collaborators for an organization
*OrgsApi* | [**orgs/listPatGrantRepositories**](Apis/OrgsApi.http#orgs/listpatgrantrepositories) | **GET** /orgs/{org}/personal-access-tokens/{pat_id}/repositories | List repositories a fine-grained personal access token has access to
*OrgsApi* | [**orgs/listPatGrantRequestRepositories**](Apis/OrgsApi.http#orgs/listpatgrantrequestrepositories) | **GET** /orgs/{org}/personal-access-token-requests/{pat_request_id}/repositories | List repositories requested to be accessed by a fine-grained personal access token
*OrgsApi* | [**orgs/listPatGrantRequests**](Apis/OrgsApi.http#orgs/listpatgrantrequests) | **GET** /orgs/{org}/personal-access-token-requests | List requests to access organization resources with fine-grained personal access tokens
*OrgsApi* | [**orgs/listPatGrants**](Apis/OrgsApi.http#orgs/listpatgrants) | **GET** /orgs/{org}/personal-access-tokens | List fine-grained personal access tokens with access to organization resources
*OrgsApi* | [**orgs/listPendingInvitations**](Apis/OrgsApi.http#orgs/listpendinginvitations) | **GET** /orgs/{org}/invitations | List pending organization invitations
*OrgsApi* | [**orgs/listPublicMembers**](Apis/OrgsApi.http#orgs/listpublicmembers) | **GET** /orgs/{org}/public_members | List public organization members
*OrgsApi* | [**orgs/listSecurityManagerTeams**](Apis/OrgsApi.http#orgs/listsecuritymanagerteams) | **GET** /orgs/{org}/security-managers | List security manager teams
*OrgsApi* | [**orgs/listWebhookDeliveries**](Apis/OrgsApi.http#orgs/listwebhookdeliveries) | **GET** /orgs/{org}/hooks/{hook_id}/deliveries | List deliveries for an organization webhook
*OrgsApi* | [**orgs/listWebhooks**](Apis/OrgsApi.http#orgs/listwebhooks) | **GET** /orgs/{org}/hooks | List organization webhooks
*OrgsApi* | [**orgs/patchCustomOrganizationRole**](Apis/OrgsApi.http#orgs/patchcustomorganizationrole) | **PATCH** /orgs/{org}/organization-roles/{role_id} | Update a custom organization role
*OrgsApi* | [**orgs/pingWebhook**](Apis/OrgsApi.http#orgs/pingwebhook) | **POST** /orgs/{org}/hooks/{hook_id}/pings | Ping an organization webhook
*OrgsApi* | [**orgs/redeliverWebhookDelivery**](Apis/OrgsApi.http#orgs/redeliverwebhookdelivery) | **POST** /orgs/{org}/hooks/{hook_id}/deliveries/{delivery_id}/attempts | Redeliver a delivery for an organization webhook
*OrgsApi* | [**orgs/removeCustomProperty**](Apis/OrgsApi.http#orgs/removecustomproperty) | **DELETE** /orgs/{org}/properties/schema/{custom_property_name} | Remove a custom property for an organization
*OrgsApi* | [**orgs/removeMember**](Apis/OrgsApi.http#orgs/removemember) | **DELETE** /orgs/{org}/members/{username} | Remove an organization member
*OrgsApi* | [**orgs/removeMembershipForUser**](Apis/OrgsApi.http#orgs/removemembershipforuser) | **DELETE** /orgs/{org}/memberships/{username} | Remove organization membership for a user
*OrgsApi* | [**orgs/removeOutsideCollaborator**](Apis/OrgsApi.http#orgs/removeoutsidecollaborator) | **DELETE** /orgs/{org}/outside_collaborators/{username} | Remove outside collaborator from an organization
*OrgsApi* | [**orgs/removePublicMembershipForAuthenticatedUser**](Apis/OrgsApi.http#orgs/removepublicmembershipforauthenticateduser) | **DELETE** /orgs/{org}/public_members/{username} | Remove public organization membership for the authenticated user
*OrgsApi* | [**orgs/removeSecurityManagerTeam**](Apis/OrgsApi.http#orgs/removesecuritymanagerteam) | **DELETE** /orgs/{org}/security-managers/teams/{team_slug} | Remove a security manager team
*OrgsApi* | [**orgs/reviewPatGrantRequest**](Apis/OrgsApi.http#orgs/reviewpatgrantrequest) | **POST** /orgs/{org}/personal-access-token-requests/{pat_request_id} | Review a request to access organization resources with a fine-grained personal access token
*OrgsApi* | [**orgs/reviewPatGrantRequestsInBulk**](Apis/OrgsApi.http#orgs/reviewpatgrantrequestsinbulk) | **POST** /orgs/{org}/personal-access-token-requests | Review requests to access organization resources with fine-grained personal access tokens
*OrgsApi* | [**orgs/revokeAllOrgRolesTeam**](Apis/OrgsApi.http#orgs/revokeallorgrolesteam) | **DELETE** /orgs/{org}/organization-roles/teams/{team_slug} | Remove all organization roles for a team
*OrgsApi* | [**orgs/revokeAllOrgRolesUser**](Apis/OrgsApi.http#orgs/revokeallorgrolesuser) | **DELETE** /orgs/{org}/organization-roles/users/{username} | Remove all organization roles for a user
*OrgsApi* | [**orgs/revokeOrgRoleTeam**](Apis/OrgsApi.http#orgs/revokeorgroleteam) | **DELETE** /orgs/{org}/organization-roles/teams/{team_slug}/{role_id} | Remove an organization role from a team
*OrgsApi* | [**orgs/revokeOrgRoleUser**](Apis/OrgsApi.http#orgs/revokeorgroleuser) | **DELETE** /orgs/{org}/organization-roles/users/{username}/{role_id} | Remove an organization role from a user
*OrgsApi* | [**orgs/setMembershipForUser**](Apis/OrgsApi.http#orgs/setmembershipforuser) | **PUT** /orgs/{org}/memberships/{username} | Set organization membership for a user
*OrgsApi* | [**orgs/setPublicMembershipForAuthenticatedUser**](Apis/OrgsApi.http#orgs/setpublicmembershipforauthenticateduser) | **PUT** /orgs/{org}/public_members/{username} | Set public organization membership for the authenticated user
*OrgsApi* | [**orgs/unblockUser**](Apis/OrgsApi.http#orgs/unblockuser) | **DELETE** /orgs/{org}/blocks/{username} | Unblock a user from an organization
*OrgsApi* | [**orgs/update**](Apis/OrgsApi.http#orgs/update) | **PATCH** /orgs/{org} | Update an organization
*OrgsApi* | [**orgs/updateMembershipForAuthenticatedUser**](Apis/OrgsApi.http#orgs/updatemembershipforauthenticateduser) | **PATCH** /user/memberships/orgs/{org} | Update an organization membership for the authenticated user
*OrgsApi* | [**orgs/updatePatAccess**](Apis/OrgsApi.http#orgs/updatepataccess) | **POST** /orgs/{org}/personal-access-tokens/{pat_id} | Update the access a fine-grained personal access token has to organization resources
*OrgsApi* | [**orgs/updatePatAccesses**](Apis/OrgsApi.http#orgs/updatepataccesses) | **POST** /orgs/{org}/personal-access-tokens | Update the access to organization resources via fine-grained personal access tokens
*OrgsApi* | [**orgs/updateWebhook**](Apis/OrgsApi.http#orgs/updatewebhook) | **PATCH** /orgs/{org}/hooks/{hook_id} | Update an organization webhook
*OrgsApi* | [**orgs/updateWebhookConfigForOrg**](Apis/OrgsApi.http#orgs/updatewebhookconfigfororg) | **PATCH** /orgs/{org}/hooks/{hook_id}/config | Update a webhook configuration for an organization
*PackagesApi* | [**packages/deletePackageForAuthenticatedUser**](Apis/PackagesApi.http#packages/deletepackageforauthenticateduser) | **DELETE** /user/packages/{package_type}/{package_name} | Delete a package for the authenticated user
*PackagesApi* | [**packages/deletePackageForOrg**](Apis/PackagesApi.http#packages/deletepackagefororg) | **DELETE** /orgs/{org}/packages/{package_type}/{package_name} | Delete a package for an organization
*PackagesApi* | [**packages/deletePackageForUser**](Apis/PackagesApi.http#packages/deletepackageforuser) | **DELETE** /users/{username}/packages/{package_type}/{package_name} | Delete a package for a user
*PackagesApi* | [**packages/deletePackageVersionForAuthenticatedUser**](Apis/PackagesApi.http#packages/deletepackageversionforauthenticateduser) | **DELETE** /user/packages/{package_type}/{package_name}/versions/{package_version_id} | Delete a package version for the authenticated user
*PackagesApi* | [**packages/deletePackageVersionForOrg**](Apis/PackagesApi.http#packages/deletepackageversionfororg) | **DELETE** /orgs/{org}/packages/{package_type}/{package_name}/versions/{package_version_id} | Delete package version for an organization
*PackagesApi* | [**packages/deletePackageVersionForUser**](Apis/PackagesApi.http#packages/deletepackageversionforuser) | **DELETE** /users/{username}/packages/{package_type}/{package_name}/versions/{package_version_id} | Delete package version for a user
*PackagesApi* | [**packages/getAllPackageVersionsForPackageOwnedByAuthenticatedUser**](Apis/PackagesApi.http#packages/getallpackageversionsforpackageownedbyauthenticateduser) | **GET** /user/packages/{package_type}/{package_name}/versions | List package versions for a package owned by the authenticated user
*PackagesApi* | [**packages/getAllPackageVersionsForPackageOwnedByOrg**](Apis/PackagesApi.http#packages/getallpackageversionsforpackageownedbyorg) | **GET** /orgs/{org}/packages/{package_type}/{package_name}/versions | List package versions for a package owned by an organization
*PackagesApi* | [**packages/getAllPackageVersionsForPackageOwnedByUser**](Apis/PackagesApi.http#packages/getallpackageversionsforpackageownedbyuser) | **GET** /users/{username}/packages/{package_type}/{package_name}/versions | List package versions for a package owned by a user
*PackagesApi* | [**packages/getPackageForAuthenticatedUser**](Apis/PackagesApi.http#packages/getpackageforauthenticateduser) | **GET** /user/packages/{package_type}/{package_name} | Get a package for the authenticated user
*PackagesApi* | [**packages/getPackageForOrganization**](Apis/PackagesApi.http#packages/getpackagefororganization) | **GET** /orgs/{org}/packages/{package_type}/{package_name} | Get a package for an organization
*PackagesApi* | [**packages/getPackageForUser**](Apis/PackagesApi.http#packages/getpackageforuser) | **GET** /users/{username}/packages/{package_type}/{package_name} | Get a package for a user
*PackagesApi* | [**packages/getPackageVersionForAuthenticatedUser**](Apis/PackagesApi.http#packages/getpackageversionforauthenticateduser) | **GET** /user/packages/{package_type}/{package_name}/versions/{package_version_id} | Get a package version for the authenticated user
*PackagesApi* | [**packages/getPackageVersionForOrganization**](Apis/PackagesApi.http#packages/getpackageversionfororganization) | **GET** /orgs/{org}/packages/{package_type}/{package_name}/versions/{package_version_id} | Get a package version for an organization
*PackagesApi* | [**packages/getPackageVersionForUser**](Apis/PackagesApi.http#packages/getpackageversionforuser) | **GET** /users/{username}/packages/{package_type}/{package_name}/versions/{package_version_id} | Get a package version for a user
*PackagesApi* | [**packages/listDockerMigrationConflictingPackagesForAuthenticatedUser**](Apis/PackagesApi.http#packages/listdockermigrationconflictingpackagesforauthenticateduser) | **GET** /user/docker/conflicts | Get list of conflicting packages during Docker migration for authenticated-user
*PackagesApi* | [**packages/listDockerMigrationConflictingPackagesForOrganization**](Apis/PackagesApi.http#packages/listdockermigrationconflictingpackagesfororganization) | **GET** /orgs/{org}/docker/conflicts | Get list of conflicting packages during Docker migration for organization
*PackagesApi* | [**packages/listDockerMigrationConflictingPackagesForUser**](Apis/PackagesApi.http#packages/listdockermigrationconflictingpackagesforuser) | **GET** /users/{username}/docker/conflicts | Get list of conflicting packages during Docker migration for user
*PackagesApi* | [**packages/listPackagesForAuthenticatedUser**](Apis/PackagesApi.http#packages/listpackagesforauthenticateduser) | **GET** /user/packages | List packages for the authenticated user's namespace
*PackagesApi* | [**packages/listPackagesForOrganization**](Apis/PackagesApi.http#packages/listpackagesfororganization) | **GET** /orgs/{org}/packages | List packages for an organization
*PackagesApi* | [**packages/listPackagesForUser**](Apis/PackagesApi.http#packages/listpackagesforuser) | **GET** /users/{username}/packages | List packages for a user
*PackagesApi* | [**packages/restorePackageForAuthenticatedUser**](Apis/PackagesApi.http#packages/restorepackageforauthenticateduser) | **POST** /user/packages/{package_type}/{package_name}/restore | Restore a package for the authenticated user
*PackagesApi* | [**packages/restorePackageForOrg**](Apis/PackagesApi.http#packages/restorepackagefororg) | **POST** /orgs/{org}/packages/{package_type}/{package_name}/restore | Restore a package for an organization
*PackagesApi* | [**packages/restorePackageForUser**](Apis/PackagesApi.http#packages/restorepackageforuser) | **POST** /users/{username}/packages/{package_type}/{package_name}/restore | Restore a package for a user
*PackagesApi* | [**packages/restorePackageVersionForAuthenticatedUser**](Apis/PackagesApi.http#packages/restorepackageversionforauthenticateduser) | **POST** /user/packages/{package_type}/{package_name}/versions/{package_version_id}/restore | Restore a package version for the authenticated user
*PackagesApi* | [**packages/restorePackageVersionForOrg**](Apis/PackagesApi.http#packages/restorepackageversionfororg) | **POST** /orgs/{org}/packages/{package_type}/{package_name}/versions/{package_version_id}/restore | Restore package version for an organization
*PackagesApi* | [**packages/restorePackageVersionForUser**](Apis/PackagesApi.http#packages/restorepackageversionforuser) | **POST** /users/{username}/packages/{package_type}/{package_name}/versions/{package_version_id}/restore | Restore package version for a user
*ProjectsApi* | [**projects/addCollaborator**](Apis/ProjectsApi.http#projects/addcollaborator) | **PUT** /projects/{project_id}/collaborators/{username} | Add project collaborator
*ProjectsApi* | [**projects/createCard**](Apis/ProjectsApi.http#projects/createcard) | **POST** /projects/columns/{column_id}/cards | Create a project card
*ProjectsApi* | [**projects/createColumn**](Apis/ProjectsApi.http#projects/createcolumn) | **POST** /projects/{project_id}/columns | Create a project column
*ProjectsApi* | [**projects/createForAuthenticatedUser**](Apis/ProjectsApi.http#projects/createforauthenticateduser) | **POST** /user/projects | Create a user project
*ProjectsApi* | [**projects/createForOrg**](Apis/ProjectsApi.http#projects/createfororg) | **POST** /orgs/{org}/projects | Create an organization project
*ProjectsApi* | [**projects/createForRepo**](Apis/ProjectsApi.http#projects/createforrepo) | **POST** /repos/{owner}/{repo}/projects | Create a repository project
*ProjectsApi* | [**projects/delete**](Apis/ProjectsApi.http#projects/delete) | **DELETE** /projects/{project_id} | Delete a project
*ProjectsApi* | [**projects/deleteCard**](Apis/ProjectsApi.http#projects/deletecard) | **DELETE** /projects/columns/cards/{card_id} | Delete a project card
*ProjectsApi* | [**projects/deleteColumn**](Apis/ProjectsApi.http#projects/deletecolumn) | **DELETE** /projects/columns/{column_id} | Delete a project column
*ProjectsApi* | [**projects/get**](Apis/ProjectsApi.http#projects/get) | **GET** /projects/{project_id} | Get a project
*ProjectsApi* | [**projects/getCard**](Apis/ProjectsApi.http#projects/getcard) | **GET** /projects/columns/cards/{card_id} | Get a project card
*ProjectsApi* | [**projects/getColumn**](Apis/ProjectsApi.http#projects/getcolumn) | **GET** /projects/columns/{column_id} | Get a project column
*ProjectsApi* | [**projects/getPermissionForUser**](Apis/ProjectsApi.http#projects/getpermissionforuser) | **GET** /projects/{project_id}/collaborators/{username}/permission | Get project permission for a user
*ProjectsApi* | [**projects/listCards**](Apis/ProjectsApi.http#projects/listcards) | **GET** /projects/columns/{column_id}/cards | List project cards
*ProjectsApi* | [**projects/listCollaborators**](Apis/ProjectsApi.http#projects/listcollaborators) | **GET** /projects/{project_id}/collaborators | List project collaborators
*ProjectsApi* | [**projects/listColumns**](Apis/ProjectsApi.http#projects/listcolumns) | **GET** /projects/{project_id}/columns | List project columns
*ProjectsApi* | [**projects/listForOrg**](Apis/ProjectsApi.http#projects/listfororg) | **GET** /orgs/{org}/projects | List organization projects
*ProjectsApi* | [**projects/listForRepo**](Apis/ProjectsApi.http#projects/listforrepo) | **GET** /repos/{owner}/{repo}/projects | List repository projects
*ProjectsApi* | [**projects/listForUser**](Apis/ProjectsApi.http#projects/listforuser) | **GET** /users/{username}/projects | List user projects
*ProjectsApi* | [**projects/moveCard**](Apis/ProjectsApi.http#projects/movecard) | **POST** /projects/columns/cards/{card_id}/moves | Move a project card
*ProjectsApi* | [**projects/moveColumn**](Apis/ProjectsApi.http#projects/movecolumn) | **POST** /projects/columns/{column_id}/moves | Move a project column
*ProjectsApi* | [**projects/removeCollaborator**](Apis/ProjectsApi.http#projects/removecollaborator) | **DELETE** /projects/{project_id}/collaborators/{username} | Remove user as a collaborator
*ProjectsApi* | [**projects/update**](Apis/ProjectsApi.http#projects/update) | **PATCH** /projects/{project_id} | Update a project
*ProjectsApi* | [**projects/updateCard**](Apis/ProjectsApi.http#projects/updatecard) | **PATCH** /projects/columns/cards/{card_id} | Update an existing project card
*ProjectsApi* | [**projects/updateColumn**](Apis/ProjectsApi.http#projects/updatecolumn) | **PATCH** /projects/columns/{column_id} | Update an existing project column
*PullsApi* | [**pulls/checkIfMerged**](Apis/PullsApi.http#pulls/checkifmerged) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/merge | Check if a pull request has been merged
*PullsApi* | [**pulls/create**](Apis/PullsApi.http#pulls/create) | **POST** /repos/{owner}/{repo}/pulls | Create a pull request
*PullsApi* | [**pulls/createReplyForReviewComment**](Apis/PullsApi.http#pulls/createreplyforreviewcomment) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/comments/{comment_id}/replies | Create a reply for a review comment
*PullsApi* | [**pulls/createReview**](Apis/PullsApi.http#pulls/createreview) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/reviews | Create a review for a pull request
*PullsApi* | [**pulls/createReviewComment**](Apis/PullsApi.http#pulls/createreviewcomment) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/comments | Create a review comment for a pull request
*PullsApi* | [**pulls/deletePendingReview**](Apis/PullsApi.http#pulls/deletependingreview) | **DELETE** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id} | Delete a pending review for a pull request
*PullsApi* | [**pulls/deleteReviewComment**](Apis/PullsApi.http#pulls/deletereviewcomment) | **DELETE** /repos/{owner}/{repo}/pulls/comments/{comment_id} | Delete a review comment for a pull request
*PullsApi* | [**pulls/dismissReview**](Apis/PullsApi.http#pulls/dismissreview) | **PUT** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id}/dismissals | Dismiss a review for a pull request
*PullsApi* | [**pulls/get**](Apis/PullsApi.http#pulls/get) | **GET** /repos/{owner}/{repo}/pulls/{pull_number} | Get a pull request
*PullsApi* | [**pulls/getReview**](Apis/PullsApi.http#pulls/getreview) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id} | Get a review for a pull request
*PullsApi* | [**pulls/getReviewComment**](Apis/PullsApi.http#pulls/getreviewcomment) | **GET** /repos/{owner}/{repo}/pulls/comments/{comment_id} | Get a review comment for a pull request
*PullsApi* | [**pulls/list**](Apis/PullsApi.http#pulls/list) | **GET** /repos/{owner}/{repo}/pulls | List pull requests
*PullsApi* | [**pulls/listCommentsForReview**](Apis/PullsApi.http#pulls/listcommentsforreview) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id}/comments | List comments for a pull request review
*PullsApi* | [**pulls/listCommits**](Apis/PullsApi.http#pulls/listcommits) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/commits | List commits on a pull request
*PullsApi* | [**pulls/listFiles**](Apis/PullsApi.http#pulls/listfiles) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/files | List pull requests files
*PullsApi* | [**pulls/listRequestedReviewers**](Apis/PullsApi.http#pulls/listrequestedreviewers) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/requested_reviewers | Get all requested reviewers for a pull request
*PullsApi* | [**pulls/listReviewComments**](Apis/PullsApi.http#pulls/listreviewcomments) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/comments | List review comments on a pull request
*PullsApi* | [**pulls/listReviewCommentsForRepo**](Apis/PullsApi.http#pulls/listreviewcommentsforrepo) | **GET** /repos/{owner}/{repo}/pulls/comments | List review comments in a repository
*PullsApi* | [**pulls/listReviews**](Apis/PullsApi.http#pulls/listreviews) | **GET** /repos/{owner}/{repo}/pulls/{pull_number}/reviews | List reviews for a pull request
*PullsApi* | [**pulls/merge**](Apis/PullsApi.http#pulls/merge) | **PUT** /repos/{owner}/{repo}/pulls/{pull_number}/merge | Merge a pull request
*PullsApi* | [**pulls/removeRequestedReviewers**](Apis/PullsApi.http#pulls/removerequestedreviewers) | **DELETE** /repos/{owner}/{repo}/pulls/{pull_number}/requested_reviewers | Remove requested reviewers from a pull request
*PullsApi* | [**pulls/requestReviewers**](Apis/PullsApi.http#pulls/requestreviewers) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/requested_reviewers | Request reviewers for a pull request
*PullsApi* | [**pulls/submitReview**](Apis/PullsApi.http#pulls/submitreview) | **POST** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id}/events | Submit a review for a pull request
*PullsApi* | [**pulls/update**](Apis/PullsApi.http#pulls/update) | **PATCH** /repos/{owner}/{repo}/pulls/{pull_number} | Update a pull request
*PullsApi* | [**pulls/updateBranch**](Apis/PullsApi.http#pulls/updatebranch) | **PUT** /repos/{owner}/{repo}/pulls/{pull_number}/update-branch | Update a pull request branch
*PullsApi* | [**pulls/updateReview**](Apis/PullsApi.http#pulls/updatereview) | **PUT** /repos/{owner}/{repo}/pulls/{pull_number}/reviews/{review_id} | Update a review for a pull request
*PullsApi* | [**pulls/updateReviewComment**](Apis/PullsApi.http#pulls/updatereviewcomment) | **PATCH** /repos/{owner}/{repo}/pulls/comments/{comment_id} | Update a review comment for a pull request
*RateLimitApi* | [**rateLimit/get**](Apis/RateLimitApi.http#ratelimit/get) | **GET** /rate_limit | Get rate limit status for the authenticated user
*ReactionsApi* | [**reactions/createForCommitComment**](Apis/ReactionsApi.http#reactions/createforcommitcomment) | **POST** /repos/{owner}/{repo}/comments/{comment_id}/reactions | Create reaction for a commit comment
*ReactionsApi* | [**reactions/createForIssue**](Apis/ReactionsApi.http#reactions/createforissue) | **POST** /repos/{owner}/{repo}/issues/{issue_number}/reactions | Create reaction for an issue
*ReactionsApi* | [**reactions/createForIssueComment**](Apis/ReactionsApi.http#reactions/createforissuecomment) | **POST** /repos/{owner}/{repo}/issues/comments/{comment_id}/reactions | Create reaction for an issue comment
*ReactionsApi* | [**reactions/createForPullRequestReviewComment**](Apis/ReactionsApi.http#reactions/createforpullrequestreviewcomment) | **POST** /repos/{owner}/{repo}/pulls/comments/{comment_id}/reactions | Create reaction for a pull request review comment
*ReactionsApi* | [**reactions/createForRelease**](Apis/ReactionsApi.http#reactions/createforrelease) | **POST** /repos/{owner}/{repo}/releases/{release_id}/reactions | Create reaction for a release
*ReactionsApi* | [**reactions/createForTeamDiscussionCommentInOrg**](Apis/ReactionsApi.http#reactions/createforteamdiscussioncommentinorg) | **POST** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number}/reactions | Create reaction for a team discussion comment
*ReactionsApi* | [**reactions/createForTeamDiscussionCommentLegacy**](Apis/ReactionsApi.http#reactions/createforteamdiscussioncommentlegacy) | **POST** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number}/reactions | Create reaction for a team discussion comment (Legacy)
*ReactionsApi* | [**reactions/createForTeamDiscussionInOrg**](Apis/ReactionsApi.http#reactions/createforteamdiscussioninorg) | **POST** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/reactions | Create reaction for a team discussion
*ReactionsApi* | [**reactions/createForTeamDiscussionLegacy**](Apis/ReactionsApi.http#reactions/createforteamdiscussionlegacy) | **POST** /teams/{team_id}/discussions/{discussion_number}/reactions | Create reaction for a team discussion (Legacy)
*ReactionsApi* | [**reactions/deleteForCommitComment**](Apis/ReactionsApi.http#reactions/deleteforcommitcomment) | **DELETE** /repos/{owner}/{repo}/comments/{comment_id}/reactions/{reaction_id} | Delete a commit comment reaction
*ReactionsApi* | [**reactions/deleteForIssue**](Apis/ReactionsApi.http#reactions/deleteforissue) | **DELETE** /repos/{owner}/{repo}/issues/{issue_number}/reactions/{reaction_id} | Delete an issue reaction
*ReactionsApi* | [**reactions/deleteForIssueComment**](Apis/ReactionsApi.http#reactions/deleteforissuecomment) | **DELETE** /repos/{owner}/{repo}/issues/comments/{comment_id}/reactions/{reaction_id} | Delete an issue comment reaction
*ReactionsApi* | [**reactions/deleteForPullRequestComment**](Apis/ReactionsApi.http#reactions/deleteforpullrequestcomment) | **DELETE** /repos/{owner}/{repo}/pulls/comments/{comment_id}/reactions/{reaction_id} | Delete a pull request comment reaction
*ReactionsApi* | [**reactions/deleteForRelease**](Apis/ReactionsApi.http#reactions/deleteforrelease) | **DELETE** /repos/{owner}/{repo}/releases/{release_id}/reactions/{reaction_id} | Delete a release reaction
*ReactionsApi* | [**reactions/deleteForTeamDiscussion**](Apis/ReactionsApi.http#reactions/deleteforteamdiscussion) | **DELETE** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/reactions/{reaction_id} | Delete team discussion reaction
*ReactionsApi* | [**reactions/deleteForTeamDiscussionComment**](Apis/ReactionsApi.http#reactions/deleteforteamdiscussioncomment) | **DELETE** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number}/reactions/{reaction_id} | Delete team discussion comment reaction
*ReactionsApi* | [**reactions/listForCommitComment**](Apis/ReactionsApi.http#reactions/listforcommitcomment) | **GET** /repos/{owner}/{repo}/comments/{comment_id}/reactions | List reactions for a commit comment
*ReactionsApi* | [**reactions/listForIssue**](Apis/ReactionsApi.http#reactions/listforissue) | **GET** /repos/{owner}/{repo}/issues/{issue_number}/reactions | List reactions for an issue
*ReactionsApi* | [**reactions/listForIssueComment**](Apis/ReactionsApi.http#reactions/listforissuecomment) | **GET** /repos/{owner}/{repo}/issues/comments/{comment_id}/reactions | List reactions for an issue comment
*ReactionsApi* | [**reactions/listForPullRequestReviewComment**](Apis/ReactionsApi.http#reactions/listforpullrequestreviewcomment) | **GET** /repos/{owner}/{repo}/pulls/comments/{comment_id}/reactions | List reactions for a pull request review comment
*ReactionsApi* | [**reactions/listForRelease**](Apis/ReactionsApi.http#reactions/listforrelease) | **GET** /repos/{owner}/{repo}/releases/{release_id}/reactions | List reactions for a release
*ReactionsApi* | [**reactions/listForTeamDiscussionCommentInOrg**](Apis/ReactionsApi.http#reactions/listforteamdiscussioncommentinorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number}/reactions | List reactions for a team discussion comment
*ReactionsApi* | [**reactions/listForTeamDiscussionCommentLegacy**](Apis/ReactionsApi.http#reactions/listforteamdiscussioncommentlegacy) | **GET** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number}/reactions | List reactions for a team discussion comment (Legacy)
*ReactionsApi* | [**reactions/listForTeamDiscussionInOrg**](Apis/ReactionsApi.http#reactions/listforteamdiscussioninorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/reactions | List reactions for a team discussion
*ReactionsApi* | [**reactions/listForTeamDiscussionLegacy**](Apis/ReactionsApi.http#reactions/listforteamdiscussionlegacy) | **GET** /teams/{team_id}/discussions/{discussion_number}/reactions | List reactions for a team discussion (Legacy)
*ReposApi* | [**repos/acceptInvitationForAuthenticatedUser**](Apis/ReposApi.http#repos/acceptinvitationforauthenticateduser) | **PATCH** /user/repository_invitations/{invitation_id} | Accept a repository invitation
*ReposApi* | [**repos/addAppAccessRestrictions**](Apis/ReposApi.http#repos/addappaccessrestrictions) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/apps | Add app access restrictions
*ReposApi* | [**repos/addCollaborator**](Apis/ReposApi.http#repos/addcollaborator) | **PUT** /repos/{owner}/{repo}/collaborators/{username} | Add a repository collaborator
*ReposApi* | [**repos/addStatusCheckContexts**](Apis/ReposApi.http#repos/addstatuscheckcontexts) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks/contexts | Add status check contexts
*ReposApi* | [**repos/addTeamAccessRestrictions**](Apis/ReposApi.http#repos/addteamaccessrestrictions) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/teams | Add team access restrictions
*ReposApi* | [**repos/addUserAccessRestrictions**](Apis/ReposApi.http#repos/adduseraccessrestrictions) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/users | Add user access restrictions
*ReposApi* | [**repos/cancelPagesDeployment**](Apis/ReposApi.http#repos/cancelpagesdeployment) | **POST** /repos/{owner}/{repo}/pages/deployments/{pages_deployment_id}/cancel | Cancel a GitHub Pages deployment
*ReposApi* | [**repos/checkAutomatedSecurityFixes**](Apis/ReposApi.http#repos/checkautomatedsecurityfixes) | **GET** /repos/{owner}/{repo}/automated-security-fixes | Check if automated security fixes are enabled for a repository
*ReposApi* | [**repos/checkCollaborator**](Apis/ReposApi.http#repos/checkcollaborator) | **GET** /repos/{owner}/{repo}/collaborators/{username} | Check if a user is a repository collaborator
*ReposApi* | [**repos/checkVulnerabilityAlerts**](Apis/ReposApi.http#repos/checkvulnerabilityalerts) | **GET** /repos/{owner}/{repo}/vulnerability-alerts | Check if vulnerability alerts are enabled for a repository
*ReposApi* | [**repos/codeownersErrors**](Apis/ReposApi.http#repos/codeownerserrors) | **GET** /repos/{owner}/{repo}/codeowners/errors | List CODEOWNERS errors
*ReposApi* | [**repos/compareCommits**](Apis/ReposApi.http#repos/comparecommits) | **GET** /repos/{owner}/{repo}/compare/{basehead} | Compare two commits
*ReposApi* | [**repos/createAutolink**](Apis/ReposApi.http#repos/createautolink) | **POST** /repos/{owner}/{repo}/autolinks | Create an autolink reference for a repository
*ReposApi* | [**repos/createCommitComment**](Apis/ReposApi.http#repos/createcommitcomment) | **POST** /repos/{owner}/{repo}/commits/{commit_sha}/comments | Create a commit comment
*ReposApi* | [**repos/createCommitSignatureProtection**](Apis/ReposApi.http#repos/createcommitsignatureprotection) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/required_signatures | Create commit signature protection
*ReposApi* | [**repos/createCommitStatus**](Apis/ReposApi.http#repos/createcommitstatus) | **POST** /repos/{owner}/{repo}/statuses/{sha} | Create a commit status
*ReposApi* | [**repos/createDeployKey**](Apis/ReposApi.http#repos/createdeploykey) | **POST** /repos/{owner}/{repo}/keys | Create a deploy key
*ReposApi* | [**repos/createDeployment**](Apis/ReposApi.http#repos/createdeployment) | **POST** /repos/{owner}/{repo}/deployments | Create a deployment
*ReposApi* | [**repos/createDeploymentBranchPolicy**](Apis/ReposApi.http#repos/createdeploymentbranchpolicy) | **POST** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies | Create a deployment branch policy
*ReposApi* | [**repos/createDeploymentProtectionRule**](Apis/ReposApi.http#repos/createdeploymentprotectionrule) | **POST** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules | Create a custom deployment protection rule on an environment
*ReposApi* | [**repos/createDeploymentStatus**](Apis/ReposApi.http#repos/createdeploymentstatus) | **POST** /repos/{owner}/{repo}/deployments/{deployment_id}/statuses | Create a deployment status
*ReposApi* | [**repos/createDispatchEvent**](Apis/ReposApi.http#repos/createdispatchevent) | **POST** /repos/{owner}/{repo}/dispatches | Create a repository dispatch event
*ReposApi* | [**repos/createForAuthenticatedUser**](Apis/ReposApi.http#repos/createforauthenticateduser) | **POST** /user/repos | Create a repository for the authenticated user
*ReposApi* | [**repos/createFork**](Apis/ReposApi.http#repos/createfork) | **POST** /repos/{owner}/{repo}/forks | Create a fork
*ReposApi* | [**repos/createInOrg**](Apis/ReposApi.http#repos/createinorg) | **POST** /orgs/{org}/repos | Create an organization repository
*ReposApi* | [**repos/createOrUpdateCustomPropertiesValues**](Apis/ReposApi.http#repos/createorupdatecustompropertiesvalues) | **PATCH** /repos/{owner}/{repo}/properties/values | Create or update custom property values for a repository
*ReposApi* | [**repos/createOrUpdateEnvironment**](Apis/ReposApi.http#repos/createorupdateenvironment) | **PUT** /repos/{owner}/{repo}/environments/{environment_name} | Create or update an environment
*ReposApi* | [**repos/createOrUpdateFileContents**](Apis/ReposApi.http#repos/createorupdatefilecontents) | **PUT** /repos/{owner}/{repo}/contents/{path} | Create or update file contents
*ReposApi* | [**repos/createOrgRuleset**](Apis/ReposApi.http#repos/createorgruleset) | **POST** /orgs/{org}/rulesets | Create an organization repository ruleset
*ReposApi* | [**repos/createPagesDeployment**](Apis/ReposApi.http#repos/createpagesdeployment) | **POST** /repos/{owner}/{repo}/pages/deployments | Create a GitHub Pages deployment
*ReposApi* | [**repos/createPagesSite**](Apis/ReposApi.http#repos/createpagessite) | **POST** /repos/{owner}/{repo}/pages | Create a GitHub Pages site
*ReposApi* | [**repos/createRelease**](Apis/ReposApi.http#repos/createrelease) | **POST** /repos/{owner}/{repo}/releases | Create a release
*ReposApi* | [**repos/createRepoRuleset**](Apis/ReposApi.http#repos/createreporuleset) | **POST** /repos/{owner}/{repo}/rulesets | Create a repository ruleset
*ReposApi* | [**repos/createTagProtection**](Apis/ReposApi.http#repos/createtagprotection) | **POST** /repos/{owner}/{repo}/tags/protection | Create a tag protection state for a repository
*ReposApi* | [**repos/createUsingTemplate**](Apis/ReposApi.http#repos/createusingtemplate) | **POST** /repos/{template_owner}/{template_repo}/generate | Create a repository using a template
*ReposApi* | [**repos/createWebhook**](Apis/ReposApi.http#repos/createwebhook) | **POST** /repos/{owner}/{repo}/hooks | Create a repository webhook
*ReposApi* | [**repos/declineInvitationForAuthenticatedUser**](Apis/ReposApi.http#repos/declineinvitationforauthenticateduser) | **DELETE** /user/repository_invitations/{invitation_id} | Decline a repository invitation
*ReposApi* | [**repos/delete**](Apis/ReposApi.http#repos/delete) | **DELETE** /repos/{owner}/{repo} | Delete a repository
*ReposApi* | [**repos/deleteAccessRestrictions**](Apis/ReposApi.http#repos/deleteaccessrestrictions) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions | Delete access restrictions
*ReposApi* | [**repos/deleteAdminBranchProtection**](Apis/ReposApi.http#repos/deleteadminbranchprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/enforce_admins | Delete admin branch protection
*ReposApi* | [**repos/deleteAnEnvironment**](Apis/ReposApi.http#repos/deleteanenvironment) | **DELETE** /repos/{owner}/{repo}/environments/{environment_name} | Delete an environment
*ReposApi* | [**repos/deleteAutolink**](Apis/ReposApi.http#repos/deleteautolink) | **DELETE** /repos/{owner}/{repo}/autolinks/{autolink_id} | Delete an autolink reference from a repository
*ReposApi* | [**repos/deleteBranchProtection**](Apis/ReposApi.http#repos/deletebranchprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection | Delete branch protection
*ReposApi* | [**repos/deleteCommitComment**](Apis/ReposApi.http#repos/deletecommitcomment) | **DELETE** /repos/{owner}/{repo}/comments/{comment_id} | Delete a commit comment
*ReposApi* | [**repos/deleteCommitSignatureProtection**](Apis/ReposApi.http#repos/deletecommitsignatureprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/required_signatures | Delete commit signature protection
*ReposApi* | [**repos/deleteDeployKey**](Apis/ReposApi.http#repos/deletedeploykey) | **DELETE** /repos/{owner}/{repo}/keys/{key_id} | Delete a deploy key
*ReposApi* | [**repos/deleteDeployment**](Apis/ReposApi.http#repos/deletedeployment) | **DELETE** /repos/{owner}/{repo}/deployments/{deployment_id} | Delete a deployment
*ReposApi* | [**repos/deleteDeploymentBranchPolicy**](Apis/ReposApi.http#repos/deletedeploymentbranchpolicy) | **DELETE** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies/{branch_policy_id} | Delete a deployment branch policy
*ReposApi* | [**repos/deleteFile**](Apis/ReposApi.http#repos/deletefile) | **DELETE** /repos/{owner}/{repo}/contents/{path} | Delete a file
*ReposApi* | [**repos/deleteInvitation**](Apis/ReposApi.http#repos/deleteinvitation) | **DELETE** /repos/{owner}/{repo}/invitations/{invitation_id} | Delete a repository invitation
*ReposApi* | [**repos/deleteOrgRuleset**](Apis/ReposApi.http#repos/deleteorgruleset) | **DELETE** /orgs/{org}/rulesets/{ruleset_id} | Delete an organization repository ruleset
*ReposApi* | [**repos/deletePagesSite**](Apis/ReposApi.http#repos/deletepagessite) | **DELETE** /repos/{owner}/{repo}/pages | Delete a GitHub Pages site
*ReposApi* | [**repos/deletePullRequestReviewProtection**](Apis/ReposApi.http#repos/deletepullrequestreviewprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/required_pull_request_reviews | Delete pull request review protection
*ReposApi* | [**repos/deleteRelease**](Apis/ReposApi.http#repos/deleterelease) | **DELETE** /repos/{owner}/{repo}/releases/{release_id} | Delete a release
*ReposApi* | [**repos/deleteReleaseAsset**](Apis/ReposApi.http#repos/deletereleaseasset) | **DELETE** /repos/{owner}/{repo}/releases/assets/{asset_id} | Delete a release asset
*ReposApi* | [**repos/deleteRepoRuleset**](Apis/ReposApi.http#repos/deletereporuleset) | **DELETE** /repos/{owner}/{repo}/rulesets/{ruleset_id} | Delete a repository ruleset
*ReposApi* | [**repos/deleteTagProtection**](Apis/ReposApi.http#repos/deletetagprotection) | **DELETE** /repos/{owner}/{repo}/tags/protection/{tag_protection_id} | Delete a tag protection state for a repository
*ReposApi* | [**repos/deleteWebhook**](Apis/ReposApi.http#repos/deletewebhook) | **DELETE** /repos/{owner}/{repo}/hooks/{hook_id} | Delete a repository webhook
*ReposApi* | [**repos/disableAutomatedSecurityFixes**](Apis/ReposApi.http#repos/disableautomatedsecurityfixes) | **DELETE** /repos/{owner}/{repo}/automated-security-fixes | Disable automated security fixes
*ReposApi* | [**repos/disableDeploymentProtectionRule**](Apis/ReposApi.http#repos/disabledeploymentprotectionrule) | **DELETE** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules/{protection_rule_id} | Disable a custom protection rule for an environment
*ReposApi* | [**repos/disablePrivateVulnerabilityReporting**](Apis/ReposApi.http#repos/disableprivatevulnerabilityreporting) | **DELETE** /repos/{owner}/{repo}/private-vulnerability-reporting | Disable private vulnerability reporting for a repository
*ReposApi* | [**repos/disableVulnerabilityAlerts**](Apis/ReposApi.http#repos/disablevulnerabilityalerts) | **DELETE** /repos/{owner}/{repo}/vulnerability-alerts | Disable vulnerability alerts
*ReposApi* | [**repos/downloadTarballArchive**](Apis/ReposApi.http#repos/downloadtarballarchive) | **GET** /repos/{owner}/{repo}/tarball/{ref} | Download a repository archive (tar)
*ReposApi* | [**repos/downloadZipballArchive**](Apis/ReposApi.http#repos/downloadzipballarchive) | **GET** /repos/{owner}/{repo}/zipball/{ref} | Download a repository archive (zip)
*ReposApi* | [**repos/enableAutomatedSecurityFixes**](Apis/ReposApi.http#repos/enableautomatedsecurityfixes) | **PUT** /repos/{owner}/{repo}/automated-security-fixes | Enable automated security fixes
*ReposApi* | [**repos/enablePrivateVulnerabilityReporting**](Apis/ReposApi.http#repos/enableprivatevulnerabilityreporting) | **PUT** /repos/{owner}/{repo}/private-vulnerability-reporting | Enable private vulnerability reporting for a repository
*ReposApi* | [**repos/enableVulnerabilityAlerts**](Apis/ReposApi.http#repos/enablevulnerabilityalerts) | **PUT** /repos/{owner}/{repo}/vulnerability-alerts | Enable vulnerability alerts
*ReposApi* | [**repos/generateReleaseNotes**](Apis/ReposApi.http#repos/generatereleasenotes) | **POST** /repos/{owner}/{repo}/releases/generate-notes | Generate release notes content for a release
*ReposApi* | [**repos/get**](Apis/ReposApi.http#repos/get) | **GET** /repos/{owner}/{repo} | Get a repository
*ReposApi* | [**repos/getAccessRestrictions**](Apis/ReposApi.http#repos/getaccessrestrictions) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions | Get access restrictions
*ReposApi* | [**repos/getAdminBranchProtection**](Apis/ReposApi.http#repos/getadminbranchprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/enforce_admins | Get admin branch protection
*ReposApi* | [**repos/getAllDeploymentProtectionRules**](Apis/ReposApi.http#repos/getalldeploymentprotectionrules) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules | Get all deployment protection rules for an environment
*ReposApi* | [**repos/getAllEnvironments**](Apis/ReposApi.http#repos/getallenvironments) | **GET** /repos/{owner}/{repo}/environments | List environments
*ReposApi* | [**repos/getAllStatusCheckContexts**](Apis/ReposApi.http#repos/getallstatuscheckcontexts) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks/contexts | Get all status check contexts
*ReposApi* | [**repos/getAllTopics**](Apis/ReposApi.http#repos/getalltopics) | **GET** /repos/{owner}/{repo}/topics | Get all repository topics
*ReposApi* | [**repos/getAppsWithAccessToProtectedBranch**](Apis/ReposApi.http#repos/getappswithaccesstoprotectedbranch) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/apps | Get apps with access to the protected branch
*ReposApi* | [**repos/getAutolink**](Apis/ReposApi.http#repos/getautolink) | **GET** /repos/{owner}/{repo}/autolinks/{autolink_id} | Get an autolink reference of a repository
*ReposApi* | [**repos/getBranch**](Apis/ReposApi.http#repos/getbranch) | **GET** /repos/{owner}/{repo}/branches/{branch} | Get a branch
*ReposApi* | [**repos/getBranchProtection**](Apis/ReposApi.http#repos/getbranchprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection | Get branch protection
*ReposApi* | [**repos/getBranchRules**](Apis/ReposApi.http#repos/getbranchrules) | **GET** /repos/{owner}/{repo}/rules/branches/{branch} | Get rules for a branch
*ReposApi* | [**repos/getClones**](Apis/ReposApi.http#repos/getclones) | **GET** /repos/{owner}/{repo}/traffic/clones | Get repository clones
*ReposApi* | [**repos/getCodeFrequencyStats**](Apis/ReposApi.http#repos/getcodefrequencystats) | **GET** /repos/{owner}/{repo}/stats/code_frequency | Get the weekly commit activity
*ReposApi* | [**repos/getCollaboratorPermissionLevel**](Apis/ReposApi.http#repos/getcollaboratorpermissionlevel) | **GET** /repos/{owner}/{repo}/collaborators/{username}/permission | Get repository permissions for a user
*ReposApi* | [**repos/getCombinedStatusForRef**](Apis/ReposApi.http#repos/getcombinedstatusforref) | **GET** /repos/{owner}/{repo}/commits/{ref}/status | Get the combined status for a specific reference
*ReposApi* | [**repos/getCommit**](Apis/ReposApi.http#repos/getcommit) | **GET** /repos/{owner}/{repo}/commits/{ref} | Get a commit
*ReposApi* | [**repos/getCommitActivityStats**](Apis/ReposApi.http#repos/getcommitactivitystats) | **GET** /repos/{owner}/{repo}/stats/commit_activity | Get the last year of commit activity
*ReposApi* | [**repos/getCommitComment**](Apis/ReposApi.http#repos/getcommitcomment) | **GET** /repos/{owner}/{repo}/comments/{comment_id} | Get a commit comment
*ReposApi* | [**repos/getCommitSignatureProtection**](Apis/ReposApi.http#repos/getcommitsignatureprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/required_signatures | Get commit signature protection
*ReposApi* | [**repos/getCommunityProfileMetrics**](Apis/ReposApi.http#repos/getcommunityprofilemetrics) | **GET** /repos/{owner}/{repo}/community/profile | Get community profile metrics
*ReposApi* | [**repos/getContent**](Apis/ReposApi.http#repos/getcontent) | **GET** /repos/{owner}/{repo}/contents/{path} | Get repository content
*ReposApi* | [**repos/getContributorsStats**](Apis/ReposApi.http#repos/getcontributorsstats) | **GET** /repos/{owner}/{repo}/stats/contributors | Get all contributor commit activity
*ReposApi* | [**repos/getCustomDeploymentProtectionRule**](Apis/ReposApi.http#repos/getcustomdeploymentprotectionrule) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules/{protection_rule_id} | Get a custom deployment protection rule
*ReposApi* | [**repos/getCustomPropertiesValues**](Apis/ReposApi.http#repos/getcustompropertiesvalues) | **GET** /repos/{owner}/{repo}/properties/values | Get all custom property values for a repository
*ReposApi* | [**repos/getDeployKey**](Apis/ReposApi.http#repos/getdeploykey) | **GET** /repos/{owner}/{repo}/keys/{key_id} | Get a deploy key
*ReposApi* | [**repos/getDeployment**](Apis/ReposApi.http#repos/getdeployment) | **GET** /repos/{owner}/{repo}/deployments/{deployment_id} | Get a deployment
*ReposApi* | [**repos/getDeploymentBranchPolicy**](Apis/ReposApi.http#repos/getdeploymentbranchpolicy) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies/{branch_policy_id} | Get a deployment branch policy
*ReposApi* | [**repos/getDeploymentStatus**](Apis/ReposApi.http#repos/getdeploymentstatus) | **GET** /repos/{owner}/{repo}/deployments/{deployment_id}/statuses/{status_id} | Get a deployment status
*ReposApi* | [**repos/getEnvironment**](Apis/ReposApi.http#repos/getenvironment) | **GET** /repos/{owner}/{repo}/environments/{environment_name} | Get an environment
*ReposApi* | [**repos/getLatestPagesBuild**](Apis/ReposApi.http#repos/getlatestpagesbuild) | **GET** /repos/{owner}/{repo}/pages/builds/latest | Get latest Pages build
*ReposApi* | [**repos/getLatestRelease**](Apis/ReposApi.http#repos/getlatestrelease) | **GET** /repos/{owner}/{repo}/releases/latest | Get the latest release
*ReposApi* | [**repos/getOrgRuleSuite**](Apis/ReposApi.http#repos/getorgrulesuite) | **GET** /orgs/{org}/rulesets/rule-suites/{rule_suite_id} | Get an organization rule suite
*ReposApi* | [**repos/getOrgRuleSuites**](Apis/ReposApi.http#repos/getorgrulesuites) | **GET** /orgs/{org}/rulesets/rule-suites | List organization rule suites
*ReposApi* | [**repos/getOrgRuleset**](Apis/ReposApi.http#repos/getorgruleset) | **GET** /orgs/{org}/rulesets/{ruleset_id} | Get an organization repository ruleset
*ReposApi* | [**repos/getOrgRulesets**](Apis/ReposApi.http#repos/getorgrulesets) | **GET** /orgs/{org}/rulesets | Get all organization repository rulesets
*ReposApi* | [**repos/getPages**](Apis/ReposApi.http#repos/getpages) | **GET** /repos/{owner}/{repo}/pages | Get a GitHub Pages site
*ReposApi* | [**repos/getPagesBuild**](Apis/ReposApi.http#repos/getpagesbuild) | **GET** /repos/{owner}/{repo}/pages/builds/{build_id} | Get GitHub Pages build
*ReposApi* | [**repos/getPagesDeployment**](Apis/ReposApi.http#repos/getpagesdeployment) | **GET** /repos/{owner}/{repo}/pages/deployments/{pages_deployment_id} | Get the status of a GitHub Pages deployment
*ReposApi* | [**repos/getPagesHealthCheck**](Apis/ReposApi.http#repos/getpageshealthcheck) | **GET** /repos/{owner}/{repo}/pages/health | Get a DNS health check for GitHub Pages
*ReposApi* | [**repos/getParticipationStats**](Apis/ReposApi.http#repos/getparticipationstats) | **GET** /repos/{owner}/{repo}/stats/participation | Get the weekly commit count
*ReposApi* | [**repos/getPullRequestReviewProtection**](Apis/ReposApi.http#repos/getpullrequestreviewprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/required_pull_request_reviews | Get pull request review protection
*ReposApi* | [**repos/getPunchCardStats**](Apis/ReposApi.http#repos/getpunchcardstats) | **GET** /repos/{owner}/{repo}/stats/punch_card | Get the hourly commit count for each day
*ReposApi* | [**repos/getReadme**](Apis/ReposApi.http#repos/getreadme) | **GET** /repos/{owner}/{repo}/readme | Get a repository README
*ReposApi* | [**repos/getReadmeInDirectory**](Apis/ReposApi.http#repos/getreadmeindirectory) | **GET** /repos/{owner}/{repo}/readme/{dir} | Get a repository README for a directory
*ReposApi* | [**repos/getRelease**](Apis/ReposApi.http#repos/getrelease) | **GET** /repos/{owner}/{repo}/releases/{release_id} | Get a release
*ReposApi* | [**repos/getReleaseAsset**](Apis/ReposApi.http#repos/getreleaseasset) | **GET** /repos/{owner}/{repo}/releases/assets/{asset_id} | Get a release asset
*ReposApi* | [**repos/getReleaseByTag**](Apis/ReposApi.http#repos/getreleasebytag) | **GET** /repos/{owner}/{repo}/releases/tags/{tag} | Get a release by tag name
*ReposApi* | [**repos/getRepoRuleSuite**](Apis/ReposApi.http#repos/getreporulesuite) | **GET** /repos/{owner}/{repo}/rulesets/rule-suites/{rule_suite_id} | Get a repository rule suite
*ReposApi* | [**repos/getRepoRuleSuites**](Apis/ReposApi.http#repos/getreporulesuites) | **GET** /repos/{owner}/{repo}/rulesets/rule-suites | List repository rule suites
*ReposApi* | [**repos/getRepoRuleset**](Apis/ReposApi.http#repos/getreporuleset) | **GET** /repos/{owner}/{repo}/rulesets/{ruleset_id} | Get a repository ruleset
*ReposApi* | [**repos/getRepoRulesets**](Apis/ReposApi.http#repos/getreporulesets) | **GET** /repos/{owner}/{repo}/rulesets | Get all repository rulesets
*ReposApi* | [**repos/getStatusChecksProtection**](Apis/ReposApi.http#repos/getstatuschecksprotection) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks | Get status checks protection
*ReposApi* | [**repos/getTeamsWithAccessToProtectedBranch**](Apis/ReposApi.http#repos/getteamswithaccesstoprotectedbranch) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/teams | Get teams with access to the protected branch
*ReposApi* | [**repos/getTopPaths**](Apis/ReposApi.http#repos/gettoppaths) | **GET** /repos/{owner}/{repo}/traffic/popular/paths | Get top referral paths
*ReposApi* | [**repos/getTopReferrers**](Apis/ReposApi.http#repos/gettopreferrers) | **GET** /repos/{owner}/{repo}/traffic/popular/referrers | Get top referral sources
*ReposApi* | [**repos/getUsersWithAccessToProtectedBranch**](Apis/ReposApi.http#repos/getuserswithaccesstoprotectedbranch) | **GET** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/users | Get users with access to the protected branch
*ReposApi* | [**repos/getViews**](Apis/ReposApi.http#repos/getviews) | **GET** /repos/{owner}/{repo}/traffic/views | Get page views
*ReposApi* | [**repos/getWebhook**](Apis/ReposApi.http#repos/getwebhook) | **GET** /repos/{owner}/{repo}/hooks/{hook_id} | Get a repository webhook
*ReposApi* | [**repos/getWebhookConfigForRepo**](Apis/ReposApi.http#repos/getwebhookconfigforrepo) | **GET** /repos/{owner}/{repo}/hooks/{hook_id}/config | Get a webhook configuration for a repository
*ReposApi* | [**repos/getWebhookDelivery**](Apis/ReposApi.http#repos/getwebhookdelivery) | **GET** /repos/{owner}/{repo}/hooks/{hook_id}/deliveries/{delivery_id} | Get a delivery for a repository webhook
*ReposApi* | [**repos/listActivities**](Apis/ReposApi.http#repos/listactivities) | **GET** /repos/{owner}/{repo}/activity | List repository activities
*ReposApi* | [**repos/listAutolinks**](Apis/ReposApi.http#repos/listautolinks) | **GET** /repos/{owner}/{repo}/autolinks | Get all autolinks of a repository
*ReposApi* | [**repos/listBranches**](Apis/ReposApi.http#repos/listbranches) | **GET** /repos/{owner}/{repo}/branches | List branches
*ReposApi* | [**repos/listBranchesForHeadCommit**](Apis/ReposApi.http#repos/listbranchesforheadcommit) | **GET** /repos/{owner}/{repo}/commits/{commit_sha}/branches-where-head | List branches for HEAD commit
*ReposApi* | [**repos/listCollaborators**](Apis/ReposApi.http#repos/listcollaborators) | **GET** /repos/{owner}/{repo}/collaborators | List repository collaborators
*ReposApi* | [**repos/listCommentsForCommit**](Apis/ReposApi.http#repos/listcommentsforcommit) | **GET** /repos/{owner}/{repo}/commits/{commit_sha}/comments | List commit comments
*ReposApi* | [**repos/listCommitCommentsForRepo**](Apis/ReposApi.http#repos/listcommitcommentsforrepo) | **GET** /repos/{owner}/{repo}/comments | List commit comments for a repository
*ReposApi* | [**repos/listCommitStatusesForRef**](Apis/ReposApi.http#repos/listcommitstatusesforref) | **GET** /repos/{owner}/{repo}/commits/{ref}/statuses | List commit statuses for a reference
*ReposApi* | [**repos/listCommits**](Apis/ReposApi.http#repos/listcommits) | **GET** /repos/{owner}/{repo}/commits | List commits
*ReposApi* | [**repos/listContributors**](Apis/ReposApi.http#repos/listcontributors) | **GET** /repos/{owner}/{repo}/contributors | List repository contributors
*ReposApi* | [**repos/listCustomDeploymentRuleIntegrations**](Apis/ReposApi.http#repos/listcustomdeploymentruleintegrations) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment_protection_rules/apps | List custom deployment rule integrations available for an environment
*ReposApi* | [**repos/listDeployKeys**](Apis/ReposApi.http#repos/listdeploykeys) | **GET** /repos/{owner}/{repo}/keys | List deploy keys
*ReposApi* | [**repos/listDeploymentBranchPolicies**](Apis/ReposApi.http#repos/listdeploymentbranchpolicies) | **GET** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies | List deployment branch policies
*ReposApi* | [**repos/listDeploymentStatuses**](Apis/ReposApi.http#repos/listdeploymentstatuses) | **GET** /repos/{owner}/{repo}/deployments/{deployment_id}/statuses | List deployment statuses
*ReposApi* | [**repos/listDeployments**](Apis/ReposApi.http#repos/listdeployments) | **GET** /repos/{owner}/{repo}/deployments | List deployments
*ReposApi* | [**repos/listForAuthenticatedUser**](Apis/ReposApi.http#repos/listforauthenticateduser) | **GET** /user/repos | List repositories for the authenticated user
*ReposApi* | [**repos/listForOrg**](Apis/ReposApi.http#repos/listfororg) | **GET** /orgs/{org}/repos | List organization repositories
*ReposApi* | [**repos/listForUser**](Apis/ReposApi.http#repos/listforuser) | **GET** /users/{username}/repos | List repositories for a user
*ReposApi* | [**repos/listForks**](Apis/ReposApi.http#repos/listforks) | **GET** /repos/{owner}/{repo}/forks | List forks
*ReposApi* | [**repos/listInvitations**](Apis/ReposApi.http#repos/listinvitations) | **GET** /repos/{owner}/{repo}/invitations | List repository invitations
*ReposApi* | [**repos/listInvitationsForAuthenticatedUser**](Apis/ReposApi.http#repos/listinvitationsforauthenticateduser) | **GET** /user/repository_invitations | List repository invitations for the authenticated user
*ReposApi* | [**repos/listLanguages**](Apis/ReposApi.http#repos/listlanguages) | **GET** /repos/{owner}/{repo}/languages | List repository languages
*ReposApi* | [**repos/listPagesBuilds**](Apis/ReposApi.http#repos/listpagesbuilds) | **GET** /repos/{owner}/{repo}/pages/builds | List GitHub Pages builds
*ReposApi* | [**repos/listPublic**](Apis/ReposApi.http#repos/listpublic) | **GET** /repositories | List public repositories
*ReposApi* | [**repos/listPullRequestsAssociatedWithCommit**](Apis/ReposApi.http#repos/listpullrequestsassociatedwithcommit) | **GET** /repos/{owner}/{repo}/commits/{commit_sha}/pulls | List pull requests associated with a commit
*ReposApi* | [**repos/listReleaseAssets**](Apis/ReposApi.http#repos/listreleaseassets) | **GET** /repos/{owner}/{repo}/releases/{release_id}/assets | List release assets
*ReposApi* | [**repos/listReleases**](Apis/ReposApi.http#repos/listreleases) | **GET** /repos/{owner}/{repo}/releases | List releases
*ReposApi* | [**repos/listTagProtection**](Apis/ReposApi.http#repos/listtagprotection) | **GET** /repos/{owner}/{repo}/tags/protection | List tag protection states for a repository
*ReposApi* | [**repos/listTags**](Apis/ReposApi.http#repos/listtags) | **GET** /repos/{owner}/{repo}/tags | List repository tags
*ReposApi* | [**repos/listTeams**](Apis/ReposApi.http#repos/listteams) | **GET** /repos/{owner}/{repo}/teams | List repository teams
*ReposApi* | [**repos/listWebhookDeliveries**](Apis/ReposApi.http#repos/listwebhookdeliveries) | **GET** /repos/{owner}/{repo}/hooks/{hook_id}/deliveries | List deliveries for a repository webhook
*ReposApi* | [**repos/listWebhooks**](Apis/ReposApi.http#repos/listwebhooks) | **GET** /repos/{owner}/{repo}/hooks | List repository webhooks
*ReposApi* | [**repos/merge**](Apis/ReposApi.http#repos/merge) | **POST** /repos/{owner}/{repo}/merges | Merge a branch
*ReposApi* | [**repos/mergeUpstream**](Apis/ReposApi.http#repos/mergeupstream) | **POST** /repos/{owner}/{repo}/merge-upstream | Sync a fork branch with the upstream repository
*ReposApi* | [**repos/pingWebhook**](Apis/ReposApi.http#repos/pingwebhook) | **POST** /repos/{owner}/{repo}/hooks/{hook_id}/pings | Ping a repository webhook
*ReposApi* | [**repos/redeliverWebhookDelivery**](Apis/ReposApi.http#repos/redeliverwebhookdelivery) | **POST** /repos/{owner}/{repo}/hooks/{hook_id}/deliveries/{delivery_id}/attempts | Redeliver a delivery for a repository webhook
*ReposApi* | [**repos/removeAppAccessRestrictions**](Apis/ReposApi.http#repos/removeappaccessrestrictions) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/apps | Remove app access restrictions
*ReposApi* | [**repos/removeCollaborator**](Apis/ReposApi.http#repos/removecollaborator) | **DELETE** /repos/{owner}/{repo}/collaborators/{username} | Remove a repository collaborator
*ReposApi* | [**repos/removeStatusCheckContexts**](Apis/ReposApi.http#repos/removestatuscheckcontexts) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks/contexts | Remove status check contexts
*ReposApi* | [**repos/removeStatusCheckProtection**](Apis/ReposApi.http#repos/removestatuscheckprotection) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks | Remove status check protection
*ReposApi* | [**repos/removeTeamAccessRestrictions**](Apis/ReposApi.http#repos/removeteamaccessrestrictions) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/teams | Remove team access restrictions
*ReposApi* | [**repos/removeUserAccessRestrictions**](Apis/ReposApi.http#repos/removeuseraccessrestrictions) | **DELETE** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/users | Remove user access restrictions
*ReposApi* | [**repos/renameBranch**](Apis/ReposApi.http#repos/renamebranch) | **POST** /repos/{owner}/{repo}/branches/{branch}/rename | Rename a branch
*ReposApi* | [**repos/replaceAllTopics**](Apis/ReposApi.http#repos/replacealltopics) | **PUT** /repos/{owner}/{repo}/topics | Replace all repository topics
*ReposApi* | [**repos/requestPagesBuild**](Apis/ReposApi.http#repos/requestpagesbuild) | **POST** /repos/{owner}/{repo}/pages/builds | Request a GitHub Pages build
*ReposApi* | [**repos/setAdminBranchProtection**](Apis/ReposApi.http#repos/setadminbranchprotection) | **POST** /repos/{owner}/{repo}/branches/{branch}/protection/enforce_admins | Set admin branch protection
*ReposApi* | [**repos/setAppAccessRestrictions**](Apis/ReposApi.http#repos/setappaccessrestrictions) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/apps | Set app access restrictions
*ReposApi* | [**repos/setStatusCheckContexts**](Apis/ReposApi.http#repos/setstatuscheckcontexts) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks/contexts | Set status check contexts
*ReposApi* | [**repos/setTeamAccessRestrictions**](Apis/ReposApi.http#repos/setteamaccessrestrictions) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/teams | Set team access restrictions
*ReposApi* | [**repos/setUserAccessRestrictions**](Apis/ReposApi.http#repos/setuseraccessrestrictions) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection/restrictions/users | Set user access restrictions
*ReposApi* | [**repos/testPushWebhook**](Apis/ReposApi.http#repos/testpushwebhook) | **POST** /repos/{owner}/{repo}/hooks/{hook_id}/tests | Test the push repository webhook
*ReposApi* | [**repos/transfer**](Apis/ReposApi.http#repos/transfer) | **POST** /repos/{owner}/{repo}/transfer | Transfer a repository
*ReposApi* | [**repos/update**](Apis/ReposApi.http#repos/update) | **PATCH** /repos/{owner}/{repo} | Update a repository
*ReposApi* | [**repos/updateBranchProtection**](Apis/ReposApi.http#repos/updatebranchprotection) | **PUT** /repos/{owner}/{repo}/branches/{branch}/protection | Update branch protection
*ReposApi* | [**repos/updateCommitComment**](Apis/ReposApi.http#repos/updatecommitcomment) | **PATCH** /repos/{owner}/{repo}/comments/{comment_id} | Update a commit comment
*ReposApi* | [**repos/updateDeploymentBranchPolicy**](Apis/ReposApi.http#repos/updatedeploymentbranchpolicy) | **PUT** /repos/{owner}/{repo}/environments/{environment_name}/deployment-branch-policies/{branch_policy_id} | Update a deployment branch policy
*ReposApi* | [**repos/updateInformationAboutPagesSite**](Apis/ReposApi.http#repos/updateinformationaboutpagessite) | **PUT** /repos/{owner}/{repo}/pages | Update information about a GitHub Pages site
*ReposApi* | [**repos/updateInvitation**](Apis/ReposApi.http#repos/updateinvitation) | **PATCH** /repos/{owner}/{repo}/invitations/{invitation_id} | Update a repository invitation
*ReposApi* | [**repos/updateOrgRuleset**](Apis/ReposApi.http#repos/updateorgruleset) | **PUT** /orgs/{org}/rulesets/{ruleset_id} | Update an organization repository ruleset
*ReposApi* | [**repos/updatePullRequestReviewProtection**](Apis/ReposApi.http#repos/updatepullrequestreviewprotection) | **PATCH** /repos/{owner}/{repo}/branches/{branch}/protection/required_pull_request_reviews | Update pull request review protection
*ReposApi* | [**repos/updateRelease**](Apis/ReposApi.http#repos/updaterelease) | **PATCH** /repos/{owner}/{repo}/releases/{release_id} | Update a release
*ReposApi* | [**repos/updateReleaseAsset**](Apis/ReposApi.http#repos/updatereleaseasset) | **PATCH** /repos/{owner}/{repo}/releases/assets/{asset_id} | Update a release asset
*ReposApi* | [**repos/updateRepoRuleset**](Apis/ReposApi.http#repos/updatereporuleset) | **PUT** /repos/{owner}/{repo}/rulesets/{ruleset_id} | Update a repository ruleset
*ReposApi* | [**repos/updateStatusCheckProtection**](Apis/ReposApi.http#repos/updatestatuscheckprotection) | **PATCH** /repos/{owner}/{repo}/branches/{branch}/protection/required_status_checks | Update status check protection
*ReposApi* | [**repos/updateWebhook**](Apis/ReposApi.http#repos/updatewebhook) | **PATCH** /repos/{owner}/{repo}/hooks/{hook_id} | Update a repository webhook
*ReposApi* | [**repos/updateWebhookConfigForRepo**](Apis/ReposApi.http#repos/updatewebhookconfigforrepo) | **PATCH** /repos/{owner}/{repo}/hooks/{hook_id}/config | Update a webhook configuration for a repository
*ReposApi* | [**repos/uploadReleaseAsset**](Apis/ReposApi.http#repos/uploadreleaseasset) | **POST** /repos/{owner}/{repo}/releases/{release_id}/assets | Upload a release asset
*SearchApi* | [**search/code**](Apis/SearchApi.http#search/code) | **GET** /search/code | Search code
*SearchApi* | [**search/commits**](Apis/SearchApi.http#search/commits) | **GET** /search/commits | Search commits
*SearchApi* | [**search/issuesAndPullRequests**](Apis/SearchApi.http#search/issuesandpullrequests) | **GET** /search/issues | Search issues and pull requests
*SearchApi* | [**search/labels**](Apis/SearchApi.http#search/labels) | **GET** /search/labels | Search labels
*SearchApi* | [**search/repos**](Apis/SearchApi.http#search/repos) | **GET** /search/repositories | Search repositories
*SearchApi* | [**search/topics**](Apis/SearchApi.http#search/topics) | **GET** /search/topics | Search topics
*SearchApi* | [**search/users**](Apis/SearchApi.http#search/users) | **GET** /search/users | Search users
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
*TeamsApi* | [**teams/addMemberLegacy**](Apis/TeamsApi.http#teams/addmemberlegacy) | **PUT** /teams/{team_id}/members/{username} | Add team member (Legacy)
*TeamsApi* | [**teams/addOrUpdateMembershipForUserInOrg**](Apis/TeamsApi.http#teams/addorupdatemembershipforuserinorg) | **PUT** /orgs/{org}/teams/{team_slug}/memberships/{username} | Add or update team membership for a user
*TeamsApi* | [**teams/addOrUpdateMembershipForUserLegacy**](Apis/TeamsApi.http#teams/addorupdatemembershipforuserlegacy) | **PUT** /teams/{team_id}/memberships/{username} | Add or update team membership for a user (Legacy)
*TeamsApi* | [**teams/addOrUpdateProjectPermissionsInOrg**](Apis/TeamsApi.http#teams/addorupdateprojectpermissionsinorg) | **PUT** /orgs/{org}/teams/{team_slug}/projects/{project_id} | Add or update team project permissions
*TeamsApi* | [**teams/addOrUpdateProjectPermissionsLegacy**](Apis/TeamsApi.http#teams/addorupdateprojectpermissionslegacy) | **PUT** /teams/{team_id}/projects/{project_id} | Add or update team project permissions (Legacy)
*TeamsApi* | [**teams/addOrUpdateRepoPermissionsInOrg**](Apis/TeamsApi.http#teams/addorupdaterepopermissionsinorg) | **PUT** /orgs/{org}/teams/{team_slug}/repos/{owner}/{repo} | Add or update team repository permissions
*TeamsApi* | [**teams/addOrUpdateRepoPermissionsLegacy**](Apis/TeamsApi.http#teams/addorupdaterepopermissionslegacy) | **PUT** /teams/{team_id}/repos/{owner}/{repo} | Add or update team repository permissions (Legacy)
*TeamsApi* | [**teams/checkPermissionsForProjectInOrg**](Apis/TeamsApi.http#teams/checkpermissionsforprojectinorg) | **GET** /orgs/{org}/teams/{team_slug}/projects/{project_id} | Check team permissions for a project
*TeamsApi* | [**teams/checkPermissionsForProjectLegacy**](Apis/TeamsApi.http#teams/checkpermissionsforprojectlegacy) | **GET** /teams/{team_id}/projects/{project_id} | Check team permissions for a project (Legacy)
*TeamsApi* | [**teams/checkPermissionsForRepoInOrg**](Apis/TeamsApi.http#teams/checkpermissionsforrepoinorg) | **GET** /orgs/{org}/teams/{team_slug}/repos/{owner}/{repo} | Check team permissions for a repository
*TeamsApi* | [**teams/checkPermissionsForRepoLegacy**](Apis/TeamsApi.http#teams/checkpermissionsforrepolegacy) | **GET** /teams/{team_id}/repos/{owner}/{repo} | Check team permissions for a repository (Legacy)
*TeamsApi* | [**teams/create**](Apis/TeamsApi.http#teams/create) | **POST** /orgs/{org}/teams | Create a team
*TeamsApi* | [**teams/createDiscussionCommentInOrg**](Apis/TeamsApi.http#teams/creatediscussioncommentinorg) | **POST** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments | Create a discussion comment
*TeamsApi* | [**teams/createDiscussionCommentLegacy**](Apis/TeamsApi.http#teams/creatediscussioncommentlegacy) | **POST** /teams/{team_id}/discussions/{discussion_number}/comments | Create a discussion comment (Legacy)
*TeamsApi* | [**teams/createDiscussionInOrg**](Apis/TeamsApi.http#teams/creatediscussioninorg) | **POST** /orgs/{org}/teams/{team_slug}/discussions | Create a discussion
*TeamsApi* | [**teams/createDiscussionLegacy**](Apis/TeamsApi.http#teams/creatediscussionlegacy) | **POST** /teams/{team_id}/discussions | Create a discussion (Legacy)
*TeamsApi* | [**teams/deleteDiscussionCommentInOrg**](Apis/TeamsApi.http#teams/deletediscussioncommentinorg) | **DELETE** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number} | Delete a discussion comment
*TeamsApi* | [**teams/deleteDiscussionCommentLegacy**](Apis/TeamsApi.http#teams/deletediscussioncommentlegacy) | **DELETE** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number} | Delete a discussion comment (Legacy)
*TeamsApi* | [**teams/deleteDiscussionInOrg**](Apis/TeamsApi.http#teams/deletediscussioninorg) | **DELETE** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number} | Delete a discussion
*TeamsApi* | [**teams/deleteDiscussionLegacy**](Apis/TeamsApi.http#teams/deletediscussionlegacy) | **DELETE** /teams/{team_id}/discussions/{discussion_number} | Delete a discussion (Legacy)
*TeamsApi* | [**teams/deleteInOrg**](Apis/TeamsApi.http#teams/deleteinorg) | **DELETE** /orgs/{org}/teams/{team_slug} | Delete a team
*TeamsApi* | [**teams/deleteLegacy**](Apis/TeamsApi.http#teams/deletelegacy) | **DELETE** /teams/{team_id} | Delete a team (Legacy)
*TeamsApi* | [**teams/getByName**](Apis/TeamsApi.http#teams/getbyname) | **GET** /orgs/{org}/teams/{team_slug} | Get a team by name
*TeamsApi* | [**teams/getDiscussionCommentInOrg**](Apis/TeamsApi.http#teams/getdiscussioncommentinorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number} | Get a discussion comment
*TeamsApi* | [**teams/getDiscussionCommentLegacy**](Apis/TeamsApi.http#teams/getdiscussioncommentlegacy) | **GET** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number} | Get a discussion comment (Legacy)
*TeamsApi* | [**teams/getDiscussionInOrg**](Apis/TeamsApi.http#teams/getdiscussioninorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number} | Get a discussion
*TeamsApi* | [**teams/getDiscussionLegacy**](Apis/TeamsApi.http#teams/getdiscussionlegacy) | **GET** /teams/{team_id}/discussions/{discussion_number} | Get a discussion (Legacy)
*TeamsApi* | [**teams/getLegacy**](Apis/TeamsApi.http#teams/getlegacy) | **GET** /teams/{team_id} | Get a team (Legacy)
*TeamsApi* | [**teams/getMemberLegacy**](Apis/TeamsApi.http#teams/getmemberlegacy) | **GET** /teams/{team_id}/members/{username} | Get team member (Legacy)
*TeamsApi* | [**teams/getMembershipForUserInOrg**](Apis/TeamsApi.http#teams/getmembershipforuserinorg) | **GET** /orgs/{org}/teams/{team_slug}/memberships/{username} | Get team membership for a user
*TeamsApi* | [**teams/getMembershipForUserLegacy**](Apis/TeamsApi.http#teams/getmembershipforuserlegacy) | **GET** /teams/{team_id}/memberships/{username} | Get team membership for a user (Legacy)
*TeamsApi* | [**teams/list**](Apis/TeamsApi.http#teams/list) | **GET** /orgs/{org}/teams | List teams
*TeamsApi* | [**teams/listChildInOrg**](Apis/TeamsApi.http#teams/listchildinorg) | **GET** /orgs/{org}/teams/{team_slug}/teams | List child teams
*TeamsApi* | [**teams/listChildLegacy**](Apis/TeamsApi.http#teams/listchildlegacy) | **GET** /teams/{team_id}/teams | List child teams (Legacy)
*TeamsApi* | [**teams/listDiscussionCommentsInOrg**](Apis/TeamsApi.http#teams/listdiscussioncommentsinorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments | List discussion comments
*TeamsApi* | [**teams/listDiscussionCommentsLegacy**](Apis/TeamsApi.http#teams/listdiscussioncommentslegacy) | **GET** /teams/{team_id}/discussions/{discussion_number}/comments | List discussion comments (Legacy)
*TeamsApi* | [**teams/listDiscussionsInOrg**](Apis/TeamsApi.http#teams/listdiscussionsinorg) | **GET** /orgs/{org}/teams/{team_slug}/discussions | List discussions
*TeamsApi* | [**teams/listDiscussionsLegacy**](Apis/TeamsApi.http#teams/listdiscussionslegacy) | **GET** /teams/{team_id}/discussions | List discussions (Legacy)
*TeamsApi* | [**teams/listForAuthenticatedUser**](Apis/TeamsApi.http#teams/listforauthenticateduser) | **GET** /user/teams | List teams for the authenticated user
*TeamsApi* | [**teams/listMembersInOrg**](Apis/TeamsApi.http#teams/listmembersinorg) | **GET** /orgs/{org}/teams/{team_slug}/members | List team members
*TeamsApi* | [**teams/listMembersLegacy**](Apis/TeamsApi.http#teams/listmemberslegacy) | **GET** /teams/{team_id}/members | List team members (Legacy)
*TeamsApi* | [**teams/listPendingInvitationsInOrg**](Apis/TeamsApi.http#teams/listpendinginvitationsinorg) | **GET** /orgs/{org}/teams/{team_slug}/invitations | List pending team invitations
*TeamsApi* | [**teams/listPendingInvitationsLegacy**](Apis/TeamsApi.http#teams/listpendinginvitationslegacy) | **GET** /teams/{team_id}/invitations | List pending team invitations (Legacy)
*TeamsApi* | [**teams/listProjectsInOrg**](Apis/TeamsApi.http#teams/listprojectsinorg) | **GET** /orgs/{org}/teams/{team_slug}/projects | List team projects
*TeamsApi* | [**teams/listProjectsLegacy**](Apis/TeamsApi.http#teams/listprojectslegacy) | **GET** /teams/{team_id}/projects | List team projects (Legacy)
*TeamsApi* | [**teams/listReposInOrg**](Apis/TeamsApi.http#teams/listreposinorg) | **GET** /orgs/{org}/teams/{team_slug}/repos | List team repositories
*TeamsApi* | [**teams/listReposLegacy**](Apis/TeamsApi.http#teams/listreposlegacy) | **GET** /teams/{team_id}/repos | List team repositories (Legacy)
*TeamsApi* | [**teams/removeMemberLegacy**](Apis/TeamsApi.http#teams/removememberlegacy) | **DELETE** /teams/{team_id}/members/{username} | Remove team member (Legacy)
*TeamsApi* | [**teams/removeMembershipForUserInOrg**](Apis/TeamsApi.http#teams/removemembershipforuserinorg) | **DELETE** /orgs/{org}/teams/{team_slug}/memberships/{username} | Remove team membership for a user
*TeamsApi* | [**teams/removeMembershipForUserLegacy**](Apis/TeamsApi.http#teams/removemembershipforuserlegacy) | **DELETE** /teams/{team_id}/memberships/{username} | Remove team membership for a user (Legacy)
*TeamsApi* | [**teams/removeProjectInOrg**](Apis/TeamsApi.http#teams/removeprojectinorg) | **DELETE** /orgs/{org}/teams/{team_slug}/projects/{project_id} | Remove a project from a team
*TeamsApi* | [**teams/removeProjectLegacy**](Apis/TeamsApi.http#teams/removeprojectlegacy) | **DELETE** /teams/{team_id}/projects/{project_id} | Remove a project from a team (Legacy)
*TeamsApi* | [**teams/removeRepoInOrg**](Apis/TeamsApi.http#teams/removerepoinorg) | **DELETE** /orgs/{org}/teams/{team_slug}/repos/{owner}/{repo} | Remove a repository from a team
*TeamsApi* | [**teams/removeRepoLegacy**](Apis/TeamsApi.http#teams/removerepolegacy) | **DELETE** /teams/{team_id}/repos/{owner}/{repo} | Remove a repository from a team (Legacy)
*TeamsApi* | [**teams/updateDiscussionCommentInOrg**](Apis/TeamsApi.http#teams/updatediscussioncommentinorg) | **PATCH** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number}/comments/{comment_number} | Update a discussion comment
*TeamsApi* | [**teams/updateDiscussionCommentLegacy**](Apis/TeamsApi.http#teams/updatediscussioncommentlegacy) | **PATCH** /teams/{team_id}/discussions/{discussion_number}/comments/{comment_number} | Update a discussion comment (Legacy)
*TeamsApi* | [**teams/updateDiscussionInOrg**](Apis/TeamsApi.http#teams/updatediscussioninorg) | **PATCH** /orgs/{org}/teams/{team_slug}/discussions/{discussion_number} | Update a discussion
*TeamsApi* | [**teams/updateDiscussionLegacy**](Apis/TeamsApi.http#teams/updatediscussionlegacy) | **PATCH** /teams/{team_id}/discussions/{discussion_number} | Update a discussion (Legacy)
*TeamsApi* | [**teams/updateInOrg**](Apis/TeamsApi.http#teams/updateinorg) | **PATCH** /orgs/{org}/teams/{team_slug} | Update a team
*TeamsApi* | [**teams/updateLegacy**](Apis/TeamsApi.http#teams/updatelegacy) | **PATCH** /teams/{team_id} | Update a team (Legacy)
*UsersApi* | [**users/addEmailForAuthenticatedUser**](Apis/UsersApi.http#users/addemailforauthenticateduser) | **POST** /user/emails | Add an email address for the authenticated user
*UsersApi* | [**users/addSocialAccountForAuthenticatedUser**](Apis/UsersApi.http#users/addsocialaccountforauthenticateduser) | **POST** /user/social_accounts | Add social accounts for the authenticated user
*UsersApi* | [**users/block**](Apis/UsersApi.http#users/block) | **PUT** /user/blocks/{username} | Block a user
*UsersApi* | [**users/checkBlocked**](Apis/UsersApi.http#users/checkblocked) | **GET** /user/blocks/{username} | Check if a user is blocked by the authenticated user
*UsersApi* | [**users/checkFollowingForUser**](Apis/UsersApi.http#users/checkfollowingforuser) | **GET** /users/{username}/following/{target_user} | Check if a user follows another user
*UsersApi* | [**users/checkPersonIsFollowedByAuthenticated**](Apis/UsersApi.http#users/checkpersonisfollowedbyauthenticated) | **GET** /user/following/{username} | Check if a person is followed by the authenticated user
*UsersApi* | [**users/createGpgKeyForAuthenticatedUser**](Apis/UsersApi.http#users/creategpgkeyforauthenticateduser) | **POST** /user/gpg_keys | Create a GPG key for the authenticated user
*UsersApi* | [**users/createPublicSshKeyForAuthenticatedUser**](Apis/UsersApi.http#users/createpublicsshkeyforauthenticateduser) | **POST** /user/keys | Create a public SSH key for the authenticated user
*UsersApi* | [**users/createSshSigningKeyForAuthenticatedUser**](Apis/UsersApi.http#users/createsshsigningkeyforauthenticateduser) | **POST** /user/ssh_signing_keys | Create a SSH signing key for the authenticated user
*UsersApi* | [**users/deleteEmailForAuthenticatedUser**](Apis/UsersApi.http#users/deleteemailforauthenticateduser) | **DELETE** /user/emails | Delete an email address for the authenticated user
*UsersApi* | [**users/deleteGpgKeyForAuthenticatedUser**](Apis/UsersApi.http#users/deletegpgkeyforauthenticateduser) | **DELETE** /user/gpg_keys/{gpg_key_id} | Delete a GPG key for the authenticated user
*UsersApi* | [**users/deletePublicSshKeyForAuthenticatedUser**](Apis/UsersApi.http#users/deletepublicsshkeyforauthenticateduser) | **DELETE** /user/keys/{key_id} | Delete a public SSH key for the authenticated user
*UsersApi* | [**users/deleteSocialAccountForAuthenticatedUser**](Apis/UsersApi.http#users/deletesocialaccountforauthenticateduser) | **DELETE** /user/social_accounts | Delete social accounts for the authenticated user
*UsersApi* | [**users/deleteSshSigningKeyForAuthenticatedUser**](Apis/UsersApi.http#users/deletesshsigningkeyforauthenticateduser) | **DELETE** /user/ssh_signing_keys/{ssh_signing_key_id} | Delete an SSH signing key for the authenticated user
*UsersApi* | [**users/follow**](Apis/UsersApi.http#users/follow) | **PUT** /user/following/{username} | Follow a user
*UsersApi* | [**users/getAuthenticated**](Apis/UsersApi.http#users/getauthenticated) | **GET** /user | Get the authenticated user
*UsersApi* | [**users/getByUsername**](Apis/UsersApi.http#users/getbyusername) | **GET** /users/{username} | Get a user
*UsersApi* | [**users/getContextForUser**](Apis/UsersApi.http#users/getcontextforuser) | **GET** /users/{username}/hovercard | Get contextual information for a user
*UsersApi* | [**users/getGpgKeyForAuthenticatedUser**](Apis/UsersApi.http#users/getgpgkeyforauthenticateduser) | **GET** /user/gpg_keys/{gpg_key_id} | Get a GPG key for the authenticated user
*UsersApi* | [**users/getPublicSshKeyForAuthenticatedUser**](Apis/UsersApi.http#users/getpublicsshkeyforauthenticateduser) | **GET** /user/keys/{key_id} | Get a public SSH key for the authenticated user
*UsersApi* | [**users/getSshSigningKeyForAuthenticatedUser**](Apis/UsersApi.http#users/getsshsigningkeyforauthenticateduser) | **GET** /user/ssh_signing_keys/{ssh_signing_key_id} | Get an SSH signing key for the authenticated user
*UsersApi* | [**users/list**](Apis/UsersApi.http#users/list) | **GET** /users | List users
*UsersApi* | [**users/listBlockedByAuthenticatedUser**](Apis/UsersApi.http#users/listblockedbyauthenticateduser) | **GET** /user/blocks | List users blocked by the authenticated user
*UsersApi* | [**users/listEmailsForAuthenticatedUser**](Apis/UsersApi.http#users/listemailsforauthenticateduser) | **GET** /user/emails | List email addresses for the authenticated user
*UsersApi* | [**users/listFollowedByAuthenticatedUser**](Apis/UsersApi.http#users/listfollowedbyauthenticateduser) | **GET** /user/following | List the people the authenticated user follows
*UsersApi* | [**users/listFollowersForAuthenticatedUser**](Apis/UsersApi.http#users/listfollowersforauthenticateduser) | **GET** /user/followers | List followers of the authenticated user
*UsersApi* | [**users/listFollowersForUser**](Apis/UsersApi.http#users/listfollowersforuser) | **GET** /users/{username}/followers | List followers of a user
*UsersApi* | [**users/listFollowingForUser**](Apis/UsersApi.http#users/listfollowingforuser) | **GET** /users/{username}/following | List the people a user follows
*UsersApi* | [**users/listGpgKeysForAuthenticatedUser**](Apis/UsersApi.http#users/listgpgkeysforauthenticateduser) | **GET** /user/gpg_keys | List GPG keys for the authenticated user
*UsersApi* | [**users/listGpgKeysForUser**](Apis/UsersApi.http#users/listgpgkeysforuser) | **GET** /users/{username}/gpg_keys | List GPG keys for a user
*UsersApi* | [**users/listPublicEmailsForAuthenticatedUser**](Apis/UsersApi.http#users/listpublicemailsforauthenticateduser) | **GET** /user/public_emails | List public email addresses for the authenticated user
*UsersApi* | [**users/listPublicKeysForUser**](Apis/UsersApi.http#users/listpublickeysforuser) | **GET** /users/{username}/keys | List public keys for a user
*UsersApi* | [**users/listPublicSshKeysForAuthenticatedUser**](Apis/UsersApi.http#users/listpublicsshkeysforauthenticateduser) | **GET** /user/keys | List public SSH keys for the authenticated user
*UsersApi* | [**users/listSocialAccountsForAuthenticatedUser**](Apis/UsersApi.http#users/listsocialaccountsforauthenticateduser) | **GET** /user/social_accounts | List social accounts for the authenticated user
*UsersApi* | [**users/listSocialAccountsForUser**](Apis/UsersApi.http#users/listsocialaccountsforuser) | **GET** /users/{username}/social_accounts | List social accounts for a user
*UsersApi* | [**users/listSshSigningKeysForAuthenticatedUser**](Apis/UsersApi.http#users/listsshsigningkeysforauthenticateduser) | **GET** /user/ssh_signing_keys | List SSH signing keys for the authenticated user
*UsersApi* | [**users/listSshSigningKeysForUser**](Apis/UsersApi.http#users/listsshsigningkeysforuser) | **GET** /users/{username}/ssh_signing_keys | List SSH signing keys for a user
*UsersApi* | [**users/setPrimaryEmailVisibilityForAuthenticatedUser**](Apis/UsersApi.http#users/setprimaryemailvisibilityforauthenticateduser) | **PATCH** /user/email/visibility | Set primary email visibility for the authenticated user
*UsersApi* | [**users/unblock**](Apis/UsersApi.http#users/unblock) | **DELETE** /user/blocks/{username} | Unblock a user
*UsersApi* | [**users/unfollow**](Apis/UsersApi.http#users/unfollow) | **DELETE** /user/following/{username} | Unfollow a user
*UsersApi* | [**users/updateAuthenticated**](Apis/UsersApi.http#users/updateauthenticated) | **PATCH** /user | Update the authenticated user


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