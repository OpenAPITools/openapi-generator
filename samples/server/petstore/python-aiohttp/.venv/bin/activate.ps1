﻿# This file must be dot sourced from PoSh; you cannot run it
# directly. Do this: . ./activate.ps1

# FIXME: clean up unused vars.
$script:THIS_PATH = $myinvocation.mycommand.path
$script:BASE_DIR = split-path (resolve-path "$THIS_PATH/..") -Parent
$script:DIR_NAME = split-path $BASE_DIR -Leaf

function global:deactivate ( [switch] $NonDestructive ){

    if ( test-path variable:_OLD_VIRTUAL_PATH ) {
        $env:PATH = $variable:_OLD_VIRTUAL_PATH
        remove-variable "_OLD_VIRTUAL_PATH" -scope global
    }

    if ( test-path function:_old_virtual_prompt ) {
        $function:prompt = $function:_old_virtual_prompt
        remove-item function:\_old_virtual_prompt
    }

    if ($env:VIRTUAL_ENV) {
        $old_env = split-path $env:VIRTUAL_ENV -leaf
        remove-item env:VIRTUAL_ENV -erroraction silentlycontinue
    }

    if ( !$NonDestructive ) {
        # Self destruct!
        remove-item function:deactivate
    }
}

# unset irrelevant variables
deactivate -nondestructive

$VIRTUAL_ENV = $BASE_DIR
$env:VIRTUAL_ENV = $VIRTUAL_ENV

$global:_OLD_VIRTUAL_PATH = $env:PATH
$env:PATH = "$env:VIRTUAL_ENV/bin:" + $env:PATH
if (! $env:VIRTUAL_ENV_DISABLE_PROMPT) {
    function global:_old_virtual_prompt { "" }
    $function:_old_virtual_prompt = $function:prompt
    function global:prompt {
        # Add a prefix to the current prompt, but don't discard it.
        write-host "($(split-path $env:VIRTUAL_ENV -leaf)) " -nonewline
        & $function:_old_virtual_prompt
    }
}

# SIG # Begin signature block
# MIISeAYJKoZIhvcNAQcCoIISaTCCEmUCAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
# gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
# AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUS5reBwSg3zOUwhXf2jPChZzf
# yPmggg6tMIIGcDCCBFigAwIBAgIBJDANBgkqhkiG9w0BAQUFADB9MQswCQYDVQQG
# EwJJTDEWMBQGA1UEChMNU3RhcnRDb20gTHRkLjErMCkGA1UECxMiU2VjdXJlIERp
# Z2l0YWwgQ2VydGlmaWNhdGUgU2lnbmluZzEpMCcGA1UEAxMgU3RhcnRDb20gQ2Vy
# dGlmaWNhdGlvbiBBdXRob3JpdHkwHhcNMDcxMDI0MjIwMTQ2WhcNMTcxMDI0MjIw
# MTQ2WjCBjDELMAkGA1UEBhMCSUwxFjAUBgNVBAoTDVN0YXJ0Q29tIEx0ZC4xKzAp
# BgNVBAsTIlNlY3VyZSBEaWdpdGFsIENlcnRpZmljYXRlIFNpZ25pbmcxODA2BgNV
# BAMTL1N0YXJ0Q29tIENsYXNzIDIgUHJpbWFyeSBJbnRlcm1lZGlhdGUgT2JqZWN0
# IENBMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyiOLIjUemqAbPJ1J
# 0D8MlzgWKbr4fYlbRVjvhHDtfhFN6RQxq0PjTQxRgWzwFQNKJCdU5ftKoM5N4YSj
# Id6ZNavcSa6/McVnhDAQm+8H3HWoD030NVOxbjgD/Ih3HaV3/z9159nnvyxQEckR
# ZfpJB2Kfk6aHqW3JnSvRe+XVZSufDVCe/vtxGSEwKCaNrsLc9pboUoYIC3oyzWoU
# TZ65+c0H4paR8c8eK/mC914mBo6N0dQ512/bkSdaeY9YaQpGtW/h/W/FkbQRT3sC
# pttLVlIjnkuY4r9+zvqhToPjxcfDYEf+XD8VGkAqle8Aa8hQ+M1qGdQjAye8OzbV
# uUOw7wIDAQABo4IB6TCCAeUwDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMC
# AQYwHQYDVR0OBBYEFNBOD0CZbLhLGW87KLjg44gHNKq3MB8GA1UdIwQYMBaAFE4L
# 7xqkQFulF2mHMMo0aEPQQa7yMD0GCCsGAQUFBwEBBDEwLzAtBggrBgEFBQcwAoYh
# aHR0cDovL3d3dy5zdGFydHNzbC5jb20vc2ZzY2EuY3J0MFsGA1UdHwRUMFIwJ6Al
# oCOGIWh0dHA6Ly93d3cuc3RhcnRzc2wuY29tL3Nmc2NhLmNybDAnoCWgI4YhaHR0
# cDovL2NybC5zdGFydHNzbC5jb20vc2ZzY2EuY3JsMIGABgNVHSAEeTB3MHUGCysG
# AQQBgbU3AQIBMGYwLgYIKwYBBQUHAgEWImh0dHA6Ly93d3cuc3RhcnRzc2wuY29t
# L3BvbGljeS5wZGYwNAYIKwYBBQUHAgEWKGh0dHA6Ly93d3cuc3RhcnRzc2wuY29t
# L2ludGVybWVkaWF0ZS5wZGYwEQYJYIZIAYb4QgEBBAQDAgABMFAGCWCGSAGG+EIB
# DQRDFkFTdGFydENvbSBDbGFzcyAyIFByaW1hcnkgSW50ZXJtZWRpYXRlIE9iamVj
# dCBTaWduaW5nIENlcnRpZmljYXRlczANBgkqhkiG9w0BAQUFAAOCAgEAcnMLA3Va
# N4OIE9l4QT5OEtZy5PByBit3oHiqQpgVEQo7DHRsjXD5H/IyTivpMikaaeRxIv95
# baRd4hoUcMwDj4JIjC3WA9FoNFV31SMljEZa66G8RQECdMSSufgfDYu1XQ+cUKxh
# D3EtLGGcFGjjML7EQv2Iol741rEsycXwIXcryxeiMbU2TPi7X3elbwQMc4JFlJ4B
# y9FhBzuZB1DV2sN2irGVbC3G/1+S2doPDjL1CaElwRa/T0qkq2vvPxUgryAoCppU
# FKViw5yoGYC+z1GaesWWiP1eFKAL0wI7IgSvLzU3y1Vp7vsYaxOVBqZtebFTWRHt
# XjCsFrrQBngt0d33QbQRI5mwgzEp7XJ9xu5d6RVWM4TPRUsd+DDZpBHm9mszvi9g
# VFb2ZG7qRRXCSqys4+u/NLBPbXi/m/lU00cODQTlC/euwjk9HQtRrXQ/zqsBJS6U
# J+eLGw1qOfj+HVBl/ZQpfoLk7IoWlRQvRL1s7oirEaqPZUIWY/grXq9r6jDKAp3L
# ZdKQpPOnnogtqlU4f7/kLjEJhrrc98mrOWmVMK/BuFRAfQ5oDUMnVmCzAzLMjKfG
# cVW/iMew41yfhgKbwpfzm3LBr1Zv+pEBgcgW6onRLSAn3XHM0eNtz+AkxH6rRf6B
# 2mYhLEEGLapH8R1AMAo4BbVFOZR5kXcMCwowggg1MIIHHaADAgECAgIEuDANBgkq
# hkiG9w0BAQUFADCBjDELMAkGA1UEBhMCSUwxFjAUBgNVBAoTDVN0YXJ0Q29tIEx0
# ZC4xKzApBgNVBAsTIlNlY3VyZSBEaWdpdGFsIENlcnRpZmljYXRlIFNpZ25pbmcx
# ODA2BgNVBAMTL1N0YXJ0Q29tIENsYXNzIDIgUHJpbWFyeSBJbnRlcm1lZGlhdGUg
# T2JqZWN0IENBMB4XDTExMTIwMzE1MzQxOVoXDTEzMTIwMzE0NTgwN1owgYwxIDAe
# BgNVBA0TFzU4MTc5Ni1HaDd4Zkp4a3hRU0lPNEUwMQswCQYDVQQGEwJERTEPMA0G
# A1UECBMGQmVybGluMQ8wDQYDVQQHEwZCZXJsaW4xFjAUBgNVBAMTDUphbm5pcyBM
# ZWlkZWwxITAfBgkqhkiG9w0BCQEWEmphbm5pc0BsZWlkZWwuaW5mbzCCAiIwDQYJ
# KoZIhvcNAQEBBQADggIPADCCAgoCggIBAMcPeABYdN7nPq/AkZ/EkyUBGx/l2Yui
# Lfm8ZdLG0ulMb/kQL3fRY7sUjYPyn9S6PhqqlFnNoGHJvbbReCdUC9SIQYmOEjEA
# raHfb7MZU10NjO4U2DdGucj2zuO5tYxKizizOJF0e4yRQZVxpUGdvkW/+GLjCNK5
# L7mIv3Z1dagxDKHYZT74HXiS4VFUwHF1k36CwfM2vsetdm46bdgSwV+BCMmZICYT
# IJAS9UQHD7kP4rik3bFWjUx08NtYYFAVOd/HwBnemUmJe4j3IhZHr0k1+eDG8hDH
# KVvPgLJIoEjC4iMFk5GWsg5z2ngk0LLu3JZMtckHsnnmBPHQK8a3opUNd8hdMNJx
# gOwKjQt2JZSGUdIEFCKVDqj0FmdnDMPfwy+FNRtpBMl1sz78dUFhSrnM0D8NXrqa
# 4rG+2FoOXlmm1rb6AFtpjAKksHRpYcPk2DPGWp/1sWB+dUQkS3gOmwFzyqeTuXpT
# 0juqd3iAxOGx1VRFQ1VHLLf3AzV4wljBau26I+tu7iXxesVucSdsdQu293jwc2kN
# xK2JyHCoZH+RyytrwS0qw8t7rMOukU9gwP8mn3X6mgWlVUODMcHTULjSiCEtvyZ/
# aafcwjUbt4ReEcnmuZtWIha86MTCX7U7e+cnpWG4sIHPnvVTaz9rm8RyBkIxtFCB
# nQ3FnoQgyxeJAgMBAAGjggOdMIIDmTAJBgNVHRMEAjAAMA4GA1UdDwEB/wQEAwIH
# gDAuBgNVHSUBAf8EJDAiBggrBgEFBQcDAwYKKwYBBAGCNwIBFQYKKwYBBAGCNwoD
# DTAdBgNVHQ4EFgQUWyCgrIWo8Ifvvm1/YTQIeMU9nc8wHwYDVR0jBBgwFoAU0E4P
# QJlsuEsZbzsouODjiAc0qrcwggIhBgNVHSAEggIYMIICFDCCAhAGCysGAQQBgbU3
# AQICMIIB/zAuBggrBgEFBQcCARYiaHR0cDovL3d3dy5zdGFydHNzbC5jb20vcG9s
# aWN5LnBkZjA0BggrBgEFBQcCARYoaHR0cDovL3d3dy5zdGFydHNzbC5jb20vaW50
# ZXJtZWRpYXRlLnBkZjCB9wYIKwYBBQUHAgIwgeowJxYgU3RhcnRDb20gQ2VydGlm
# aWNhdGlvbiBBdXRob3JpdHkwAwIBARqBvlRoaXMgY2VydGlmaWNhdGUgd2FzIGlz
# c3VlZCBhY2NvcmRpbmcgdG8gdGhlIENsYXNzIDIgVmFsaWRhdGlvbiByZXF1aXJl
# bWVudHMgb2YgdGhlIFN0YXJ0Q29tIENBIHBvbGljeSwgcmVsaWFuY2Ugb25seSBm
# b3IgdGhlIGludGVuZGVkIHB1cnBvc2UgaW4gY29tcGxpYW5jZSBvZiB0aGUgcmVs
# eWluZyBwYXJ0eSBvYmxpZ2F0aW9ucy4wgZwGCCsGAQUFBwICMIGPMCcWIFN0YXJ0
# Q29tIENlcnRpZmljYXRpb24gQXV0aG9yaXR5MAMCAQIaZExpYWJpbGl0eSBhbmQg
# d2FycmFudGllcyBhcmUgbGltaXRlZCEgU2VlIHNlY3Rpb24gIkxlZ2FsIGFuZCBM
# aW1pdGF0aW9ucyIgb2YgdGhlIFN0YXJ0Q29tIENBIHBvbGljeS4wNgYDVR0fBC8w
# LTAroCmgJ4YlaHR0cDovL2NybC5zdGFydHNzbC5jb20vY3J0YzItY3JsLmNybDCB
# iQYIKwYBBQUHAQEEfTB7MDcGCCsGAQUFBzABhitodHRwOi8vb2NzcC5zdGFydHNz
# bC5jb20vc3ViL2NsYXNzMi9jb2RlL2NhMEAGCCsGAQUFBzAChjRodHRwOi8vYWlh
# LnN0YXJ0c3NsLmNvbS9jZXJ0cy9zdWIuY2xhc3MyLmNvZGUuY2EuY3J0MCMGA1Ud
# EgQcMBqGGGh0dHA6Ly93d3cuc3RhcnRzc2wuY29tLzANBgkqhkiG9w0BAQUFAAOC
# AQEAhrzEV6zwoEtKjnFRhCsjwiPykVpo5Eiye77Ve801rQDiRKgSCCiW6g3HqedL
# OtaSs65Sj2pm3Viea4KR0TECLcbCTgsdaHqw2x1yXwWBQWZEaV6EB05lIwfr94P1
# SFpV43zkuc+bbmA3+CRK45LOcCNH5Tqq7VGTCAK5iM7tvHwFlbQRl+I6VEL2mjpF
# NsuRjDOVrv/9qw/a22YJ9R7Y1D0vUSs3IqZx2KMUaYDP7H2mSRxJO2nADQZBtriF
# gTyfD3lYV12MlIi5CQwe3QC6DrrfSMP33i5Wa/OFJiQ27WPxmScYVhiqozpImFT4
# PU9goiBv9RKXdgTmZE1PN0NQ5jGCAzUwggMxAgEBMIGTMIGMMQswCQYDVQQGEwJJ
# TDEWMBQGA1UEChMNU3RhcnRDb20gTHRkLjErMCkGA1UECxMiU2VjdXJlIERpZ2l0
# YWwgQ2VydGlmaWNhdGUgU2lnbmluZzE4MDYGA1UEAxMvU3RhcnRDb20gQ2xhc3Mg
# MiBQcmltYXJ5IEludGVybWVkaWF0ZSBPYmplY3QgQ0ECAgS4MAkGBSsOAwIaBQCg
# eDAYBgorBgEEAYI3AgEMMQowCKACgAChAoAAMBkGCSqGSIb3DQEJAzEMBgorBgEE
# AYI3AgEEMBwGCisGAQQBgjcCAQsxDjAMBgorBgEEAYI3AgEVMCMGCSqGSIb3DQEJ
# BDEWBBRVGw0FDSiaIi38dWteRUAg/9Pr6DANBgkqhkiG9w0BAQEFAASCAgCInvOZ
# FdaNFzbf6trmFDZKMojyx3UjKMCqNjHVBbuKY0qXwFC/ElYDV1ShJ2CBZbdurydO
# OQ6cIQ0KREOCwmX/xB49IlLHHUxNhEkVv7HGU3EKAFf9IBt9Yr7jikiR9cjIsfHK
# 4cjkoKJL7g28yEpLLkHt1eo37f1Ga9lDWEa5Zq3U5yX+IwXhrUBm1h8Xr033FhTR
# VEpuSz6LHtbrL/zgJnCzJ2ahjtJoYevdcWiNXffosJHFaSfYDDbiNsPRDH/1avmb
# 5j/7BhP8BcBaR6Fp8tFbNGIcWHHGcjqLMnTc4w13b7b4pDhypqElBa4+lCmwdvv9
# GydYtRgPz8GHeoBoKj30YBlMzRIfFYaIFGIC4Ai3UEXkuH9TxYohVbGm/W0Kl4Lb
# RJ1FwiVcLcTOJdgNId2vQvKc+jtNrjcg5SP9h2v/C4aTx8tyc6tE3TOPh2f9b8DL
# S+SbVArJpuJqrPTxDDoO1QNjTgLcdVYeZDE+r/NjaGZ6cMSd8db3EaG3ijD/0bud
# SItbm/OlNVbQOFRR76D+ZNgPcU5iNZ3bmvQQIg6aSB9MHUpIE/SeCkNl9YeVk1/1
# GFULgNMRmIYP4KLvu9ylh5Gu3hvD5VNhH6+FlXANwFy07uXks5uF8mfZVxVCnodG
# xkNCx+6PsrA5Z7WP4pXcmYnMn97npP/Q9EHJWw==
# SIG # End signature block
