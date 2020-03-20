#!/bin/bash

set nuget-key=$NUGET_KEY
set github-user=$GH_USERNAME
set github-pw=$GH_ACCESS_TOKEN

dotnet fake -v build --target Release
