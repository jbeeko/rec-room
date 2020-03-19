#!/bin/bash

@echo off
cls

set nuget-key=oy2gbbyumnd3vgf2j2wqshhuuspstb3evb2bhh3waluzey
set github-user=jbeeko
set github-pw=1MixedMess..

dotnet fake build --target Release
