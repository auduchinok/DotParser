@echo off
cls

dotnet build DotParser.sln /P:Configuration=Release
dotnet test tests\DotParser.Tests\DotParser.Tests.fsproj /P:Configuration=Release