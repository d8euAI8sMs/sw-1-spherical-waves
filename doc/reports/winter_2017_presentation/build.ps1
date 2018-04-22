$ScriptInfo = "MiKTeX pdfLaTeX Build Script v2.0"

$LatexBuildCommandKey = "latex-build-command"
$LatexInputDocKey     = "latex-input-doc"
$LatexWorkDirKey      = "latex-work-dir"

$LatexBuildCommand = $Env:LATEX_BUILD_COMMAND
$LatexInputDoc     = $Env:LATEX_INPUT_DOC
$LatexWorkDir      = $Env:LATEX_WORK_DIR

$FailBuildOnError  = $Env:FAIL_BUILD_ON_ERROR

$InvocationPath = $MyInvocation.MyCommand.Path;

function New-BuildConfiguration{
    param(
        [String]$ConfigFilePath,
        [String]$WorkingDirectory,
        [String]$InputDocument,
        [String]$BuildCommand,
        [String]$Identifier
    )
    [PSCustomObject]@{
        PSTypeName       = "BuildConfiguration"
        Identifier       = $Identifier
        ConfigFilePath   = $ConfigFilePath
        WorkingDirectory = $WorkingDirectory
        InputDocument    = $InputDocument
        BuildCommand     = $BuildCommand
    }
}

function Get-CategoryFromSeverity{
    param([String]$Category)
    Switch($Category)
    {
        "WARN"  { "Warning" }
        "ERROR" { "Error" }
        "INFO"  { "Information" }
    }
}

function Fail-Build{
    If(($FailBuildOnError -ieq "true") -or ($FailBuildOnError -ieq "yes")){
        exit 1
        $true
    } Else {
        $false
    }
}

function Publish-Artifact{
    param(
        [String]$Path,
        [PSTypeName("BuildConfiguration")]$BuildInfo
    )

    $Tmp = Get-Location;

    Set-Location (Get-Item $InvocationPath).Directory.FullName;

    $PubPath = Resolve-Path -Relative $Path;

    Add-LogMessage -Message "Pushing artifacts" -Details "Public artifact path is '$($PubPath)'" -BuildInfo $BuildInfo;

    If(Get-Command Push-AppveyorArtifact -ErrorAction SilentlyContinue){
        Push-AppveyorArtifact "$($PubPath)" -FileName "$($PubPath)";
    }

    Set-Location $Tmp;
}

function Get-ShouldDisturbAppveyor{
    param(
        [switch]$Verbose,
        [String]$Severity
    )
    ((($Severity -eq "WARN") -or ($Severity -eq "ERROR")) -or $Verbose)
}

function Add-LogMessage{
    param(
        [String]$Severity = "INFO",
        [String]$Message,
        [String]$Details,
        [switch]$Verbose = $false,
        [PSTypeName("BuildConfiguration")]$BuildInfo
    )

    Write-Host("[$($ScriptInfo)] $($Severity)[$($BuildInfo.Identifier)]: $($Message)");
    If(-not [String]::IsNullOrWhiteSpace($Details)){
        Write-Host($Details);
    }
    If($Verbose -and ($BuildInfo -ne $null)){
        Write-Host(($BuildInfo | Out-String).Trim());
    }

    If((Get-Command Add-AppveyorMessage -ErrorAction SilentlyContinue) -and (Get-ShouldDisturbAppveyor -Verbose:$Verbose -Severity $Severity)){
        [String]$AppveyorDetails = "";
        If(-not [String]::IsNullOrWhiteSpace($Details)){$AppveyorDetails += $Details + "`n";}
        If($Verbose -and ($BuildInfo -ne $null)){$AppveyorDetails += ($BuildInfo | Out-String).Trim();}
        If([String]::IsNullOrWhiteSpace($AppveyorDetails)){$AppveyorDetails = $null}
        Add-AppveyorMessage -Category (Get-CategoryFromSeverity $Severity) -Message $Message -Details $AppveyorDetails;
    }
}

function Get-FirstMetTexFile{
    param([String]$WorkingDirectory)
    @(Get-ChildItem -Path $WorkingDirectory -Filter "*.tex" -Recurse -ErrorAction SilentlyContinue -Force)[0].FullName
}

function Resolve-BuildConfiguration{
    param(
        [PSTypeName("BuildConfiguration")]$BuildConfiguration
    )

    $ConfigDir = (Get-Item $BuildConfiguration.ConfigFilePath).Directory.FullName;

    If([String]::IsNullOrWhiteSpace($BuildConfiguration.BuildCommand)){
        Add-LogMessage -Message "Try to get build command from environment variable" -BuildInfo $BuildInfo;
        $BuildConfiguration.BuildCommand = $LatexBuildCommand;
        If([String]::IsNullOrWhiteSpace($BuildConfiguration.BuildCommand)){
            Add-LogMessage -Severity "ERROR" -Message "No environment variable specified for the build command" -BuildInfo $BuildInfo;
            If(-not(Fail-Build)){Return}
        }
    }

    If([String]::IsNullOrWhiteSpace($BuildConfiguration.InputDocument)){
        Add-LogMessage -Message "Try to get input document file name from environment variable" -BuildInfo $BuildInfo;
        $BuildConfiguration.InputDocument = $LatexInputDoc;
        If([String]::IsNullOrWhiteSpace($BuildConfiguration.InputDocument)){
            Add-LogMessage -Message "No environment variable specified for the input document file name" -BuildInfo $BuildInfo;
        }
    }
    If(-not([String]::IsNullOrWhiteSpace($BuildConfiguration.InputDocument))){
        $BuildConfiguration.InputDocument = (Get-Item (Join-Path $ConfigDir $BuildConfiguration.InputDocument) -ErrorAction SilentlyContinue).FullName;
        If([String]::IsNullOrWhiteSpace($BuildConfiguration.InputDocument)){
            Add-LogMessage -Severity "ERROR" -Message "The document file does not exist" -BuildInfo $BuildInfo;
            If(-not(Fail-Build)){Return}
        }
    }

    If([String]::IsNullOrWhiteSpace($BuildConfiguration.WorkingDirectory)){
        Add-LogMessage -Message "Try to get working directory name from environment variable" -BuildInfo $BuildInfo;
        $BuildConfiguration.WorkingDirectory = $LatexWorkDir;
        If([String]::IsNullOrWhiteSpace($BuildConfiguration.WorkingDirectory)){
            Add-LogMessage -Message "No environment variable specified for the working directory name" -BuildInfo $BuildInfo;
        }
    }
    If(-not([String]::IsNullOrWhiteSpace($BuildConfiguration.WorkingDirectory))){
        $BuildConfiguration.WorkingDirectory = (Get-Item (Join-Path $ConfigDir $BuildConfiguration.WorkingDirectory) -ErrorAction SilentlyContinue).FullName;
        If([String]::IsNullOrWhiteSpace($BuildConfiguration.WorkingDirectory)){
            Add-LogMessage -Severity "ERROR" -Message "The working directory does not exist" -BuildInfo $BuildInfo;
            If(-not(Fail-Build)){Return}
        }
    }

    If([String]::IsNullOrWhiteSpace($BuildConfiguration.WorkingDirectory)){
        Add-LogMessage -Message "Using the directory of the configuration file as the working directory" -BuildInfo $BuildInfo;
        $BuildConfiguration.WorkingDirectory = $ConfigDir;
    }

    If([String]::IsNullOrWhiteSpace($BuildConfiguration.InputDocument)){
        Add-LogMessage -Message "Searching for any .tex files recursively starting from the configuration file location" -BuildInfo $BuildInfo;
        $BuildConfiguration.InputDocument = Get-FirstMetTexFile $ConfigDir;
        If([String]::IsNullOrWhiteSpace($BuildConfiguration.InputDocument)){
            Add-LogMessage -Severity "ERROR" -Message "No any .tex file found" -BuildInfo $BuildInfo;
            If(-not(Fail-Build)){Return}
        }
    }
    
    Add-LogMessage -Verbose -Message "Using the following configuration" -BuildInfo $BuildInfo;

    $BuildConfiguration
}

function Get-BuildConfiguration{
    param(
        [String]$ConfigurationFile,
        [String]$ConfigurationIdentifier
    )
    $Content = Get-Content -Path $ConfigurationFile;

    $Content | ForEach-Object {
        $m = [Regex]::Match($_, "^\s*$($LatexBuildCommandKey)\s*=\s*(.*)$");
        if ($m.Success) {
            $build_command = $m.Groups[1].Value;
        }
        $m = [Regex]::Match($_, "^\s*$($LatexInputDocKey)\s*=\s*(.*)$");
        if ($m.Success) {
            $build_doc = $m.Groups[1].Value;
        }
        $m = [Regex]::Match($_, "^\s*$($LatexWorkDirKey)\s*=\s*(.*)$");
        if ($m.Success) {
            $build_dir = $m.Groups[1].Value;
        }
    }

    $BuildInfo = New-BuildConfiguration -ConfigFilePath $ConfigurationFile -WorkingDirectory $build_dir -InputDocument $build_doc -BuildCommand $build_command -Identifier $ConfigurationIdentifier;

    Add-LogMessage -Verbose -Message "Processing new build configuration" -BuildInfo $BuildInfo;

    Resolve-BuildConfiguration -BuildConfiguration $BuildInfo
}

function Get-BuildConfigurations{
    [Int32]$Counter = 0;
    $Objects = New-Object System.Collections.ArrayList;
    $Configs = Get-ChildItem -Path ((Get-Item($InvocationPath)).Directory.FullName) -Filter ".latex.build" -Recurse -ErrorAction SilentlyContinue -Force;
    foreach($_ in $Configs){
        $Counter++;
        $Object = Get-BuildConfiguration -ConfigurationFile ($_.FullName) -ConfigurationIdentifier "$($Counter)";
        If($Object -ne $null){
            $Objects.Add($Object) > $null;
        }
    }
    $Objects
}

function Run-Build{
    $Configurations = Get-BuildConfigurations;
    foreach($_ in $Configurations){
        $BuildArtifact = [IO.Path]::ChangeExtension($_.InputDocument, "pdf");
        Add-LogMessage -Verbose -Message "Start building the current configuration" -Details "Expected artifact is '$($BuildArtifact)'" -BuildInfo $_;

        cd "$($_.WorkingDirectory)";
        Invoke-Expression "$($_.BuildCommand) '$($_.InputDocument)'" -WarningVariable $wa;
        Write-Host($?); Write-Host($wa); Write-Host($LASTEXITCODE);

        If((-not($?)) -or (-not(Test-Path($BuildArtifact)))){
            Add-LogMessage -Severity "ERROR" -Message "Expected artifact not found" -BuildInfo $_;
            If(-not(Fail-Build)){Continue}
        }

        #If(-not(Test-Path($BuildArtifact))){
        #    Add-LogMessage -Severity "WARN" -Message "Expected artifact not found, rerun build" -BuildInfo $_;
        #    Invoke-Expression -Command:"$($_.BuildCommand) '$($_.InputDocument)'";
        #    If(-not(Test-Path($BuildArtifact))){
        #        Add-LogMessage -Severity "ERROR" -Message "Expected artifact not found" -BuildInfo $_;
        #        If(-not(Fail-Build)){Continue}
        #    }
        #}

        Publish-Artifact -Path $BuildArtifact -BuildInfo $_
    }
}

Run-Build
