program PasHash;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, HashCalculator, PathUtils, FileProcessor, CLIParser;

var
  Config: TAppConfig;
  OutputList: TStringList;
  UseConsoleOutput: Boolean;
  InputDir: string;
begin
  try
    // Parse command line arguments
    Config := ParseCommandLine;

    // Validate configuration
    if not ValidateConfig(Config) then
    begin
      ExitCode := 1;
      Exit;
    end;

    UseConsoleOutput := Config.OutputFile = '';

    // Create output list if needed
    OutputList := TStringList.Create;
    try
      if Config.HasInputDir then
      begin
        // Process directory
        InputDir := ExpandFileName(Config.InputDir);
        InputDir := IncludeTrailingPathDelimiter(InputDir);
        ProcessDirectory(InputDir, InputDir, OutputList, UseConsoleOutput, Config.PathMode);
      end
      else
      begin
        // Process single file
        ProcessFile(Config.InputFile, ExtractFilePath(Config.InputFile),
                   OutputList, UseConsoleOutput, Config.PathMode);
      end;

      // Save to output file if specified
      if not UseConsoleOutput then
      begin
        try
          OutputList.SaveToFile(Config.OutputFile);
        except
          on E: Exception do
          begin
            WriteLn('Error: Could not write to output file "', Config.OutputFile, '": ', E.Message);
            ExitCode := 1;
            Exit;
          end;
        end;
      end;

    finally
      OutputList.Free;
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.