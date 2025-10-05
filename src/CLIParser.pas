unit CLIParser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, PathUtils;

const
  VERSION = '1.0.0';

type
  TAppConfig = record
    InputDir: string;
    InputFile: string;
    OutputFile: string;
    HasInputDir: Boolean;
    HasInputFile: Boolean;
    PathMode: TPathMode;
  end;

procedure PrintVersion;
procedure PrintUsage;
function ParseCommandLine: TAppConfig;
function ValidateConfig(const Config: TAppConfig): Boolean;

implementation

procedure PrintVersion;
begin
  WriteLn('pashash version ', VERSION);
end;

procedure PrintUsage;
begin
  WriteLn('Usage: pashash [OPTIONS]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --version      Show version information');
  WriteLn('  -d <dir>      Process files in directory (recursively)');
  WriteLn('  -f <file>     Process single file');
  WriteLn('  -o <file>     Output file (if omitted, prints to console)');
  WriteLn('  -p <mode>     Path mode in output (default: name)');
  WriteLn('                 name   - filename only');
  WriteLn('                 rel    - relative path from input directory');
  WriteLn('                 full   - full absolute path');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  pashash -d /path/to/dir -o hashes.txt -p rel');
  WriteLn('  pashash -f document.pdf');
  WriteLn('  pashash -d . -p full');
end;

function ParseCommandLine: TAppConfig;
var
  i: Integer;
begin
  // Initialize config
  Result.InputDir := '';
  Result.InputFile := '';
  Result.OutputFile := '';
  Result.HasInputDir := False;
  Result.HasInputFile := False;
  Result.PathMode := pmFilenameOnly; // Default path mode

  // Parse command line arguments
  i := 1;
  while i <= ParamCount do
  begin
    case ParamStr(i) of
      '--version':
      begin
        PrintVersion;
        Halt(0);
      end;
      '-d':
      begin
        if i < ParamCount then
        begin
          Inc(i);
          Result.InputDir := ParamStr(i);
          Result.HasInputDir := True;
        end;
      end;
      '-f':
      begin
        if i < ParamCount then
        begin
          Inc(i);
          Result.InputFile := ParamStr(i);
          Result.HasInputFile := True;
        end;
      end;
      '-o':
      begin
        if i < ParamCount then
        begin
          Inc(i);
          Result.OutputFile := ParamStr(i);
        end;
      end;
      '-p':
      begin
        if i < ParamCount then
        begin
          Inc(i);
          Result.PathMode := ParsePathMode(ParamStr(i));
        end;
      end;
    end;
    Inc(i);
  end;
end;

function ValidateConfig(const Config: TAppConfig): Boolean;
begin
  Result := True;

  // Check arguments validity
  if (not Config.HasInputDir) and (not Config.HasInputFile) then
  begin
    PrintUsage;
    Result := False;
    Exit;
  end;

  if Config.HasInputDir and Config.HasInputFile then
  begin
    WriteLn('Error: Cannot use both -d and -f options simultaneously');
    Result := False;
    Exit;
  end;

  if Config.HasInputDir and (not DirectoryExists(Config.InputDir)) then
  begin
    WriteLn('Error: Directory "', Config.InputDir, '" does not exist');
    Result := False;
    Exit;
  end;

  if Config.HasInputFile and (not FileExists(Config.InputFile)) then
  begin
    WriteLn('Error: File "', Config.InputFile, '" does not exist');
    Result := False;
    Exit;
  end;
end;

end.
