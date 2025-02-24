program PasMD5;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, MD5;

const
  VERSION = '1.0.0';
  HexDigits: array[0..15] of Char = '0123456789abcdef';

type
  TMD5String = string[32];
  
  TPathMode = (
    pmFilenameOnly,    // Only filename
    pmRelativePath,    // Path relative to input directory
    pmFullPath         // Full absolute path
  );

procedure PrintVersion;
begin
  WriteLn('pasmd5 version ', VERSION);
end;

procedure PrintUsage;
begin
  WriteLn('Usage: pasmd5 [OPTIONS]');
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
  WriteLn('  pasmd5 -d /path/to/dir -o hashes.txt -p rel');
  WriteLn('  pasmd5 -f document.pdf');
  WriteLn('  pasmd5 -d . -p full');
end;

function CalculateFileMD5(const AFileName: string): TMD5String;
var
  FileStream: TFileStream;
  MD5Context: TMD5Context;
  Buffer: array[0..16383] of Byte; // 16KB buffer
  BytesRead: Integer;
  DigestResult: TMD5Digest;
  i: Integer;
  HexStr: string;
begin
  Result := '';
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    MD5Init(MD5Context);
    repeat
      BytesRead := FileStream.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        MD5Update(MD5Context, Buffer, BytesRead);
    until BytesRead = 0;
    
    MD5Final(MD5Context, DigestResult);
    
    // Convert digest to hex string
    SetLength(HexStr, 32);
    for i := 0 to 15 do begin
      HexStr[i*2 + 1] := HexDigits[DigestResult[i] shr 4];
      HexStr[i*2 + 2] := HexDigits[DigestResult[i] and $F];
    end;
    Result := HexStr;
  finally
    FileStream.Free;
  end;
end;

function GetPathForOutput(const FullPath, BasePath: string; PathMode: TPathMode): string;
begin
  case PathMode of
    pmFilenameOnly:
      Result := ExtractFileName(FullPath);
    pmRelativePath:
      begin
        if (Length(FullPath) >= Length(BasePath)) and
           (Copy(FullPath, 1, Length(BasePath)) = BasePath) then
          // Remove base path and leading separator if present
          Result := Copy(FullPath, Length(BasePath) + 1, Length(FullPath))
        else
          Result := FullPath;
        // Remove leading path separator if present
        if (Length(Result) > 0) and (Result[1] = PathDelim) then
          Delete(Result, 1, 1);
      end;
    pmFullPath:
      Result := ExpandFileName(FullPath);
  end;
end;

procedure ProcessFile(const FileName, BasePath: string; OutputList: TStringList; 
                     UseConsoleOutput: Boolean; PathMode: TPathMode);
var
  MD5Hash: TMD5String;
  DisplayPath: string;
begin
  try
    MD5Hash := CalculateFileMD5(FileName);
    DisplayPath := GetPathForOutput(FileName, BasePath, PathMode);
    
    if UseConsoleOutput then
      WriteLn(DisplayPath, ' ', MD5Hash)
    else
      OutputList.Add(DisplayPath + ' ' + MD5Hash);
  except
    on E: Exception do
      WriteLn('Warning: Could not process file "', FileName, '": ', E.Message);
  end;
end;

procedure ProcessDirectory(const DirPath, BasePath: string; OutputList: TStringList; 
                         UseConsoleOutput: Boolean; PathMode: TPathMode);
var
  SearchRec: TSearchRec;
begin
  if FindFirst(DirPath + '*', faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and faDirectory) = faDirectory then
            // Recursively process subdirectory
            ProcessDirectory(DirPath + SearchRec.Name + PathDelim, 
                          BasePath, OutputList, UseConsoleOutput, PathMode)
          else
            // Process file
            ProcessFile(DirPath + SearchRec.Name, BasePath, 
                      OutputList, UseConsoleOutput, PathMode);
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

var
  InputDir, InputFile, OutputFile: string;
  OutputList: TStringList;
  UseConsoleOutput: Boolean;
  i: Integer;
  HasInputDir, HasInputFile: Boolean;
  PathMode: TPathMode;
begin
  try
    // Initialize flags
    InputDir := '';
    InputFile := '';
    OutputFile := '';
    HasInputDir := False;
    HasInputFile := False;
    PathMode := pmFilenameOnly; // Default path mode
    
    // Parse command line arguments
    i := 1;
    while i <= ParamCount do
    begin
      case ParamStr(i) of
        '--version': 
        begin
          PrintVersion;
          Exit;
        end;
        '-d': 
        begin
          if i < ParamCount then
          begin
            Inc(i);
            InputDir := ParamStr(i);
            HasInputDir := True;
          end;
        end;
        '-f':
        begin
          if i < ParamCount then
          begin
            Inc(i);
            InputFile := ParamStr(i);
            HasInputFile := True;
          end;
        end;
        '-o':
        begin
          if i < ParamCount then
          begin
            Inc(i);
            OutputFile := ParamStr(i);
          end;
        end;
        '-p':
        begin
          if i < ParamCount then
          begin
            Inc(i);
            case LowerCase(ParamStr(i)) of
              'name': PathMode := pmFilenameOnly;
              'rel':  PathMode := pmRelativePath;
              'full': PathMode := pmFullPath;
            else
              WriteLn('Warning: Unknown path mode "', ParamStr(i), '", using "name"');
            end;
          end;
        end;
      end;
      Inc(i);
    end;

    // Check arguments validity
    if (not HasInputDir) and (not HasInputFile) then
    begin
      PrintUsage;
      ExitCode := 1;
      Exit;
    end;

    if HasInputDir and HasInputFile then
    begin
      WriteLn('Error: Cannot use both -d and -f options simultaneously');
      ExitCode := 1;
      Exit;
    end;

    UseConsoleOutput := OutputFile = '';

    // Create output list if needed
    OutputList := TStringList.Create;
    try
      if HasInputDir then
      begin
        // Process directory
        if not DirectoryExists(InputDir) then
        begin
          WriteLn('Error: Directory "', InputDir, '" does not exist');
          ExitCode := 1;
          Exit;
        end;

        InputDir := ExpandFileName(InputDir);
        InputDir := IncludeTrailingPathDelimiter(InputDir);
        ProcessDirectory(InputDir, InputDir, OutputList, UseConsoleOutput, PathMode);
      end
      else
      begin
        // Process single file
        if not FileExists(InputFile) then
        begin
          WriteLn('Error: File "', InputFile, '" does not exist');
          ExitCode := 1;
          Exit;
        end;
        
        ProcessFile(InputFile, ExtractFilePath(InputFile), 
                   OutputList, UseConsoleOutput, PathMode);
      end;

      // Save to output file if specified
      if not UseConsoleOutput then
      begin
        try
          OutputList.SaveToFile(OutputFile);
        except
          on E: Exception do
          begin
            WriteLn('Error: Could not write to output file "', OutputFile, '": ', E.Message);
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