program PasMD5;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, MD5;

const
  VERSION = '1.0.0';
  HexDigits: array[0..15] of Char = '0123456789abcdef';

type
  TMD5String = string[32];

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
  WriteLn('  -d <dir>      Process all files in directory');
  WriteLn('  -f <file>     Process single file');
  WriteLn('  -o <file>     Output file (if omitted, prints to console)');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  pasmd5 -d /path/to/dir -o hashes.txt');
  WriteLn('  pasmd5 -f document.pdf');
  WriteLn('  pasmd5 -d . ');
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

procedure ProcessFile(const FileName: string; OutputList: TStringList; UseConsoleOutput: Boolean);
var
  MD5Hash: TMD5String;
  BaseName: string;
begin
  try
    MD5Hash := CalculateFileMD5(FileName);
    BaseName := ExtractFileName(FileName);
    
    if UseConsoleOutput then
      WriteLn(BaseName, ' ', MD5Hash)
    else
      OutputList.Add(BaseName + ' ' + MD5Hash);
  except
    on E: Exception do
      WriteLn('Warning: Could not process file "', FileName, '": ', E.Message);
  end;
end;

var
  InputDir, InputFile, OutputFile: string;
  SearchRec: TSearchRec;
  OutputList: TStringList;
  UseConsoleOutput: Boolean;
  i: Integer;
  HasInputDir, HasInputFile: Boolean;
begin
  try
    // Initialize flags
    InputDir := '';
    InputFile := '';
    OutputFile := '';
    HasInputDir := False;
    HasInputFile := False;
    
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

        InputDir := IncludeTrailingPathDelimiter(InputDir);
        if FindFirst(InputDir + '*', faAnyFile - faDirectory, SearchRec) = 0 then
        begin
          repeat
            ProcessFile(InputDir + SearchRec.Name, OutputList, UseConsoleOutput);
          until FindNext(SearchRec) <> 0;
          FindClose(SearchRec);
        end;
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
        
        ProcessFile(InputFile, OutputList, UseConsoleOutput);
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