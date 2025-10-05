unit FileProcessor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, HashCalculator, PathUtils;

procedure ProcessFile(const FileName, BasePath: string; OutputList: TStringList;
                     UseConsoleOutput: Boolean; PathMode: TPathMode);
procedure ProcessDirectory(const DirPath, BasePath: string; OutputList: TStringList;
                         UseConsoleOutput: Boolean; PathMode: TPathMode);

implementation

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

end.
