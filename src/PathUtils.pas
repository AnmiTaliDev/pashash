unit PathUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TPathMode = (
    pmFilenameOnly,    // Only filename
    pmRelativePath,    // Path relative to input directory
    pmFullPath         // Full absolute path
  );

function GetPathForOutput(const FullPath, BasePath: string; PathMode: TPathMode): string;
function ParsePathMode(const ModeStr: string): TPathMode;

implementation

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

function ParsePathMode(const ModeStr: string): TPathMode;
begin
  case LowerCase(ModeStr) of
    'name': Result := pmFilenameOnly;
    'rel':  Result := pmRelativePath;
    'full': Result := pmFullPath;
  else
    WriteLn('Warning: Unknown path mode "', ModeStr, '", using "name"');
    Result := pmFilenameOnly;
  end;
end;

end.
