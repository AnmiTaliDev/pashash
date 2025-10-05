unit HashCalculator;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, MD5;

type
  TMD5String = string[32];

const
  HexDigits: array[0..15] of Char = '0123456789abcdef';

function CalculateFileMD5(const AFileName: string): TMD5String;

implementation

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

end.
