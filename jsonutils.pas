{
  The file is part of docusign command-line utility.
  Please refer to LICENSE file for the use of the source codes.
}
unit jsonutils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

function GetJsonStr(jd: TJSONData): string;

const
  JsonNonStringsTypes = [jtUnknown, jtNull, jtObject, jtArray];

implementation

function GetJsonStr(jd: TJSONData): string;
begin
  Result:='';
  if not Assigned(jd) then Exit;
  if jd.JSONType in JsonNonStringsTypes then begin
    Result:=''
  end else
    Result:=jd.AsString;
end;

end.

