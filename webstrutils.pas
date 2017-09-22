{
  The file is part of docusign command-line utility.
  Please refer to LICENSE file for the user of the source codes.
}

unit webstrutils;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

function JsonStr(const s: string): string;
const
  JsonBoolStr : array [boolean] of string = ('false','true');

function AddTrailingChar(const s: string; const trchar: char): string;
function RemoveTrailingChar(const s: string; const trchar: char): string;

function AddTrailingSlash(const s: string): string;
function RemoveTrailingSlash(const s: string): string;

function RemoveTrailingAmp(const s: string): string;

function encodeURIComponent(const inp: string): string;

type

  { TJsonWriter }

  TJsonWriter = class(TObject)
  private
    bufidx: Integer;
    needseparator: Boolean;
  protected
    procedure AdjusSizeFor(addsize: integer);
    procedure WriteAVal(const nm, vl: string; isStrValue: Boolean);
  public
    buffer: string;
    constructor Create;
    procedure StrVal(const nm, vl: string);
    procedure StrValNotEmpty(const nm, vl: string);
    procedure AnyVal(const nm, vl: string);
    procedure AnyValNotEmpty(const nm, vl: string);
    procedure BoolVal(const nm: string; abool: Boolean);

    procedure OpenArray(const nm: string);
    procedure CloseArray;
    procedure OpenObject(const nm: string);
    procedure CloseObject;

    function Finish: string;
    procedure Reset;
  end;

procedure JsonWriterFloat(jw: TJsonWriter; const nm: string; const value: double; const fmt: string = '%0.2f');
procedure JsonWriterInt(jw: TJsonWriter; const nm: string; int: Integer);

implementation

procedure JsonWriterFloat(jw: TJsonWriter; const nm: string; const value: double; const fmt: string);
begin
  if not Assigned(jw) then Exit;
  jw.AnyVal(nm, Format(fmt, [value]));
end;

procedure JsonWriterInt(jw: TJsonWriter; const nm: string; int: Integer);
begin
  if not Assigned(jw) then Exit;
  jw.AnyVal(nm, IntToStr(int));
end;

function RemoveTrailingAmp(const s: string): string;
begin
  Result:=RemoveTrailingChar(s, '&');
end;

function AddTrailingChar(const s: string; const trchar: char): string;
begin
  if (s<>'') and (s[length(s)]<>trchar) then Result:=s+trchar
  else Result:=s;
end;

function RemoveTrailingChar(const s: string; const trchar: char): string;
begin
  if (s<>'') and (s[length(s)]=trchar) then Result:=Copy(s,1,length(s)-1)
  else Result:=s
end;

function AddTrailingSlash(const s: string): string;
begin
  Result:=AddTrailingChar(s, '/');
end;

function RemoveTrailingSlash(const s: string): string;
begin
  Result:=RemoveTrailingChar(s, '/');
end;

function encodeURIComponent(const inp: string): string;
var
  i, k: Integer;
  s: string;
  b: byte;
  c: char;
const
  NonURLChars: set of Char =
  [#$00..#$20, ';', '/', '?', ':', '@', '=', '&', '#', '+'
    , '_', '<', '>', '"', '%', '{', '}', '|', '\', '^', '~', '[', ']','`', #$7F..#$FF];
  Prefix = '%';
  HexChar : array [0..$F] of char =('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
begin
  SetLength(s, Length(inp) * 3);
  k := 1;
  for i := 1 to length(inp) do begin
    c := inp[i];
    if c in NonURLChars then begin
      s[k] := Prefix;
      inc(k);
      b:=Ord(c);
      s[k] := HexChar[ (b shr 4) and $F ];
      inc(k);
      s[k] := HexChar[ b and $F ];
      inc(k);
    end else begin
      s[k] := c;
      inc(k);
    end;
  end;
  Dec(k);
  SetLength(s, k);
  Result:=s;
end;


function JsonStr(const s: string): string;
var
  t : string;
  i : integer;
  ch : array [char] of string;
  j  : integer;
begin
  for i:=0 to 255 do ch[chr(i)]:=chr(i);
  ch[#10]:='\n';
  ch[#13]:='\r';
  ch['\']:='\\';
  //ch[#39]:='\''';
  ch['"']:='\"';
  SetLength(t, length(s)*3);
  j:=1;
  for i:=1 to length(s) do begin
    if length(ch[ s[i] ])=1 then
      t[j]:=s[i]
    else begin
      t[j]:='\'; inc(j);
      t[j]:=ch[s[i]][2];
    end;
    inc(j);
  end;
  SetLength(t, j-1);
  Result:=t;
end;

{ TJsonWriter }

procedure TJsonWriter.AdjusSizeFor(addsize: integer);
var
  ln : integer;
  needsz:  integer;
begin
  ln:=length(buffer);
  needsz:=bufidx+addsize;
  if (ln<needsz) then begin
    if ln=0 then ln:=4;
    while ln<needsz do ln:=ln*2;
    SetLength(buffer, ln);
  end;
end;

procedure TJsonWriter.WriteAVal(const nm, vl: string; isStrValue: Boolean);
var
  addsize: Integer;
  i : integer;
  hasName : Boolean;
const
  ExtraSizeName  = 3; // "":
  ExtraSizeStr   = 2; // ''
begin
  hasName:=length(nm)>0;
  if hasName
    then addsize:=ExtraSizeName+length(nm)
    else addsize:=0;

  if needseparator then inc(addsize);

  if isStrValue
    then addsize:=addsize+ExtraSizeStr+length(vl)*2 // possible json encoding
    else addsize:=addsize+length(vl);

  AdjusSizeFor(addsize);
  if needseparator then begin
    buffer[bufidx]:=',';
    inc(bufidx);
  end;

  if hasName then begin
    buffer[bufidx]:='"'; inc(bufidx);
    Move(nm[1], buffer[bufidx], length(nm));
    inc(bufidx, length(nm));
    buffer[bufidx]:='"'; inc(bufidx);
    buffer[bufidx]:=':'; inc(bufidx);
  end;

  if isStrValue then begin
    buffer[bufidx]:='"'; inc(bufidx);
    for i:=1 to length(vl) do begin
      if vl[i] in [#10,#13,'"'] then begin
        buffer[bufidx]:='\';
        inc(bufidx);
        case vl[i] of
          #10: buffer[bufidx]:='n';
          #13: buffer[bufidx]:='r';
          '"': buffer[bufidx]:='"';
        end;
        inc(bufidx);
      end else begin
        buffer[bufidx]:=vl[i];
        inc(bufidx);
      end;
    end;
    buffer[bufidx]:='"'; inc(bufidx);
  end else begin
    Move(vl[1], buffer[bufidx], length(vl));
    inc(bufidx, length(vl));
  end;
  needseparator:=true;
end;

constructor TJsonWriter.Create;
begin
  inherited Create;
  Reset;
end;

procedure TJsonWriter.StrVal(const nm, vl: string);
begin
  WriteAVal(nm, vl, true)
end;

procedure TJsonWriter.StrValNotEmpty(const nm, vl: string);
begin
  if vl='' then Exit;
  WriteAVal(nm, vl, true);
end;

procedure TJsonWriter.AnyVal(const nm, vl: string);
begin
  WriteAVal(nm, vl, false);
end;

procedure TJsonWriter.AnyValNotEmpty(const nm, vl: string);
begin
  if vl='' then Exit;
  WriteAVal(nm, vl, false);
end;

procedure TJsonWriter.BoolVal(const nm: string; abool: Boolean);
begin
  WriteAVal(nm, JsonBoolStr[abool], false);
end;

procedure TJsonWriter.OpenArray(const nm: string);
begin
  WriteAVal(nm, '[', false);
  needseparator:=false;
end;

procedure TJsonWriter.CloseArray;
begin
  AdjusSizeFor(1);
  buffer[bufidx]:=']'; inc(bufidx);
end;

procedure TJsonWriter.OpenObject(const nm: string);
begin
  WriteAVal(nm, '{', false);
  needseparator:=false;
end;

procedure TJsonWriter.CloseObject;
begin
  AdjusSizeFor(1);
  buffer[bufidx]:='}';
  inc(bufidx);
end;

function TJsonWriter.Finish: string;
begin
  SetLength(buffer, bufidx-1);
  Result:=buffer;
end;

procedure TJsonWriter.Reset;
begin
  buffer:='';
  bufidx:=1;
end;


end.

