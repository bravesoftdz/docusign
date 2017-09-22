{
  The file is part of docusign command-line utility.
  Please refer to LICENSE file for the use of the source codes.
}
unit httpconnsynapse;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, docusignrest, HttpSend, ssl_openssl, ssl_openssl_lib
  , synautil, synsock;

type

  { THTTPConnSynapse }

  THTTPConnSynapse = class(THTTPConn)
  private
    fLogWrite: Boolean;
    fparts : TList;
    procedure ClearParts;
    procedure Monitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPart(s: TStream; OwnStream: Boolean; const atype, adisposition, extrahdr: string); override;
    function Http(const url, method, extrahdr{, input}: string;
      var AResult: string; var errCode: Integer): Boolean; override;
    function GetSendingData: string; override;
    property LogWrite: Boolean read fLogWrite write fLogWrite;
  end;

function GenerateBoundary: string;

implementation

type

  { THTTPDataStream }

  THTTPDataStream = class(TObject)
  public
    data : TStream;
    own  : Boolean;
    mimetype : string;
    disp     : string;
    extrahdr : string;
    constructor Create(adata: TStream; aown: Boolean; const atype, adisposition, aextrahdr: string);
    destructor Destroy; override;
  end;


function GenerateBoundary: string;
begin
  Result:=IntToHex(round(random*MaxInt), 8)
          +'-'+IntToHex(round(random*MaXInt),8)
          +'-'+IntToHex(round(random*MaXInt),8)
          +'-'+IntToHex(round(random*MaXInt),8);
end;

{ THTTPDataStream }

constructor THTTPDataStream.Create(adata: TStream; aown: Boolean; const atype, adisposition, aextrahdr: string);
begin
  inherited Create;
  data:=adata;
  own:=aown;
  mimetype := atype;
  disp     := adisposition;
  extrahdr := aextrahdr;
end;

destructor THTTPDataStream.Destroy;
begin
  if own then data.Free;
  inherited Destroy;
end;

{ THTTPConnSynapse }

procedure THTTPConnSynapse.ClearParts;
var
  i : Integer;
begin
  for i:=0 to fparts.Count-1 do  TObjecT(fparts[i]).Free;
  fparts.Clear;

end;

procedure THTTPConnSynapse.Monitor(Sender: TObject; Writing: Boolean;
  const Buffer: TMemory; Len: Integer);
var
  s : string;
begin
  if Writing and fLogWrite then begin
    if Len>0 then begin
      SetLength(s, Len);
      Move(Buffer^, s[1], len);
      write(s);
    end;
  end;
end;

constructor THTTPConnSynapse.Create();
begin
  inherited Create;
  fparts := TList.Create;
end;

destructor THTTPConnSynapse.Destroy;
begin
  fparts.Free;
  inherited Destroy;
end;

procedure THTTPConnSynapse.AddPart(s: TStream; OwnStream: Boolean; const atype, adisposition, extrahdr: string);
begin
  fparts.Add( THTTPDataStream.Create(s, OwnStream, atype, adisposition, extrahdr) );
end;

function THTTPConnSynapse.Http(const url, method, extrahdr{, input}: string;
  var AResult: string; var errCode: Integer): Boolean;
var
  m     : string;
  http  : THTTPSend;
  h     : TStringList;
  i,j   : Integer;
  hs    : string;
  bnd   : string;
  dt    : THTTPDataStream;
  s     : string;
const
  CRLF = #13#10;
begin
  m:=AnsiUpperCase(method);
  Result:=(m='GET') or (m='POST') or (m='PUT');
  if not Result then Exit;

  errCode:=200;
  http := THTTPSend.Create;
  try
    if extrahdr<>'' then http.Headers.Add(extrahdr);

    if fParts.Count=1 then begin
      dt:=THTTPDataStream(fparts[0]);
      http.MimeType:=dt.mimetype;
      http.Headers.Add(dt.extrahdr);
      if Assigned(dt.data) and (dt.Data.Size>0) then begin
        dt.Data.Position:=0;
        http.Document.CopyFrom(dt.Data, dt.data.Size);
      end;
    end else if fParts.Count>1 then begin
      //todo: boundries!
      bnd:=GenerateBoundary;
      http.MimeType:='multipart/form-data; boundary=' + bnd;

      {writeln('mime type = ', http.MimeType );
      writelN('parts: ', fparts.Count);}
      for i:=0 to fparts.Count-1 do begin
        dt:=THTTPDataStream(fparts[i]);
        s := '--' + bnd + CRLF;

        if dt.mimetype<>'' then s:=s+'Content-Type: '+dt.mimetype+CRLF;
        if dt.disp<>'' then s:=s +'Content-Disposition: '+dt.disp+CRLF;
        //if dt.extrahdr<>'' then s:=s+extrahdr+CRLF;//todo: make sure that extrahdr ends with CRLF!
        s:=s+CRLF;
        WriteStrToStream(HTTP.Document, s);

        dt.data.Position:=0;
        HTTP.Document.CopyFrom(dt.Data, 0);
        s := CRLF;
        WriteStrToStream(HTTP.Document, s);
      end;
      s := '--' + bnd + '--' + CRLF;
      WriteStrToStream(HTTP.Document, s);

      {Http.Document.Position:=0;
      writeln(ReadStrFromStream(Http.Document, Http.Document.Size));
      Http.Document.Position:=0;}
    end;

    if fLogWrite then  Http.Sock.OnMonitor:=Monitor;

    Result := http.HTTPMethod(m, url);

    http.Document.Position:=0;
    if http.Document.Size>0 then begin
      SetLength(AResult, http.Document.Size);
      Move(http.Document.Memory^, AResult[1], http.Document.Size);
    end else
      AResult:='';
    errCode:=http.ResultCode;

  finally
    http.Free;
    ClearParts;
  end;

end;

function THTTPConnSynapse.GetSendingData: string;
var
  i : integer;
  s : THTTPDataStream;
begin
  Result:='';
  for i:=0 to fparts.Count-1 do begin
    s:=THTTPDataStream(fparts[i]);
    Result:=Result+'Content-Disposition: '+s.disp+LineEnding;
    Result:=Result+s.extrahdr+LineEnding;
    if s.data is TStringStream then begin
      Result:=Result+TStringStream(s.data).DataString+LineEnding;
    end;
  end;
end;

end.

