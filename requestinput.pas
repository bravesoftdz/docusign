{
  The file is part of docusign command-line utility.
  Please refer to LICENSE file for the use of the source codes.
}
unit requestinput;

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface

uses
  Classes, SysUtils, docusignrest;

procedure ReadRequestFile(const FileName: string; var r: TDocuSignRequest);
procedure ReadRequestFile(const src: TStream; var r: TDocuSignRequest);

type
  TFieldLock = (flUndefined, flLocked, flUnlock);
  TFieldData = record
    pagenum        : Integer;
    left, top      : Double;
    width, height  : Double;
    text           : string;
    fldtype        : string;
    tabLabel       : string;
    anchor         : string;
    isAnchorStrict : Boolean;
    cond           : string;
    receipient     : string;
    skipfield      : Boolean; // set to true, if receipent paranthesis () are specified, but empty
    style          : string;
    required       : Boolean;
    locked         : TFieldLock; // explicitly unlocked
  end;

  { TDocuSignInputFile }

  TDocuSignInputFile = class(TObject)
  private
    fSection : string;
    fFieldDoc: string;
    fPageNum : Integer;
    fFldData : TFieldData;
    fMrgXMM : double;
    fMrgYMM : double;
  private
    procedure AddField(var r: TDocuSignRequest; const fd: TFieldData);
    procedure ParseSectionData(const data: string; var r: TDocuSignRequest);
    procedure NormalizeRequest(var r: TDocuSignRequest);
  public
    procedure LoadFromStream(src: TStream; var r: TDocuSignRequest);
  end;

const
  SECDOCS = 'docs';
  SECTABS = 'tabs';
  SECSUBJ = 'subject';
  SECSIGNERS = 'signers';
  SECCARBONCOPY = 'carboncopies';

  _SECCC = 'cc'; // alias for carboncopies

  SMS_AUTH_DEFAULT = 'SMS Auth $';
  ID_AUTH_DEFAULT  = 'ID Check $'; // todo: is it correct?

function isCoord(const s: string; var millimeter: double; const DefType: string = ''): Boolean;
function MMtoPt(mm: double): double;
procedure ParseFieldCoord(const src: string; var x,y,width,height:double);
procedure TestCoord;

function ParseField(const s: string; var fld : TFieldData; var isNewField: Boolean): Boolean;
procedure TestParseField;

function FindDocumentId(const r: TDocuSignRequest; const docfilename: string): string;
function FindRecipientId(const r: TDocuSignRequest; const rcvemail: string): string;

implementation

const
  whitespace : set of char = [#32, #9];

procedure TestCoord;
var
  s : string;
  mm : double;
begin
  readln(s);
  while s<>'' do begin
    if isCoord(s, mm) then begin
      writeln( mm:0:2, 'mm');
      writeln( MMtoPt(mm):0:0,'pt');
    end else
      writeln('not coord');
    readln(s);
  end;
end;

function FindDocumentId(const r: TDocuSignRequest; const docfilename: string): string;
var
  i : integer;
  lw : string;
begin
  lw:=AnsiLowerCase(docfilename);
  for i:=0 to r.doccount-1 do
    if AnsiLowercase(r.documents[i].filename)=lw then begin
      Result:=r.documents[i].id;
      Exit;
    end;
  Result:='';
end;

function FindRecipientId(const r: TDocuSignRequest; const rcvemail: string): string;
var
  i : integer;
  lw : string;
begin
  lw:=AnsiLowerCase(rcvemail);
  for i:=0 to r.signcount-1 do
    if AnsiLowerCase(r.signers[i].email)=lw then begin
      Result:=r.signers[i].id;
      Exit;
    end;
end;

procedure FieldDataClear(var d: TFieldData);
begin
  d.text:='';
  d.fldtype:='';
  d.tabLabel:='';
  d.anchor:='';
  d.cond:='';
  d.receipient:='';
  d.style:='';
  FillChar(d, sizeof(d), 0);
end;

function GetSection(const s: string): string;
var
  tt: string;
begin
  Result:='';
  if (length(s)=0) or (s[1]<>'*') then Exit;
  tt:=trim(s);
  //if tt[length(tt)]<>':' then Exit;
  Result:=AnsiLowerCase(Copy(tt, 2, length(tt)-1));

  if Result=_SECCC then
    Result:=SECCARBONCOPY;
  //writeln('tt: ', Result);

  {if (Result<>SECDOCS) and (Result<>SECSIGNERS) and (Result<>SECDOCS) and (Result<>SECTABS) then
    Result:='';}
end;

procedure ParseParams(const s: string; var p1, p2: string);
var
  i : integer;
  t : string;
begin
  t:=trim(s);
  if t='' then begin
    p1:='';
    p2:='';
    Exit;
  end;
  if t[1]='"' then begin
    t:=Copy(t, 2, length(t));
    i:=pos('"', t);
    p1:=Copy(t, 1, i-1);
    p2:=trim(Copy(t, i+1, length(t)));
  end else begin
    i:=Pos(' ', t);
    if i<=0 then begin
      p1:=t;
      p2:='';
    end else begin
      p1:=copy(t, 1, i-1);
      p2:=trim(copy(t, i+1, length(t)));
    end;
  end;
end;

function NextWordEnd(const s: string; const CloseChar: string; var idx: integer): string;
var
  i : integer;
begin
  i:=idx;
  while (i<=length(s)) and (s[i]<>CloseChar) do
    inc(i);
  Result:=Copy(s, idx, i-idx);
  inc(i);
  idx:=i;
end;

function NextWord(const s: string; var idx: integer): string;
var
  i : Integer;
  j : Integer;
  ch: char;
begin
  i:=idx;
  Result:='';
  while (i<=length(s)) and (s[i] in whitespace) do
    inc(i);
  if (i<=length(s)) then begin
    j:=i;
    if (i<=length(s)) and (s[i] in [#39,'"']) then begin
      ch:=s[i];
      inc(i);
      while (i<=length(s)) and not (s[i] in [#10,#13, ch]) do
        inc(i);
      Result:=Copy(s, j+1, i-j-1);
      if (i<=length(s)) and (s[i] = ch) then inc(i);
    end else begin
      while (i<=length(s)) and not (s[i] in whitespace) do
        inc(i);
      Result:=Copy(s, j, i-j);
    end;
  end;
  idx:=i;
end;

function NextPageCoord(const s: string; var idx: integer; var f: double): Boolean;
var
  t   : string;
  err : Integer;
begin
  t:=NextWord( s, idx);
  Result:=t<>'';
  if Result then begin
    Val(s, f, err);
    Result:=err=0;
  end;
end;

function isCoord(const s: string; var millimeter: double; const DefType: string = ''): Boolean;
var
  i : integer;
  t : string;
  err : Integer;
begin
  i:=1;
  while (i<=length(s)) and (s[i] in ['-','+','.','0'..'9'])do
    inc(i);
  Val(Copy(s, 1, i-1), millimeter, err);
  Result:=err=0;
  if Result then begin
    t:=AnsiLowerCase(Copy(s, i,length(s)));
    if t='' then t:=DefType;
    if t='in' then millimeter:=millimeter*25.4
    else if t='cm' then millimeter:=millimeter*10
    else if t='pt' then millimeter:=millimeter*25.4/72
    else if t='m' then millimeter:=millimeter*1000;
  end;
end;

function MMtoPt(mm: double): double;
begin
  Result:=mm/25.4*72;
end;

procedure ParseFieldCoord(const src: string; var x,y,width,height:double);
var
  i   : integer;
  j   : integer;
  cnt : Integer;
  s   : string;
  dd  : double;
begin
  x:=0;
  y:=0;
  width:=0;
  height:=0;
  j:=1;
  cnt:=0;
  for i:=1 to length(src)+1 do
    if (i>length(src)) or (src[i]=',') then begin
      s:=trim(Copy(src, j, i-j));

      if (s<>'') and (isCoord(s, dd)) then
        case cnt of
          0: x:=dd;
          1: y:=dd;
          2: width:=dd;
          3: height:=dd;
        end;

      inc(cnt); // it's allowed for a value to be empty
      if cnt>3 then Exit; //done!
      j:=i+1;
    end;
end;

function ParseField(const s: string; var fld : TFieldData; var isNewField: Boolean): Boolean;
var
  i : integer;
  p : string;
  j : integer;
begin
  isNewField:=false;
  i:=1;
  while (i<=length(s)) do begin
    if s[i] in whitespace then
      inc(i)
    else if (s[i]=':') then begin
      isNewField:=true;
      inc(i);
      fld.fldtype:=NextWord(s, i);
      j:=1;
      while (j<=length(fld.fldtype)) and (fld.fldtype[j] in ['!','+','-']) do
      begin
        if fld.fldtype[j]='!' then fld.required:=true
        else if fld.fldtype[j]='+' then fld.locked:=flUnlock
        else if fld.fldtype[j]='-' then fld.locked:=flLocked;
        inc(j);
      end;
      fld.fldtype:=Copy(fld.fldtype, j, length(fld.fldtype));
    end else if (s[i]='#') then begin
      inc(i);
      fld.tabLabel:=NextWord(s, i);
    end else if (s[i]='@') then begin
      inc(i);
      if (i<=length(s)) and (s[i]='!') then
      begin
        inc(i);
        fld.isAnchorStrict:=true;
      end;
      fld.anchor:=NextWord(s, i);
    end else if (s[i] = '?') then begin
      inc(i);
      fld.cond:=NextWord(s, i);
    end else if (s[i] = '(') then begin
      inc(i);
      fld.receipient:=NextWordEnd(s, ')', i);
      if trim(fld.receipient) = '' then fld.skipfield:=true;
    end else if (s[i] = '{') then begin
      inc(i);
      fld.style:=NextWordEnd(s, '}', i);
    end else if (s[i]='[') then begin
      inc(i);
      p:=NextWordEnd(s, ']', i);
      ParseFieldCoord(p, fld.left, fld.top, fld.width, fld.height);
    end else if (s[i] in ['"',#39]) then begin
      inc(i);
      fld.text:=NextWordEnd(s, s[i-1], i);
    end else
      inc(i);
  end;
  Result:=True;
end;

procedure TDocuSignInputFile.AddField(var r: TDocuSignRequest; const fd: TFieldData);
var
  i : integer;
  rcp : string;
  rcpid : string;
  j : integer;
begin
  rcp:=fd.receipient;
  rcpid:=rcp;

  i:=DocuSignRequestAddTab(r);
  r.tabs[i]._tabtype:=fd.fldtype;
  r.tabs[i].documentId:=fFieldDoc;
  r.tabs[i].pageNumber:=IntToStr(fd.pagenum);
  r.tabs[i].recipientId:=rcpid;
  r.tabs[i]._lockset:=fd.locked<>flUndefined;
  r.tabs[i].locked:=fd.locked=flLocked;

  if fd.anchor<>'' then begin
    r.tabs[i].anchorString:=fd.anchor;
    r.tabs[i].anchorIgnoreIfNotPresent:=not fd.isAnchorStrict;
    r.tabs[i].anchorUnits:='mms';
    r.tabs[i].anchorXOffset:=fd.left;
    r.tabs[i].anchorYOffset:=fd.top;
  end else begin
    r.tabs[i].xPosition:=MMtoPt(fd.left+fMrgXMM);
    r.tabs[i].yPosition:=MMtoPt(fd.top+fMrgYMM);
  end;

  r.tabs[i].width:=MMtoPt(fd.width);
  r.tabs[i].height:=MMtoPt(fd.height);
  r.tabs[i].value:=fd.text;
  r.tabs[i].tabLabel:=fd.tabLabel;

  j:=Pos('=',fd.cond);
  if j>0 then begin
    r.tabs[i].conditionalParentLabel:=Copy(fd.cond, 1, j-1);
    r.tabs[i].conditionalParentValue:=Copy(fd.cond, j+1, length(fd.cond));
  end;

  r.tabs[i].required:=fd.required;
end;

procedure CheckAuth(var r: TDocuSignRecipient; const authConfName: string);
begin
  if (authConfName<>'') and (not r.requireIdLookup) then begin
    r.requireIdLookup:=true;
    r.idCheckConfigurationName:=authConfName;
  end;
end;

function StrValueToBool(const avalue: string): boolean;
var
  v : string;
begin
  v:=AnsiLowerCase(avalue);
  Result:=(v='1') or (v='true') or (v='yes') or (v='y') or (v='on');
end;

procedure AddFieldtoRecp(var r: TDocuSignRecipient; const adata, avalue: string);
var
  n : string;
begin
  n:=AnsiLowerCase(adata);
  if (n = ':auth') and (avalue<>'') then begin
    r.requireIdLookup:=true;
    r.idCheckConfigurationName:=avalue;
  end else if (n = ':sms.phone') and (avalue<>'') then begin
    CheckAuth(r, SMS_AUTH_DEFAULT);
    if r.smsPhoneCount=length(r.smsPhoneCheck) then begin
      if r.smsPhoneCount=0
        then SetLength(r.smsPhoneCheck, 2)
        else SetLength(r.smsPhoneCheck, r.smsPhoneCount * 2)
    end;
    r.smsPhoneCheck[r.smsPhoneCount]:=avalue;
    inc(r.smsPhoneCount);
  end else if (n = ':accesscode') then begin
    r.accesscode:=avalue;
  end else if (n = ':accesscodetoemail') then begin
    r.accesscodetoemail:=StrValueToBool(avalue);
  end else if (n = ':idauth.address1') and (avalue<>'') then begin
    CheckAuth(r, ID_AUTH_DEFAULT);
    r.idauthaddress1:=avalue;
  end else if (n = ':idauth.addrses2') and (avalue<>'') then begin
    CheckAuth(r, ID_AUTH_DEFAULT);
    r.idauthaddress2:=avalue;
  end else if (n = ':idauth.city') and (avalue<>'') then begin
    CheckAuth(r, ID_AUTH_DEFAULT);
    r.idauthcity:=avalue;
  end else if (n = ':idauth.state') and (avalue<>'')then begin
    CheckAuth(r, ID_AUTH_DEFAULT);
    r.idauthstate:=avalue;
  end else if (n = ':idauth.zip')  and (avalue<>'') then begin
    CheckAuth(r, ID_AUTH_DEFAULT);
    r.idauthzip:=avalue;
  end else if (n = ':idauth.dateofbirth') and (avalue<>'')then begin
    CheckAuth(r, ID_AUTH_DEFAULT);
    r.idauthdateofbirth:=avalue;
  end else if n = ':clientuserid' then begin
    r.clientUserID:=avalue;
  end;
end;

procedure TDocuSignInputFile.ParseSectionData(const data: string; var r: TDocuSignRequest);
var
  p1, p2 : string;
  l : string;
  isnewfld: Boolean;
  i : integer;
  mm : double;
const
  PFX_DOC  = 'doc ';
  PFX_PAGE = 'page ';
  PFX_MARGIN = 'margin ';
begin
  if fSection=SECSUBJ then
    r.subj:=r.subj+data
  else if fSection=SECTABS then begin
    l:=StringReplace(AnsiLowerCase(data), #9, #32, [rfReplaceAll]);
    if length(l)=0 then Exit;

    if pos(PFX_DOC, l)=1 then begin
      fFieldDoc:=trim(Copy(data, 5, length(data)));
    end else if pos(PFX_PAGE, l)=1 then begin
      fPageNum:=StrToIntDef(trim(Copy(data, length(PFX_PAGE)+1,length(data))),-1);
    end else if pos(PFX_MARGIN, l)=1 then begin
      i:=1;
      inc(i, length(PFX_MARGIN));
      p1:=NextWord(l, i);
      if isCoord(p1, mm) then fMrgXMM:=mm;

      p2:=NextWord(l, i);
      if isCoord(p2, mm) then fMrgYMM:=mm;

    end else if l[1] in [#32,#9,':'] then begin
      ParseField(data, fFldData, isnewfld);
      if fFldData.fldtype<>'' then begin
        fFldData.pagenum:=fPageNum;

        if not fFldData.skipfield then
          AddField(r, fFldData);
      end;
      FieldDataClear(fFldData);
    end;
  end else if fSection=SECDOCS then begin
    ParseParams(data, p1, p2);
    DocuSignRequestAddDocument(r, p1, p2 );
  end else if fSection=SECSIGNERS then begin
    ParseParams(data, p1, p2);

    p1:=trim(p1);
    if Pos(':',p1)<>1 then
      DocuSignRequestAddSigner(r, p1, p2)
    else if (r.signcount>0) then begin
      AddFieldtoRecp(r.signers[r.signcount-1], p1, p2);
    end;
  end else if fSection=SECCARBONCOPY then begin
    ParseParams(data, p1, p2);
    DocuSignRequestAddCarbonCopy(r, p1, p2);
  end;
end;


procedure TDocuSignInputFile.NormalizeRequest(var r: TDocuSignRequest);
var
  i : integer;
  rid : string;
  k  : integer;
begin
  if r.signcount>0 then rid := r.signers[0].id;

  if r.cccount>0 then begin
    k:=r.signcount+1;
    for i:=0 to r.cccount-1 do begin
      r.carboncopes[i].id:=IntToStr(k);
      inc(k);
    end;
  end;

  if (r.signcount>0) and (r.cccount>0) then begin
    for i:=0 to r.signcount-1 do
      r.signers[i].routingOrder:=2+i;
    for i:=0 to r.cccount-1 do
      r.carboncopes[i].routingOrder:=1;
  end;


  for i:=0 to r.tabcount-1 do begin
    r.tabs[i].documentId:=FindDocumentId(r, r.tabs[i].documentId); // from document path
    rid:=FindRecipientId(r, r.tabs[i].recipientId);
    r.tabs[i].recipientId:=rid; // from email
  end;
end;

procedure ReadRequestFile(const src: TStream; var r: TDocuSignRequest);
var
  inp : TDocuSignInputFile;
begin
  inp := TDocuSignInputFile.Create;
  try
    inp.LoadFromStream(src, r);
  finally
    inp.Free;
  end;
end;

procedure ReadRequestFile(const FileName: string; var r: TDocuSignRequest);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    ReadRequestFile(fs, r);
  finally
    fs.Free;
  end;

end;

{ TDocuSignInputFile }

procedure TDocuSignInputFile.LoadFromStream(src: TStream; var r: TDocuSignRequest);
var
  st   : TStringList;
  s    : string;
  ts   : string;
  body : TStringList;
  i    : integer;
begin
  DocuSignRequestInit(r);
  st:=TStringList.Create;
  body:=TStringList.Create;
  fSection:='';
  try
    st.LoadFromStream(src);
    for i:=0 to st.Count-1 do begin
      s:=st[i];
      ts:=trim(s);
      // skipping comments
      if ts='' then begin
        fSection:=''; // end current section
        Continue;
      end;
      if (s[1] in ['#',';']) or (copy(s,1,2)='//') then Continue;
      if fSection='' then begin
        fSection:=GetSection(s);
        //writeln('parsing: ', s);
        //writeln('result: ', sec);
        if fSection='' then body.Add(s);
      end else
        ParseSectionData(s, r);
    end;
    NormalizeRequest(r);
    r.body:=body.Text;
  finally
    st.Free;
    body.Free;
  end;
end;

procedure TestParseField;
var
  s : string;
  fld : TFieldData;
  isnewfld : Boolean;
begin
  readln(s);
  while s<>'' do begin
    ParseField(s, fld, isnewfld);
    writeln( 'fldTypes: ', fld.fldtype);
    writeln( 'coords:   ',fld.left:0:2, ' ',fld.top:0:2, ' ',fld.height:0:2, ' ',fld.width:0:2);
    writeln( 'tabLable: ', fld.tabLabel);
    writeln( 'anchor:   ', fld.anchor);
    writeln( 'cond:     ', fld.cond);
    writeln( 'Signers:  ', fld.receipient);
    writeln( 'style:    ', fld.style);
    readln(s);
  end;
end;

end.

