{
  The file is part of docusign command-line utility.
  Please refer to LICENSE file for the use of the source codes.
}
program docusign;

uses
  SysUtils, Classes, dateutils,
  httpsend, docusignrest
  , httpconnsynapse
  , jsonutils, webstrutils
  , requestinput;

var
  Action : string = '';
  Url    : string = '';
  User   : string = '';
  Pass   : string = '';
  Key      : string = '';
  InputFn  : string = '';
  OutputFn : string = '';
  ReqFn    : string = '';
  isRaw    : Boolean = false;
  RawDir   : string = '';
  gReturnURL  : string = '';
  gAuthMethod : string = 'email';

{
  DocuSign API - API Certification

  5. The Trace must be a raw, fully consumable request and response including headers. DocuSign should be able to recreate the event using
     the provided trace. We are looking for the verb/method, URI, endpoint, authentication method, credentials, and payload. The intent is to
     validate that your Support teams can provide this information when contacting DocuSign Support for backend assistance
}
  TraceFile   : string = ''; // communication trace file

  Debug  : Boolean = false;
  isCSV  : Boolean = false;

  StatusCheck : TDocuSignStatusCheck;

procedure DumpStatus(const st: TStatusV2);
var
  i : integer;
begin
  writeln('envelopes: ', st.envCount);
  for i:=0 to st.envCount-1 do begin
    writeln( st.envelopes[i].envelopeId,' ', st.envelopes[i].status);
  end;
end;

procedure DumpStatusCSV(const st: TStatusV2);
var
  i: Integer;
begin
  for i:=0 to st.envCount-1 do
    with st.envelopes[i] do
      writeln(envelopeId
        ,',',status
        ,',',statusChangedDateTime
        ,',',envelopeUri
        ,',',documentsUri);
end;

procedure ClientError(const msg: string);
begin
  writeln('Client Error: '+msg);
end;

procedure ShowHelp;
begin
  writeln('docusign usage');
  writeln('  docusign %command% [%options] [%input_filename%]');
  writeln;
  writeln('commands:');
  writeln('  status [%options%] - lists status of envelopes');
  writeln('  send   [%options%] %inputfile% - sends a document for signing');
  writeln('           the input filename that contains the request file');
  writeln('  resend [%options%] %envelopeid% - resends notification for previously sent out envelope');
  writeln('  getsigners [%options%] %envelopeid% - get signers of an envelope');
  writeln('  gettabs [%options%] %envelopeid% - get status of tabs');
  writeln('  change [%options%] %envelopeid% %inputfile% - makes a change to envelope, specified in %inputfile%');
  writeln('  getall [%options%] %envelopeid% [%dstfile%] - gets all documents combined from a specified envelope');
  writeln('           if dstfile is not specified the content will be put to stdout');
  writeln('  void   [%options%] %envelopeid% ["%reason%"] - voids an envelope and specifies a reason for void');
  writeln('  testinput %filename% the command parses an input file and produces a request json ');
  writeln('  geturl [%options%] %envelopeid% %receipientemail% - gets a view URL of the recipient');
  writeln;
  writeln;
  writeln('options:');
  writeln(' -r  %url% - DocuSign URL to connect to');
  writeln(' -u  %username% - user name');
  writeln(' -p  %password% - password');
  writeln(' -k  %key% - security key');
  writeln(' -reqfn %reqfn% - request file name to log into');
  writeln(' -csv - give the result as comma separated values - used for "status"');
  writeln('       envelopeid, status, statuschangedatetime, envelopeURI, documentURI');
  writeln(' -debug - shows debug output');
  writeln(' -fromstatus - the lowest status to be shown');
  writeln(' -fromdate - the lowest date to be shown. Date must be in YYYY-MM-DD format. Time doesn''t matter');
  writeln(' -todate   - the highest date to be shown');
  writeln(' -trace %filename%');
  writeln('           - the file name where to write out communication trace');
  writeln;
  writeln(' -raw      - the input file contains raw json ready to be sent, rather than an input file');
  writeln(' -rawdir   - the directory where to look for raw files. (the document name must match the physical file name)');
  writeln('             used with -raw only. if rawdir is not specified the current directory is used');
  writeln(' -returnurl - returnURL used for DocuSign embedded geturl/send (with clientUserID)');
  writeln(' -authmethod - authMethod used for DocuSign embedded geturl/send (with clientUserID)');

end;

function SafeParamStr(i: integer; const def: string = ''): string;
begin
  if (i>Paramcount) then Result:=def
  else Result:=ParamStr(i);
end;

procedure ParseCommandLine;
var
  i : integer;
  a : string;
  dt : TDateTime;
begin
  if ParamCount=0 then Exit;
  action:=AnsiLowerCase(ParamStr(1));
  i:=2;
  while i<=Paramcount do begin
    a:=AnsiLowerCase(ParamStr(i));
    if a='-u' then begin
      inc(i);
      if i<=ParamCount then User:=ParamStr(i);
    end else if a = '-p' then begin
      inc(i);
      if i<=ParamCount then Pass:=ParamStr(i);
    end else if a = '-k' then begin
      inc(i);
      if i<=ParamCount then Key:=ParamStr(i);
    end else if a = '-r' then begin
      inc(i);
      if i<=ParamCount then Url:=ParamStr(i);
    end else if a = '-csv' then begin
      isCSV:=true;
    end else if a = '-debug' then begin
      Debug:=true;
    end else if a = '-reqfn' then begin
      inc(i);
      ReqFn:=SafeParamStr(i);
    end else if a = '-raw' then begin
      isRaw:=true;
    end else if a = '-rawdir' then begin
      inc(i);
      RawDir:=SafeParamStr(i);
    end else if a = '-fromstatus' then begin
      inc(i);
      StatusCheck.from_to_status:=SafeParamStr(i);
      if StatusCheck.from_to_status<>'' then
        Include(StatusCheck.flags, dcsFromToStatus);
    end else if  a = '-fromdate' then begin
      inc(i);
      if TryStrToDate( SafeParamStr(i), dt, 'YYYY-MM-DD', '-')then
        StatusCheck.from_date:=dt
      else
        writeln('failed to parse value for -fromdate: ', SafeParamStr(i));
    end else if a='-todate' then begin
      inc(i);
      if TryStrToDate( SafeParamStr(i), dt) then begin
        StatusCheck.to_date:=dt;
        Include(StatusCheck.flags,dcsToDate);
      end;
    end else if a='-trace' then begin
      inc(i);
      TraceFile:=SafeParamStr(i);
    end else if a='-returnurl' then begin
      inc(i);
      gReturnURL := SafeParamStr(i);
    end else if a='-authmethod' then begin
      inc(i);
      gAuthMethod := SafeParamStr(i);
    end else begin
      if InputFn = '' then
        InputFn:=ParamStr(i)
      else if OutputFn ='' then
         OutputFn:=ParamStr(i);
    end;

    inc(i);
  end;
end;

procedure WriteErr(r: TDocuSignRest);
var
  f : Text;
begin
  writeln('ERROR: ',r.ErrorCode,' ', r.ErrorMsg);
  if ReqFn <> '' then begin
    try ForceDirectories(ExtractFileDir(ReqFn)) except end;
    AssignFile(f, ReqFn);
    try
      Rewrite(f);
      Writeln(f, 'URL:');
      Writeln(f, r.LastURL);
      Writeln(f, 'Method:');
      Writeln(f, r.LastMthd);
      Writeln(f, 'Headers:');
      Writeln(f, r.LastHdr);
      Writeln(f, 'Request:');
      Writeln(f, r.LastReqData);
      WritelN(f, 'Response:');
      WritelN(f, r.LastResponse);
    finally
      CloseFile(f);
    end;
  end;
end;

type
  TPersonChange = record
    existEmail: string;
    existRecipID : string;
    newEmail: string;
    newName : string;
  end;

  TPersonChanges = record
    count : integer;
    ch    : array of TPersonChange;
  end;

function ParseChangeFile(const fn: string; var ch: TPersonChanges): Boolean;
var
  fs : TStringList;
  i  : integer;
  j  : integer;
  s  : string;
  k  : integer;
begin
  ch.count:=0;
  try
    fs:=TStringList.Create;
    try
      fs.LoadFromFile(fn);
      j:=0;
      i:=0;
      while i<fs.Count-1 do begin
        if j=length(ch.ch) then begin
          if j=0 then SetLength(ch.ch, 4)
          else SetLength(ch.ch, j*2);
        end;
        ch.ch[j].existEmail:=trim(fs[i]);
        inc(i);
        s:=trim(fs[i]);
        if s<>'' then begin
          k:=Pos(' ', s);
          if k<=0 then begin
            ch.ch[j].newEmail:=AnsiLowerCase(s);
            ch.ch[j].newName:='';
          end else begin
            ch.ch[j].newEmail:=AnsiLowerCase(Copy(s, 1, k-1));
            ch.ch[j].newName:=trim(Copy(s, k+1, length(s)));
          end;
          ch.ch[j].existRecipID:='';
          inc(j);
        end;
        inc(i);
      end;
      ch.count:=j;
      Result:=true;
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;
end;

procedure ActionListTabs(r: TDocuSignRest; const EnvelopeID: string);
var
  i : integer;
  rp: TDocuSignRecipients;
  u : string;
  rsp : string;
begin
  if not r.Login(Url, User, Pass, Key) then begin
    WriteErr(r);
    Exit;
  end;

  if r.GetRecipients(EnvelopeID, rp) then begin
    for i:=0 to rp.signcount-1 do begin
      u:=r.GetTabsURL(EnvelopeID, rp.signers[i].id);
      r.RawSend(u, 'GET', '', rsp);
      writeln(rp.signers[i].email,' ',rp.signers[i].name);
      writeln(rsp);
    end;
  end;
end;

procedure ActionListSigners(r: TDocuSignRest; const EnvelopeID: string);
var
  i : integer;
  rp: TDocuSignRecipients;
begin
  if not r.Login(Url, User, Pass, Key) then begin
    WriteErr(r);
    Exit;
  end;
  if r.GetRecipients(EnvelopeID, rp) then begin
    for i:=0 to rp.signcount-1 do begin
      writeln('[',rp.signers[i].id,']');
      writeln(rp.signers[i].email,' ',rp.signers[i].name);
    end;
  end;

end;

procedure ActionResend(r: TDocuSignRest; const AEnvelopeID, ARecipientEmail: string);
var
  resp: string;
  s   : string;
  rp  : TDocuSignRecipients;
  i   : integer;
  loem : string;
begin
  if AEnvelopeID='' then begin
    ClientError('EnvelopeID is not specified');
    Exit;
  end;

  if not r.Login(Url, User, Pass, Key) then begin
    WriteErr(r);
    Exit;
  end;

  if Debug then writeln('resending: ', InputFn);
    s:='';
    if not r.GetRecipients(AEnvelopeID, rp) then begin
      WriteErr(r);
      exit;
    end;
    //if ARecipientEmail='' then begin
    loem:=AnsiLowercase( ARecipientEmail);
    for i:=0 to rp.signcount-1 do begin
      //writelN('i = ',i,'/', rp.signcount,' len=',length(rp.signers));
      if (ARecipientEmail='') or (loem=AnsiLowerCase(rp.signers[i].email)) then begin
        if s<>'' then s:=s+',';
        s:=s+'{"email":"'+JsonStr(rp.signers[i].email)+'","recipientId":"'+JsonStr(rp.signers[i].id)+'"}';
      end;
    end;

    if s<>'' then begin
      if r.RawSend( r.GetRecipientsURL(AEnvelopeID) +'?resend_envelope=true', 'PUT','{"signers":['+s+']}'+#13#10,resp) then begin
      if Debug then writeln('resent!');
      ExitCode:=1;
    end else
      ExitCode:=1;
  end else
    WriteErr(r);
end;

procedure ActionChange(r: TDocuSignRest; const EnvelopeID: string; const AchangeFile: string);
var
  rp: TDocuSignRecipients;
  ch : TPersonChanges;
  i,j : integer;
  newdata : array of TDocuSignRecipient;
  cnt : integer;
begin
  if not ParseChangeFile(AchangeFile, ch) then begin
    ClientError('Unable to read changes file "'+AchangeFile+'"');
    Exit;
  end;
  if ch.count=0 then begin
    ClientError('No changes are requested or change file format is invalud');
    Exit;
  end;

  if not r.Login(Url, User, Pass, Key) then begin
    WriteErr(r);
    Exit;
  end;

  if not r.GetRecipients(EnvelopeID, rp) then begin
    ClientError('failed to get recipients');
    Exit;
  end;

  for i:=0 to rp.signcount-1 do
    rp.signers[i].email:=AnsiLowercase(rp.signers[i].email);

  writelN('ch.count=',ch.count);
  writelN('rp.signcount=',rp.signcount);
  for i:=0 to ch.count-1 do
    for j:=0 to rp.signcount-1 do begin
      writelN(rp.signers[j].email,' <> ',ch.ch[i].existEmail);
      if rp.signers[j].email=ch.ch[i].existEmail then
        ch.ch[i].existRecipID:=rp.signers[j].id;
    end;

  SetLength(newdata, ch.count);

  cnt:=0;
  for i:=0 to ch.count-1 do begin
    if ch.ch[i].existRecipID<>'' then begin
      newdata[cnt].id:=ch.ch[i].existRecipID;
      newdata[cnt].email:=ch.ch[i].newEmail;
      newdata[cnt].name:=ch.ch[i].newName;
      inc(cnt);
    end;
  end;

  if cnt=0 then begin
    ClientError('no changes are needed or no changes identified');
    Exit;
  end;

  r.EditRecipients(EnvelopeID, newdata, cnt);
end;

procedure ActionGetURL(r: TDocuSignRest; const EnvID, recpEmail: string);
var
  rp  : TDocuSignRecipients;
  i   : integer;
  rurl : string;
begin
  if not r.Login(Url, User, Pass, Key) then begin
    WriteErr(r);
    Exit;
  end;

  if not r.GetRecipients(EnvID, rp) then begin
    ClientError('failed to get recipients');
    Exit;
  end;

  for i:=0 to rp.signcount-1 do begin
    if AnsiLowerCase(rp.signers[i].email)=AnsiLowerCase(recpEmail) then begin
      rurl:=r.GetRecpViewURL(EnvID, rp.signers[i], gReturnURL,gAuthMethod);
      if rurl<>'' then begin
        ExitCode:=1;
        writeln(recpEmail,' ', rurl);
      end else
        WriteErr(r);
      Exit;
    end;
  end;
  ClientError('recipient not found: '+ recpEmail);

end;


procedure ActionStatus(r: TDocuSignRest);
var
  st  : TStatusV2;
begin
  r.Login(Url, User, Pass, Key);
  if Debug then begin
    writeln('Checking status:');
    writeln('from date:   ', FormatDateTime('YYYY-MM-DD', StatusCheck.from_date));
    if StatusCheck.to_date<>0 then
      writeln('to date:     ', FormatDateTime('YYYY-MM-DD', StatusCheck.to_date));
    if StatusCheck.from_to_status<>'' then
      writeln('from status: ', StatusCheck.from_to_status);
  end;
  if r.GetStatus( r.GetAccountId, StatusCheck, st ) then begin
    if isCSV then DumpStatusCSV(st)
    else DumpStatus(st);
    ExitCode:=1;
  end else begin
    WriteErr(r);
  end;
end;

procedure ActionSend(r: TDocuSignRest);
var
  q   : TDocuSignRequest;
  env : TEnvelopeInfoV2;
  anyError:  Boolean;
  i : Integer;
  fs : TFileStream;
  raw : string;
  j   : integer;
begin
  if Debug then writeln('input file: "', InputFn,'"');

  if InputFn ='' then begin
    ClientError('no input file specified');
    Exit;
  end;

  if isRaw then begin
    if not r.Login(Url, User, Pass, Key) then begin
      WriteErr(r);
      Exit;
    end;

    fs:=TFileStream.Create(InputFn, fmOpenRead or fmShareDenyNone);
    try
      SetLength(raw, fs.Size);
      fs.Read(raw[1], fs.Size);
    finally
      fs.Free;
    end;
    if r.RequestSignRaw(raw, rawDir, env) then begin
      ExitCode:=1;
      writeln(env.envelopeId);
    end else
      WriteErr(r);
    Exit;
  end;

  try
    ReadRequestFile(InputFn, q);
  except
    ClientError('unable reading the input file');
    Exit;
  end;
  anyError:=false;
  for i:=0 to q.doccount-1 do
    if (trim(q.documents[i].filename)<>'') and not FileExists(q.documents[i].filename) then begin
      ClientError('"'+q.documents[i].filename+'" file doesn''t exist ');
      anyError:=true;
    end;

  if q.subj='' then begin
    ClientError('no subject found in the input file');
    anyError:=true;
  end;
  if q.signcount=0 then begin
    ClientError('no receipients found in the input file');
    anyError:=true;
  end;
  if q.doccount=0 then begin
    ClientError('no documents specified');
    anyError:=true;
  end;
  if url='' then begin
    ClientError('docusign url is not specified');
    anyError:=true;
  end;
  if anyError then Exit;

  if User='' then writeln('Client Warning: User name is not specified, the submission is likely to fail');
  if Pass='' then writeln('Client Warning: Password is not specified, the submission is likely to fail');
  if Key ='' then writeln('Client Warning: Security Key is not specified, the submission is likely to fail');
  if Debug then
    writeln('suject file: "',q.subj, '", documents: ', q.doccount,', signers: ' ,q.signcount);

  if r.Login(Url, User, Pass, Key) then begin
    if r.RequestSign(q, env) then begin
      ExitCode:=1;
      writeln(env.envelopeId);

      for i:=0 to q.signcount-1 do begin
        if q.signers[i].clientUserID<>'' then begin
          writeln(q.signers[i].email,' ', r.GetRecpViewURL(env.envelopeId, q.signers[i], gReturnURL, gAuthMethod));
        end;
      end;
      //env.recipientsUri

    end else
      WriteErr(r);
  end else
    WriteErr(r);
end;

procedure ActionVoid(r: TDocuSignRest; const AEnvelopeID, AMessage: string);
begin
  if not r.Login(Url, User, Pass, Key) then begin
    WriteErr(r);
    Exit;
  end;

  if Debug then writeln('voiding: ', AEnvelopeID);

  if r.Void( DocuSignVoid( AEnvelopeID, AMessage )) then begin
    if Debug then writeln('voided!');
    ExitCode:=1;
  end else
    WriteErr(r);
end;


procedure ActionTestInput(r: TDocuSignRest);
var
  q   : TDocuSignRequest;
begin
  ReadRequestFile(InputFn, q);
  writeln(DocuSignRequestSignStr('sent', q));
end;

procedure ActionGetAll(r: TDocuSignRest);
var
  dr : TDocuSignGetDoc;
  fs : TFileStream;
  mem : TStringStream;
  st  : TStream;
begin
  r.Login(Url, User, Pass, Key);
  dr.envelopeId:=InputFn;
  dr.documentId:='';
  dr.show_changes:=false;
  dr.watermark:=false;
  dr.certificate:=false;
  if OutputFn<>'' then begin
    try ForceDirectories(ExtractFileDir(OutputFn)) except end;
    fs:=TFileStream.Create(OutputFn, fmCreate);
    st:=fs;
  end else begin
    mem:=TStringStream.Create('');
    st:=mem;
  end;
  try
    if not r.GetDocument(dr, st) then
      WriteErr(r)
    else begin
      ExitCode:=1;
    end;
    if st = mem then writeln(mem.DataString);
  finally
    st.Free;
  end;
end;

var
  r   : TDocuSignRest;
begin
  StatusCheck.from_date:=now-365;
  StatusCheck.to_date:=0;
  StatusCheck.to_status:='';
  StatusCheck.from_to_status:='';
  StatusCheck.flags:=[dcsFromDate];

  ParseCommandLine;
  if action='' then begin
    ShowHelp;
    exit;
  end;

  r := TDocuSignRest.Create( THTTPConnSynapse.Create );
  try
    r.DebugLog:=Debug;
    r.TraceFile:=TraceFile;
    if action = 'status' then begin
      ActionStatus(r);
    end else if action='change' then begin
      ActionChange(r, InputFn, OutputFn);
    end else if action='gettabs' then begin
      ActionListTabs(r, InputFn);
    end else if action='resend' then begin
      ActionResend(r, InputFn, OutputFn);
    end else if action='getsigners' then begin
      ActionListSigners(r, InputFn);
    end else if action = 'testinput' then begin
      ActionTestInput(r);
    end else if action = 'send' then begin
      ActionSend(r);
    end else if action='getall' then begin
      ActionGetAll(r);
    end else if action='geturl' then begin
      ActionGetURL(r, InputFn, OutputFn);
    end else if action='void' then begin
      ActionVoid(r, InputFn, OutputFn);
    end else
      writeln('unknown action: "', action,'"');
  finally
    r.Free;
  end;


end.

