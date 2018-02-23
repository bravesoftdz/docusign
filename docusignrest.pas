{
  The file is part of docusign command-line utility.
  Please refer to LICENSE file for the use of the source codes.
}
unit docusignrest;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, jsonparser, fpjson, jsonutils, webstrutils;

const
  UrlEnvelopes  ='/envelopes';
  UrlRecipients ='/recipients';
  UrlViewRecp   ='/views/recipient';

const
  DocuSignIntCoord : Boolean = true;    // this affects DocuSignFloatFmt, if true, format will be %0.0f
    // Width / Height must be integers! for Docusign Version 2
    // though it's not mentioned anywhere in DocuSign requirements
  DocuSignFloatFmt : string = '%0.2f';

type
  TLoginAccountV2 = record
    name      : string;
    accountid : string;
    baseurl   : string;
    isdefault : string;
    username  : string;
    userid    : string;
    email     : string;
    sitedescr : string;
  end;
  TLoginAccountsV2 = array of TLoginAccountV2;

  TEnvelopeInfoV2 = record
    status        : string;
    documentsUri  : string;
    recipientsUri : string;
    envelopeUri   : string;
    emailSubject  : string;
    emailBlurb    : string;
    envelopeId      : string;
    customFieldsUri : string;
    notificationUri : string;
    enableWetSign   : string;
    createdDateTime : string;
    statusChangedDateTime : string;
    documentsCombinedUri  : string;
    certificateUri  : string;
    templatesUri    : string;
  end;

  TStatusV2 = record
    resultSetSize : Integer;
    totalSetSize  : Integer;
    startPosition : Integer;
    endPosition   : Integer;
    nextUri       : string;
    previousUri   : string;
    envCount  : Integer;
    envelopes : array of TEnvelopeInfoV2;
  end;

type

  { THTTPConn }

  THTTPConnRequest = procedure (Sender: TObject; const url, method, headers: string; body: TStream) of object;

  THTTPConn = class(TObject)
  private
    fOnRequest : THTTPConnRequest;
  protected
    procedure DoRequest(const url, method, headers: string; body: TStream);
  public
    procedure AddPart(s: TStream; OwnStream: Boolean; const atype, adisposition, extrahdr: string); virtual; abstract; overload;
    procedure AddPart(const data: string; const atype, adisposition, extrahdr: string); virtual; overload;
    function Http(const url, method, extraheader{, input}: string; var Response: string;
        var errCode: Integer): Boolean; virtual; abstract;
    function GetSendingData: string; virtual; abstract;
    property OnRequest : THTTPConnRequest read fOnRequest write fOnRequest;
  end;

  { TDocuSignRest }

  TDocuSignStatusCheckFlags = set of (dcsFromDate, dcsToDate, dcsFromToStatus, dcsStatus);

  TDocuSignStatusCheck = record
    flags    : TDocuSignStatusCheckFlags;
    from_date : TDatetime;
    to_date   : TDateTime;
    from_to_status : string;
    to_status     : string;
  end;

  TDocuSignDocument = record
    name     : string;
    id       : string;
    filename : string;
    stream   : TStream;
  end;

  TDocuSignTab = record
    _tabtype : string;
    _lockset : boolean;

    documentId  : string;
    pageNumber  : string;
    recipientId : string;

    xPosition : double; // this is coordinate off set in Points (pt)
    yPosition : double;
    height    : double;
    width     : double;

    anchorString : string;
    anchorXOffset: double;
    anchorYOffset: double;
    anchorIgnoreIfNotPresent : boolean;
    anchorUnits: string;
    conditionalParentLabel: string;
    conditionalParentValue: string;
    templateLocked   : Boolean;
    templateRequired : Boolean;

    mergeFieldXml: string; //reserved
    name : string;  // must be "CheckBox" for checkbox
                    // "Company" for company
                    // "Date Signed"
                    // "Text" for Date tab
                    // "Text" for Email tab
                    // "Full Name" for Full Name
    requireInitialOnSharedTabChange: Boolean;
    selected: Boolean;
    shared: Boolean;
    bold : double;
    font : string;
    fontColor : string;
    fontSize  : string;
    italic: boolean;
    tabLabel : string; // it is "groupName" for Radio Groups button
    scaleValue : string;
    underline : Boolean;
    concealValueOnDocument : Boolean;
    disableAutosize: Boolean;
    required: Boolean;
    locked: Boolean;
    value: string;
    buttonText : string; // "Approve" for Approve button, "Decline" for Decline button
  end;

  TDocuSignRecipient = record
    name  : string;
    id    : string;
    email : string;

    requireIdLookup : Boolean;
    idCheckConfigurationName : string;
    smsPhoneCheck   : array of string;
    smsPhoneCount   : integer;

    accesscode        : string;
    accesscodetoemail : Boolean;

    idauthaddress1    : string;
    idauthaddress2    : string;
    idauthcity        : string;
    idauthstate       : string;
    idauthzip         : string;
    idauthdateofbirth : string;
    idauthdisplaycode : string;

    clientUserID      : string;

    routingOrder      : Integer;

  end;

  //
  TExtraData = record
    name  : string;
    value : string;
  end;

  TDocuSignRequest = record
    subj      : string;
    body      : string;

    signers   : array of TDocuSignRecipient;
    signcount : Integer;

    documents : array of TDocuSignDocument;
    doccount  : integer;

    tabs      : array of TDocuSignTab;
    tabcount  : integer;

    carboncopes : array of TDocuSignRecipient;
    cccount     : Integer;
  end;

  TDocuSignRecipients = record
    signers   : array of TDocuSignRecipient;
    signcount : Integer;

    //recipientCount : integer;
    //currentRoutingOrder : Integer;
  end;

  TTraceInfo = record
    verb        : string;
    uri         : string;
    endpoint    : string;
    authmethod  : string;
    credentials : string;
    body        : string;
  end;

  TTraceEvent = procedure (Sender: TObject; const info: TTraceInfo) of object;

  TDocuSignGetDoc = record
    envelopeId   : string;
    documentId   : string;
    watermark    : Boolean;
    show_changes : Boolean;
    certificate  : Boolean;
  end;

  TDocuSignVoid = record
    EnvelopeId: string;
    Reason: string;
  end;

  TDocuSignRest = class(TObject)
  private
    fConn        : THTTPConn;
    floginurl    : string;
    fbaseurl     : string;
    fOnRequest   : TTraceEvent;
    fOnResponse  : TTraceEvent;

    isFileResp   : Boolean;
  protected
    fAuthParam : string;
    fHttpError : integer;

    procedure AddRequestDataStr(const s: string; const ContentType, Disposition, ExtraHeaders: string);
    procedure AddRequestData(st: TStream; OwnStream: Boolean; const ContentType, Disposition, ExtraHeaders: string);
    function SendRequest(const Url, HttpMethod: string; var Resp: String): Boolean; overload;
    function SendRequest(const Url, HttpMethod, Body: string; var Resp: String): Boolean; overload;
  protected
    traceStream: TFileStream;
    procedure TraceData(const s: string);

    function PostEditRecipients(const envelopeid: string;
      const rawData: string): Boolean;
  public
    apiversion : string;
    DebugLog   : Boolean;
    logins     : TLoginAccountsV2;
    TraceFile   : string;

    LastResponse : string;
    LastURL      : string;
    LastHdr      : string;
    LastReqData  : string;
    LastMthd     : string;
    ErrorCode    : string;
    ErrorMsg     : string;
    constructor Create(AConn: THTTPConn);
    destructor Destroy; override;
    function Login(const aloginurl, aname, apassword, akey: string): Boolean;
    function GetStatus(const accountid: string; StatusCheck: TDocuSignStatusCheck; var st: TStatusV2): Boolean;
    function GetRecipients(const envelopeid: string;
      var rp: TDocuSignRecipients): Boolean;
    function EditRecipients(const envelopeid: string;
      const signers: array of TDocuSignRecipient; signCount: integer): Boolean;

    function RequestSign(const request: TDocuSignRequest; var env: TEnvelopeInfoV2): Boolean;
    function RequestSignRaw(const rawData, rawFileDir: string; var env: TEnvelopeInfoV2): Boolean;
    function GetDocument(doc: TDocuSignGetDoc; dst: TStream): Boolean;
    function Void(void: TDocuSignVoid): Boolean;
    function GetRecpViewURL(const EnvId: string; const cl: TDocuSignRecipient;
      const ReturnURL: string = '';
      const AuthMethod: String = 'email'): string;

    //todo: remove GetAccountId?
    function GetAccountId: string;

    function RawSend(const Url, HttpMethod, Body: string; var Resp: String): Boolean;

    function GetEnvelopesURL: string;
    function GetEnvelopeURL(const envelopeid: string): string;
    function GetRecipientsURL(const envelopeid: string): string;
    function GetTabsURL(const envelopeid, recipientid: string): string;

    property BaseURL: string read fbaseurl write fbaseurl;
    property OnTraceRequest: TTraceEvent read fOnRequest write fOnRequest;
    property OnTraceResponse: TTraceEvent read fOnResponse write fOnResponse;
  end;


function JsonStrParse(const js: string): TJSONData;

// json generators

function DocuSignJsonLoginStr(const aname, apassword, akey: string): string;
function DocuSignArrayOfDocumentsStr(const documents: array of TDocuSignDocument; len: Integer ): string;
function DocuSignArrayOfRecipientsStr(//const idname: string;
  const res: array of TDocuSignRecipient; len: Integer;
  const tabs: array of TDocuSignTab; const tabCount: integer): string;
function DocuSignRequestSignStr(const astatus: string; const request: TDocuSignRequest): string;

procedure DocuSignRequestInit(var request : TDocuSignRequest; const subject: string = ''; const body: string = '');
function DocuSignRequestAddSigner(var request : TDocuSignRequest; const email, name: string): string;
function DocuSignRequestAddDocument(var request : TDocuSignRequest; const filename, name: string): string; overload;
function DocuSignRequestAddDocument(var request : TDocuSignRequest; stream: TStream; const name: string): string; overload;
function DocuSignRequestAddCarbonCopy(var request : TDocuSignRequest; const email, name: string): string;


function DocuSignRequestAddTab(var request : TDocuSignRequest): Integer;
procedure DocuSignTabInit(var atab: TDocuSignTab; const adocumentId, areceipientId: string; const PageNum: Integer; const atabtype: string);
procedure DocuSignTabAnchor(var atab: TDocuSignTab; const anchorStr: string;
  const OffsXmm: double = 0; const OffsYmm: double = 0; IgnoreIfNotPreset: Boolean = true);

function DocuSignRequestVoid(const voidReason: string): string;

// json parsers
function DocuSignParseError(const envjson: string; var ErrorCode, ErrorMessage: string): Boolean; overload;
function DocuSignParseError(j: TJSONObject; var ErrorCode, ErrorMessage: string): Boolean; overload;

function DocuSignJsonParseAccount(j: TJSONObject; var la: TLoginAccountV2): Boolean;
function DocuSignJsonParseAccounts(j: TJSONArray; var la: TLoginAccountsV2): Integer; overload;
function DocuSignJsonParseAccounts(const rest: string; var la: TLoginAccountsV2): Integer; overload;

function DocuSignJsonParseEnvelope(j: TJSONObject; var en: TEnvelopeInfoV2): Boolean; overload;
function DocuSignJsonParseEnvelope(const envjson: string; var en: TEnvelopeInfoV2): Boolean; overload;
function DocuSignJsonParseStatus(j: TJSONObject; var Status: TStatusV2): Boolean; overload;
function DocuSignJsonParseStatus(const rest: string; var Status: TStatusV2): Boolean; overload;
function DocuSignJsonParseRecipient(j: TJSONObject; var r: TDocuSignRecipient): boolean;
function DocuSignJsonParseRecipients(j: TJSONObject; var rp: TDocuSignRecipients): Boolean; overload;
function DocuSignJsonParseRecipients(const rest: string; var rp: TDocuSignRecipients): Boolean; overload;

function DocuSignJsonParseViewURL(j: TJSONObject; var url: string): Boolean; overload;
function DocuSignJsonParseViewURL(const rest: string; var url: string): Boolean; overload;

//url generators
function DocuSignURLStatusStr(const st: TDocuSignStatusCheck): string;
function DocuSignURLGetDoc(const st: TDocuSignGetDoc): string;

//utilities
function DocuSignCheckFromDate(fromdate: TDateTime; const fromtostatus: string = 'changed'): TDocuSignStatusCheck;
function DocuSignCheckDateRange(fromdate, todate: TDateTime; const fromtostatus: string = 'changed'): TDocuSignStatusCheck;
procedure DocuSignCheckInit(var ds: TDocuSignStatusCheck; flags: TDocuSignStatusCheckFlags = []);
function DocuSignVoid(const EnvelopeId, Reason: string): TDocuSignVoid;

// json generators for tabs
function DocuSignArrayOfTabsStr(const tabs: array of TDocuSignTab; tabcount: Integer; const SignerId: string): string;
function DocuSignJsonTabStr(const tab: TDocuSignTab; wr: TJsonWriter = nil): string;

procedure TestRadioGroups;

//
function DocuSignReqParseDocuments(const json: string; str: TStrings): Boolean;

implementation

function JsonStrParse(const js: string): TJSONData;
var
  p : TJSONParser;
  st : TStringStream;
begin
  try
    st:=TStringStream.Create(js);
    p:=nil;
    try
      p:=TJSONParser.Create(st);
      Result:=p.Parse;
    finally
      st.Free;
      p.Free;
    end;
  except
    Result:=nil;
  end;
end;


procedure DocuSignCheckInit(var ds: TDocuSignStatusCheck; flags: TDocuSignStatusCheckFlags = []);
begin
  FillChar(ds, sizeof(ds), 0);
  ds.flags:=flags;
end;

function DocuSignCheckFromDate(fromdate: TDateTime; const fromtostatus: string): TDocuSignStatusCheck;
begin
  DocuSignCheckInit(Result, [dcsFromDate]);
  Result.from_date:=fromDate;
  Result.from_to_status:=fromtostatus;
end;

function DocuSignCheckDateRange(fromdate, todate: TDateTime; const fromtostatus: string): TDocuSignStatusCheck;
begin
  DocuSignCheckInit(Result, [dcsFromDate, dcsToDate]);
  Result.from_date:=fromdate;
  Result.to_date:=todate;
  Result.from_to_status:=fromtostatus;
end;

function DocuSignVoid(const EnvelopeId, Reason: string): TDocuSignVoid;
begin
  Result.EnvelopeId:=EnvelopeId;
  Result.Reason:=Reason;
end;

function DocuSignParseError(const envjson: string; var ErrorCode,
  ErrorMessage: string): Boolean;
var
  j : TJSONData;
begin
  j:=JsonStrParse(envjson);
  try
    Result:=Assigned(j) and (j.JSONType = jtObject);
    if not Result then begin
      ErrorCode:='!CLIENT_INTERNAL';
      ErrorMessage:='Unexpected Server response (non Rest protocol used)';
      Exit;
    end;
    Result:=DocuSignParseError( TJSONObject(j), ErrorCode, ErrorMessage);
  finally
    j.Free;
  end;

end;

function DocuSignParseError(j: TJSONObject; var ErrorCode, ErrorMessage: string): Boolean;
var
  i  : integer;
  nm : string;
  v  : string;
begin
  Result:=false;
  ErrorCode:='';
  ErrorMessage:='';
  for i:=0 to j.Count-1 do begin
    nm:=AnsiLowerCase(j.Names[i]);
    v:=GetJsonStr(j.Items[i]);
    if (nm='') then Continue;
    case nm[1] of
      'e': if nm='errorcode' then begin ErrorCode:=v; Result:=True; end;
      'm': if nm='message' then ErrorMessage:=v;
    end;
  end;
end;

function DocuSignJsonLoginStr(const aname, apassword, akey: string): string;
begin
  Result:=
    '{'
    +'"Username":"'+JsonStr(aname)+'",'
    +'"Password":"'+JsonStr(apassword)+'",'
    +'"IntegratorKey":"'+JsonStr(akey)+'"'
    +'}';
end;

function DocuSignIDAuth(const r: TDocuSignRecipient): string;
var
  addr, dob : string;
  dd : string;
begin
  Result:='';
  if (r.idauthaddress1='') and (r.idauthdateofbirth='') then begin
    Exit;
  end;

  dd := r.idauthdisplaycode;
  if dd = '' then dd := 'DoNotDisplay'
  else dd := JsonStr(dd);

  if (r.idauthaddress1<>'') then
    addr:='"addressInformationInput": {'
     +'"addressInformation": {  '
        +'"street1": "'+JsonStr(r.idauthaddress1)+'",'
        +'"street2": "'+JsonStr(r.idauthaddress2)+'",'
        +'"city": "'+JsonStr(r.idauthcity)+'",'
        +'"state": "'+JsonStr(r.idauthstate)+'",'
        +'"zip": "'+JsonStr(r.idauthzip)+'"'
     +'}'
     +',"displayLevelCode": "'+dd+'"}'
  else
    addr:='';

  if (r.idauthdateofbirth<>'') then begin
    dob:='"dobInformationInput": { "dateOfBirth": "'+JsonStr(r.idauthdateofbirth)+'"'
    +',"displayLevelCode": "'+dd+'"}';
  end else
    dob:='';
  Result:=addr;
  if dob<>'' then begin
    if Result<>'' then Result:=Result+',';
      Result:=Result+dob;
  end;
  Result:='"idCheckInformationInput":{'+Result+'}';
end;

function DocuSignSmsAuth(const r: TDocuSignRecipient): string;
var
  i : integer;
begin
  Result:='';
  if r.smsPhoneCount=0 then Exit;

  for i:=0 to r.smsPhoneCount-1 do begin
    if i>0 then Result:=Result+',';
    Result:=Result+'"'+r.smsPhoneCheck[i]+'"';
  end;
  if Result<>'' then
    Result:='"smsAuthentication":{"senderProvidedNumbers":['+Result+']}';
end;

function DocuSignArrayOfRecipientsStr(const res: array of TDocuSignRecipient; len: Integer;
  const tabs: array of TDocuSignTab; const tabCount: integer): string;
var
  i,j : Integer;
  stb : string;
const
  BoolStr : array [Boolean] of string = ('false','true');
begin
  Result:='';
  for i:=0 to len-1 do begin
    if i>0 then Result:=Result+',';
    Result:=Result
      +'{'
      +'"email":"'+JsonStr(res[i].email)+'"'
      +',"name":"'+JsonStr(res[i].name)+'"';
    if res[i].id<>'' then
      Result:=Result+',"recipientId":"'+Jsonstr(res[i].id)+'"';

    if res[i].requireIdLookup then begin
      //Result:=Result+#10
      Result:=Concat(Result,',"requireIDLookup":true',',"idCheckConfigurationName":"',JsonStr(res[i].idCheckConfigurationName),'"');

      stb:=DocuSignIDAuth(res[i]);
      if stb<>'' then Result:=Concat(Result,',',stb);
      stb:=DocuSignSmsAuth(res[i]);
      if stb<>'' then Result:=Concat(Result,',',stb);
    end;

    if res[i].accesscode<>'' then begin
      Result:=Result+',"accessCode":"'+JsonStr(res[i].accesscode)+'"';
      Result:=Result+',"addAccessCodeToEmail":'+BoolStr[res[i].accesscodetoemail];
    end;

    if res[i].clientUserID<>'' then begin
      Result:=Result+',"clientUserId":"'+JsonStr(res[i].clientUserID)+'"';
    end;

    if res[i].routingOrder>0 then begin
      Result:=Result+',"routingOrder":'+IntToStr(res[i].routingOrder);
    end;

    if res[i].id<>'' then begin
      stb:=DocuSignArrayOfTabsStr(tabs, tabCount, res[i].id);
      if stb<>'' then
        Result:=Result+',"tabs":{'+stb+'}';
    end;

    Result:=Result+'}';
  end;
end;

function DocuSignArrayOfDocumentsStr(const documents: array of TDocuSignDocument; len: Integer): string;
var
  i : integer;
begin
  Result:='';
  for i:=0 to len - 1 do begin
    if i>0 then Result:=Result+',';
    Result:=Result
      +'{'
      +'"name":"'+JsonStr(documents[i].name)+'"'
      +',"documentId":"'+JsonStr(documents[i].id)+'"'
      +',"order":"'+IntToStr(i+1)+'"'
      +'}';
  end;
end;

function DocuSignJsonTabStr(const tab: TDocuSignTab; wr: TJsonWriter): string;
var
  isAnchor: Boolean;
  ownwr: Boolean;
begin
  ownwr:=not Assigned(wr);
  if ownwr then wr := TJsonWriter.Create;

  try
    wr.StrVal('documentId', tab.documentId);
    wr.StrVal('pageNumber', tab.pageNumber);
    //do not show recipientID, as it is (not?)required by API specification
    // Actually the field is marked as "required", BUT
    // samples doesn't require it to be filled out.
    // Also array of Tabs shows up under a specific "signer". Thus binding
    // is specified by the actual location of the "tabs" array
    //wr.StrVal('recipientId', tab.recipientId);

    // mandatory pagenumber and document
    isAnchor:=tab.anchorString<>'';
    if isAnchor then begin
      wr.StrVal('anchorString', tab.anchorString);
      if tab.anchorXOffset<>0 then
        JsonWriterFloat(wr, 'anchorXOffset', tab.anchorXOffset, DocuSignFloatFmt);
      if tab.anchorYOffset<>0 then
        JsonWriterFloat(wr, 'anchorYOffset', tab.anchorYOffset, DocuSignFloatFmt);
      wr.BoolVal('anchorIgnoreIfNotPresent', tab.anchorIgnoreIfNotPresent);
      wr.StrValNotEmpty('anchorUnits', tab.anchorUnits);
    end else begin
      JsonWriterFloat(wr, 'xPosition', tab.xPosition, DocuSignFloatFmt);
      JsonWriterFloat(wr, 'yPosition', tab.yPosition, DocuSignFloatFmt);
    end;

    if tab.conditionalParentLabel<>'' then begin
      wr.StrValNotEmpty('conditionalParentLabel', tab.conditionalParentLabel);
      wr.StrValNotEmpty('conditionalParentValue', tab.conditionalParentValue);
    end;
    wr.StrValNotEmpty('name', tab.name);
    if tab.height>0 then  wr.StrVal( 'height', Format( DocuSignFloatFmt, [tab.height] ));

    wr.StrValNotEmpty('value', tab.value);
    if tab.width>0 then
      wr.StrVal( 'width', Format( DocuSignFloatFmt, [tab.width] ));
    wr.StrValNotEmpty('tabLabel', tab.tabLabel);

    wr.BoolVal('required', tab.required);
    wr.BoolVal('optional', not tab.required);

    if tab._lockset then begin
      wr.BoolVal('locked', tab.locked);
    end;
    wr.StrValNotEmpty('scaleValue', tab.scaleValue);

    Result:=wr.Finish;
  finally
    if ownwr then wr.Free;
  end;
end;

type

  { TRadioGroup }

  TRadioGroup = clasS(TObject)
    groupname : string;
    idxCount  : integer;
    idx       : array of Integer;
    constructor Create(const agroupname: string);
    procedure AddIndex(aidx: integer);
  end;

  { TRadioGroups }

  TRadioGroups = class(TObject)
  private
    fCount: Integer;
    fGrp  : array of TRadioGroup;
  public
    destructor Destroy; override;
    function GetGroup(const groupName: string): TRadioGroup;
  end;

constructor TRadioGroup.Create(const agroupname: string);
begin
  inherited Create;
  groupname:=agroupName;
end;

procedure TRadioGroup.AddIndex(aidx: integer);
begin
  if idxCount= length(idx) then begin
    if idxCount = 0 then SetLength(idx, 4)
    else SetLength(idx, idxCount * 2);
  end;
  idx[idxCount]:=aidx;
  inc(idxCount);
end;

destructor TRadioGroups.Destroy;
var
  i : integer;
begin
  for i:=0 to fCount-1 do fGrp[i].Free;
  inherited Destroy;
end;

function TRadioGroups.GetGroup(const groupName: string): TRadioGroup;
var
  i : integer;
  l : string;
begin
  l:=AnsiLowerCase(groupName);
  for i:=0 to fCount-1 do
    if AnsiLowercase(fGrp[i].groupname)=l then begin
      Result:=fGrp[i];
      Exit;
    end;
  if fCount=length(fGrp) then begin
    if fCount=0 then SetLength(fGrp, 4)
    else SetLength(fGrp, fCount*2);
  end;
  Result:=TRadioGroup.Create(groupName);
  fGrp[fCount]:=Result;
  inc(fCount);
end;

procedure DocuSignJsonTabRadGroup(const tab: TDocuSignTab; wr: TJsonWriter);
begin
  wr.StrVal('documentId', tab.documentId);
  wr.StrVal('groupName', tab.tabLabel);
  //wr.StrVal('recipientId', t.recipientId);
  wr.BoolVal('requireInitialOnSharedChange', tab.requireInitialOnSharedTabChange);
  wr.BoolVal('shared', tab.shared);
  if tab.conditionalParentLabel<>'' then begin
    wr.StrValNotEmpty('conditionalParentLabel', tab.conditionalParentLabel);
    wr.StrValNotEmpty('conditionalParentValue', tab.conditionalParentValue);
  end;
  wr.BoolVal('required', tab.required);
  wr.BoolVal('optional', not tab.required);
end;

procedure DocuSignJsonTabRadio(const tab: TDocuSignTab; wr: TJsonWriter);
var
  isAnchor :Boolean;
begin
  wr.StrVal('pageNumber', tab.pageNumber);

  // mandatory pagenumber and document
  isAnchor:=tab.anchorString<>'';
  if isAnchor then begin
    wr.StrVal('anchorString', tab.anchorString);
    if tab.anchorXOffset<>0 then
      JsonWriterFloat(wr, 'anchorXOffset', tab.anchorXOffset, DocuSignFloatFmt);
    if tab.anchorYOffset<>0 then
      JsonWriterFloat(wr, 'anchorYOffset', tab.anchorYOffset, DocuSignFloatFmt);
    wr.BoolVal('anchorIgnoreIfNotPresent', tab.anchorIgnoreIfNotPresent);
    wr.StrValNotEmpty('anchorUnits', tab.anchorUnits);
  end else begin
    JsonWriterFloat(wr, 'xPosition', tab.xPosition, DocuSignFloatFmt);
    JsonWriterFloat(wr, 'yPosition', tab.yPosition, DocuSignFloatFmt);
  end;
  wr.BoolVal('required', tab.required);
  wr.BoolVal('selected', tab.selected);
  wr.StrValNotEmpty('value', tab.value);
end;

function DocuSignArrayOfTabsStr(const tabs: array of TDocuSignTab; tabcount: Integer; const SignerId: string): string;
var
  i  : integer;
  j  : integer;
  k  : integer;
  t  : TStringList;
  vs : string;
  tp : string;
  grp : TRadioGroups;
  wr  : TJsonWriter;
begin
  t:=TStringList.Create;
  grp:=TRadioGroups.Create;
  try
    for i:=0 to tabcount-1 do begin
      if (SignerId<>'') and not ((tabs[i].recipientId='') or (tabs[i].recipientId=SignerId)) then begin
        Continue;
      end;
      tp:=tabs[i]._tabtype;
      if tp='' then Continue;
      if tp='radio' then begin
        grp.GetGroup(tabs[i].tabLabel).AddIndex(i);
        Continue;
      end;

      vs:=t.Values[tp];
      if vs<>'' then vs:=vs+',';
      vs:=vs+'{'+DocuSignJsonTabStr( tabs[i] )+'}';
      t.Values[tp]:=vs;
    end;
    Result:='';
    for i:=0 to t.Count-1 do begin
      tp:=t.Names[i];
      vs:=t.ValueFromIndex[i];
      if i>0 then Result:=Result+',';
      Result:=Result+'"'+tp+'Tabs":['+vs+']';
    end;

    if grp.fCount>0 then begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+'"radioGroupTabs":[';
      wr:=TJsonWriter.Create;
      try
        for i:=0 to grp.fCount-1 do begin
          if grp.fGrp[i].idxCount=0 then Continue;
          wr.OpenObject('');
          DocuSignJsonTabRadGroup( tabs[ grp.fGrp[i].idx[0]], wr );
          wr.OpenArray('radios');
          for j:=0 to grp.fGrp[i].idxCount-1 do begin
            k:=grp.fGrp[i].idx[j];
            wr.OpenObject('');
            DocuSignJsonTabRadio( tabs[k], wr );
            wr.CloseObject;
          end;
          wr.CloseArray;
          wr.CloseObject;
        end;
        Result:=Result+wr.Finish+']';
      finally
        wr.Free;
      end;
    end;
  finally
    grp.Free;
    t.Free;
  end;

end;

function DocuSignRequestSignStr(const astatus: string; const request: TDocuSignRequest): string;
var
  j : string;
begin
  j:='{';
  if astatus<>'' then j:=j+'"status":"'+Jsonstr(astatus)+'",';
  j:=j+'"emailSubject":"'+JsonStr(request.subj)+'"';
  if request.body<>'' then
    j:=j+',"emailBlurb":"'+JsonStr(request.body)+'"';

  if request.doccount>0 then
    j:=j+',"documents":['+DocuSignArrayOfDocumentsStr( request.documents, request.doccount )+']';

  if (request.signcount > 0) or (request.cccount> 0) then begin
    // listing recipients
    j:=j+',"recipients":{ ';
    if request.signcount > 0 then begin
      j:=j+'"signers":['+DocuSignArrayOfRecipientsStr(request.signers, request.signcount, request.tabs, request.tabcount  )+']';
    end;
    if request.cccount > 0 then begin
      if request.signcount>0 then j:=j+',';
      j:=j+'"carbonCopies":['+DocuSignArrayOfRecipientsStr(request.carboncopes, request.cccount, request.tabs, request.tabcount  )+']';
    end;
    j:=j+'}';
  end;
  j:=j+'}'; // closing body

  Result:=j;
end;

function DocuSignRequestVoid(const voidReason: string): string;
var
  vr : string;
const
  TWO_SPACES = '  ';
  // For whatever reason (might be an error in Synapse or the implementation)
  // but DocuSign would not accept the reason request without additional two spaces
  // following json.
begin
  if voidReason=''
    then vr:=JsonStr('unspecified')
    else vr:=JsonStr(voidReason);
  Result:='{ "status":"voided", "voidedReason" : "'+vr+'" }'+TWO_SPACES;
end;

procedure DocuSignRequestInit(var request: TDocuSignRequest; const subject: string; const body: string = '');
begin
  FillChar(request, sizeof(request), 0);
  request.subj:=subject;
  request.body:=body;
end;

function DocuSignRequestAddSigner(var request: TDocuSignRequest; const email, name: string): string;
var
  i: Integer;
begin
  i:=request.signcount;
  if length(request.signers)=i then begin
    if i=0 then SetLength(request.signers,2)
    else SetLength(request.signers, i*2);
  end;
  request.signers[i].email:=email;
  request.signers[i].name:=name;
  request.signers[i].id:=IntToStr(i+1);
  request.signcount:=i+1;
  Result:=request.signers[i].id;
end;

function DocuSignRequestAddDocumentInt(var request : TDocuSignRequest; const filename: string; stream: TStream; const name: string): string;
var
  i : integer;
begin
  i:=request.doccount;
  if i=length(request.documents) then begin
    if i=0 then SetLength(request.documents, 4)
    else SetLength(request.documents, i*2);
  end;
  request.documents[i].id:=IntTostr(i+1);
  request.documents[i].name:=name;
  request.documents[i].filename:=filename;
  request.documents[i].stream:=stream;
  request.doccount:=i+1;
  Result:=request.documents[i].id;
end;

function DocuSignRequestAddDocument(var request : TDocuSignRequest; const filename, name: string): string; overload;
var
  nm : string;
begin
  nm:=name;
  if nm='' then nm:=ExtractFileName(filename);
  Result:=DocuSignRequestAddDocumentInt(request, filename, nil, nm);
end;

function DocuSignRequestAddDocument(var request : TDocuSignRequest; stream: TStream; const name: string): string; overload;
var
  nm: string;
begin
  nm:=name;
  if nm='' then nm:='file_'+IntTostr(request.doccount);
  Result:=DocuSignRequestAddDocumentInt(request, '', stream, nm);
end;

function DocuSignRequestAddCarbonCopy(var request: TDocuSignRequest;
  const email, name: string): string;
var
  i : integer;
begin
  i:=request.cccount;
  if length(request.carboncopes)=i then begin
    if request.cccount=0 then SetLength(request.carboncopes,2)
    else SetLength(request.carboncopes, i*2);
  end;
  request.carboncopes[i].email:=email;
  request.carboncopes[i].name:=name;
  request.carboncopes[i].id:='cc'+IntToStr(i+1);
  inc(request.cccount);
  Result:='';
  Result:=request.carboncopes[i].id;
end;

function DocuSignRequestAddTab(var request : TDocuSignRequest): Integer;
var
  i : integer;
begin
  i:=request.tabcount;
  if i=length(request.tabs) then begin
    if i=0 then SetLength(request.tabs, 4)
    else SetLength(request.tabs, i*2);
  end;
  request.tabs[i]._tabtype:='';
  request.tabs[i].documentId:='';
  request.tabs[i].recipientId:='';
  request.tabs[i].pageNumber:='';
  Result:=i;
  inc(request.tabcount);
end;

procedure DocuSignTabInit(var atab: TDocuSignTab; const adocumentId,
  areceipientId: string; const PageNum: Integer; const atabtype: string);
begin
  atab._tabtype:=atabtype;
  atab.documentId:=adocumentId;
  atab.recipientId:=areceipientId;
  atab.pageNumber:=IntToStr(PageNum);
  atab.xPosition := 0;
  atab.yPosition := 0;

  atab.anchorString:='';
  atab.anchorXOffset:=0.0;
  atab.anchorYOffset:=0.0;
  atab.anchorIgnoreIfNotPresent:=true;
  atab.anchorUnits:='';

  atab.conditionalParentLabel:='';
  atab.conditionalParentValue:='';

  atab.templateLocked   :=false;
  atab.templateRequired :=false;
  atab.mergeFieldXml:=''; //reserved
  atab.name :='';  // must be "CheckBox" for checkbox
                  // "Company" for company
                  // "Date Signed"
                  // "Text" for Date tab
                  // "Text" for Email tab
                  // "Full Name" for Full Name
  atab.requireInitialOnSharedTabChange:=false;
  atab.selected:=false;
  atab.shared:=false;

  atab.bold:=0.0;
  atab.font :='';
  atab.fontColor :='';
  atab.fontSize  :='';
  atab.italic := false;
  atab.underline := false;

  atab.tabLabel:='';
  atab.concealValueOnDocument:=false;
  atab.disableAutosize:=false;
  atab.required:=false;
  atab.value:='';
  atab.buttonText:='';
  atab.height := 0;
  atab.width  := 0;
end;

procedure DocuSignTabAnchor(var atab: TDocuSignTab; const anchorStr: string;
  const OffsXmm, OffsYmm: double; IgnoreIfNotPreset: Boolean);
begin
  atab.anchorString:=anchorStr;
  atab.anchorXOffset:=OffsXmm;
  atab.anchorYOffset:=OffsYmm;
  atab.anchorUnits:='mms';
  atab.anchorIgnoreIfNotPresent:=true;
end;

function DocuSignJsonParseAccount(j: TJSONObject; var la: TLoginAccountV2): Boolean;
var
  i  : integer;
  nm : string;
  v  : string;
begin
  for i:=0 to j.Count-1 do begin
    nm:=AnsiLowerCase(j.Names[i]);
    v:=GetJsonStr(j.Items[i]);
    if (nm='') then Continue;
    case nm[1] of
      'n': if nm='name' then la.name:=v;
      'a': if nm='accountid' then la.accountid:=v;
      'b': if nm='baseurl' then la.baseurl:=v;
      'i': if nm='isdefault' then la.isdefault:=v;
      'u': if nm='username' then la.username:=v
           else if nm='userid' then la.userid:=v;
      'e': if nm='email' then la.email:=v;
      's': if nm='sitedesription' then la.sitedescr:=v;
    end;
  end;
  Result:=True;
end;

function DocuSignJsonParseEnvelope(j: TJSONObject; var en: TEnvelopeInfoV2): Boolean;
var
  i  : integer;
  nm : string;
  v  : string;
begin
  for i:=0 to j.Count-1 do begin
    nm:=AnsiLowerCase(j.Names[i]);
    v:=GetJsonStr(j.Items[i]);
    if (nm='') then Continue;
    case nm[1] of
    //abcdefg
      'c': if nm='customfieldsuri' then en.customFieldsUri:=v
           else if nm='certificateuri' then en.certificateUri:=v;
      'd': if nm='documentsuri' then en.documentsUri:=v
           else if nm='documentscombineduri' then en.documentsCombinedUri:=v;
      'e': if nm='envelopeuri' then en.envelopeUri:=v
           else if nm='envelopeid' then en.envelopeId:=v;
      'n': if nm='notificationuri' then en.notificationUri:=v;
      's': if nm='status' then en.status:=v
           else if nm='statuschangeddatetime' then en.statusChangedDateTime:=v;
      'r': if nm='recipientsuri' then en.recipientsUri:=v;
      't': if nm='templatesuri' then en.templatesUri:=v;
    end;
  end;
  Result:=True;
end;

function DocuSignJsonParseEnvelope(const envjson: string; var en: TEnvelopeInfoV2): Boolean;
var
  j : TJSONData;
begin
  j:=JsonStrParse(envjson);
  try
    Result:=Assigned(j) and (j.JSONType = jtObject);
    if not Result then Exit;
    Result:=DocuSignJsonParseEnvelope( TJSONObject(j), en);
  finally
    j.Free;
  end;
end;

function DocuSignJsonParseStatus(j: TJSONObject; var Status: TStatusV2): Boolean;
var
  ji : TJSONData;
  nm : string;
  k  : Integer;
  i  : Integer;
  ja : TJSONArray;
begin
  Result:=Assigned(j);
  if not Result then Exit;

  for i:=0 to j.Count-1 do begin
    ji:=j.Items[i];
    nm:=ansiLowercase(j.Names[i]);
    if nm ='' then Continue;
    case nm[1] of
      'e': if nm ='envelopes' then begin
             if ji.JSONType<>jtArray then Continue;
             ja:=TJSONArray(ji);
             Status.envCount:=ja.Count;
             SetLength( Status.envelopes, Status.envCount );
             for k:=0 to ja.Count-1 do begin
               if ja.Items[k].JSONType = jtObject  then
                 DocuSignJsonParseEnvelope( TJSONObject(ja.Items[k]), Status.envelopes[k] );
             end;
           end;
    end;
  end;
end;

function DocuSignJsonParseStatus(const rest: string; var Status: TStatusV2): Boolean;
var
  j : TJSONData;
begin
  j:=JsonStrParse(rest);
  try
    Result:=Assigned(j) and (j.JSONType = jtObject);
    if not Result then Exit;
    Result:=DocuSignJsonParseStatus(TJSONObject(j), Status)
  finally
    j.Free;
  end;
end;

(*
{
  "signers": [
    {
      "isBulkRecipient": "false",
      "recipientSuppliesTabs": "true",
      "name": "Demeterey Boyarintsev",
      "email": "dboyarintsev@maxprocessing.com",
      "recipientId": "1",
      "recipientIdGuid": "436a695d-f5f0-4e5f-863a-25f40bf6947a",
      "requireIdLookup": "false",
      "userId": "d54fffc9-7779-48d9-adbc-35f178794666",
      "routingOrder": "1",
      "status": "sent",
      "totalTabCount": "0"
    }
  ],
  "agents": [],
  "editors": [],
  "intermediaries": [],
  "carbonCopies": [],
  "certifiedDeliveries": [],
  "inPersonSigners": [],
  "recipientCount": "1",
  "currentRoutingOrder": "1"
}
*)
function DocuSignJsonParseRecipient(j: TJSONObject; var r: TDocuSignRecipient): boolean;
var
  i  : integer;
  nm : string;
  v  : string;
begin
  if not Assigned(j) then begin
    Result:=false;
    Exit;
  end;

  for i:=0 to j.Count-1 do begin
    nm:=AnsiLowerCase(j.Names[i]);
    v:=GetJsonStr(j.Items[i]);
    if (nm='') then Continue;
    case nm[1] of
      'n': if nm='name' then r.name:=v;
      'e': if nm='email' then r.email:=v;
      'r': if nm='recipientid' then r.id:=v;
      'c': if nm='clientuserid' then r.clientUserID:=v
      //todo: add more fields
    end;
  end;
  Result:=True;
end;

function DocuSignJsonParseRecipients(j: TJSONObject; var rp: TDocuSignRecipients): Boolean;
var
  arr : TJSONArray;
  i,k : integer;
begin
  FillChar(rp, sizeof(rp), 0);
  arr:=TJSONArray(j.Find('signers', jtArray));
  Result:=Assigned(arr);
  if not Result then Exit;
  rp.signcount:=arr.Count;
  SetLength(rp.signers, rp.signcount);
  k:=0;
  for i:=0 to length(rp.signers)-1 do begin
    if arr.Items[i].JSONType<>jtObject then Continue;
    DocuSignJsonParseRecipient(TJSONObject(arr.Items[i]), rp.signers[k]);
    inc(k);
  end;
  rp.signcount:=k;
end;

function DocuSignJsonParseRecipients(const rest: string; var rp: TDocuSignRecipients): Boolean;
var
  j : TJSONData;
begin
  j:=JsonStrParse(rest);
  try
    Result:=Assigned(j) and (j.JSONType = jtObject);
    if not Result then Exit;
    Result:=DocuSignJsonParseRecipients(TJSONObject(j), rp);
  finally
    j.Free;
  end;
end;


function DocuSignJsonParseAccounts(const rest: string; var la: TLoginAccountsV2): Integer; overload;
var
  j  : TJSONData;
  jo : TJSONObject;
  i  : Integer;
begin
  j:=JsonStrParse(rest);
  try
    if not Assigned(j) or not (j is TJSONObject)  then begin
      Result:=0;
      Exit;
    end;
    jo:=TJSONObject(j);
    if jo.Count = 0 then Exit;

    for i:=0 to jo.Count-1 do
      if (AnsiLowerCase(jo.Names[i])='loginaccounts') and (jo.Items[i] is TJSONArray) then
      begin
        DocuSignJsonParseAccounts( TJSONArray(jo.Items[i]), la);
      end;
  finally
    j.Free;
  end;
end;

function DocuSignJsonParseAccounts(j: TJSONArray; var la: TLoginAccountsV2): Integer;
var
  i  : Integer;
begin
  if not Assigned(j) then begin
    Result:=0;
    Exit;
  end;
  SetLength(la, j.Count);
  for i:=0 to j.Count-1 do begin
    if not (j.Items[i] is TJSONObject) then Continue;
    DocuSignJsonParseAccount(TJSONObject(j.Items[i]), la[i]);
  end;
  Result:=j.Count;
end;

{ THTTPConn }

procedure THTTPConn.DoRequest(const url, method, headers: string; body: TStream
  );
var
  p : Int64;
begin
  if Assigned(fOnRequest) then begin
    try
      p:=body.Position;
      try
        fOnRequest(Self, url, method, headers, body);
      finally
        body.Position:=p;
      end;
    except
    end;
  end;
end;

procedure THTTPConn.AddPart(const data: string; const atype, adisposition, extrahdr: string);
var
  st : TStringStream;
begin
  st := TStringStream.Create(data);
  st.Position:=0;
  AddPart(st, true, atype, adisposition, extrahdr);
end;

{ TDocuSignRest }

procedure TDocuSignRest.AddRequestDataStr(const s: string; const ContentType,
  Disposition, ExtraHeaders: string);
begin
  fConn.AddPart(s, ContentType, Disposition, ExtraHeaders);
  // not nice and is ditry hack!
  LastReqData:=s;
end;

procedure TDocuSignRest.AddRequestData(st: TStream; OwnStream: Boolean;
  const ContentType, Disposition, ExtraHeaders: string);
begin
  fConn.AddPart(st, OwnStream, ContentType, Disposition, ExtraHeaders);
end;

function TDocuSignRest.SendRequest(const Url, HttpMethod{, ContentType,  Data}: string; var Resp: String): Boolean;
begin
  Result:=Sendrequest(Url, HttpMethod, '', Resp);
end;

function TDocuSignRest.SendRequest(const Url, HttpMethod, Body: string;
  var Resp: String): Boolean;
var
  extrahdr : string;
begin
  extrahdr :=
    'Accept:application/json'+#13#10
    +fAuthParam+#13#10;
    ;

  LastUrl:=Url;
  LastHdr:=ExtraHdr;
  LastMthd:=HttpMethod;

  if TraceFile<>'' then begin
    TraceData('Request >>> ');
    TraceData(HttpMethod);
    TraceData(Url);
    TraceData(ExtraHdr);
    TraceData(fConn.GetSendingData);
    TraceData(LineEnding)
  end;

  if body<>'' then begin
    if TraceFile<>'' then begin
      TraceData('Body: ');
      TraceData(body);
      TraceData('--- End of Body---');
    end;

    fConn.AddPart(body, 'application/json' {''}, '', '');
  end;

  fConn.Http(Url, HttpMethod, ExtraHdr, {body,} Resp, fHttpError);
  if TraceFile<>'' then begin
    TraceData('Response <<< ');
    TraceData('HTTP Code: '+IntToStr(fHttpError));
    TraceData(Resp);
  end;
  LastResponse:=Resp;
  if DebugLog and not isFileResp then
    WriteLn(Resp);

  Result:=not DocuSignParseError(Resp, ErrorCode, ErrorMsg);
  if not Result and isFileResp and DebugLog then Writeln(resp)
end;

procedure TDocuSignRest.TraceData(const s: string);
var
  dir : string;
  t   : string;
begin
  if (TraceFile='') or (s='') then Exit;
  if not Assigned(traceStream) then begin
    if not FileExists(TraceFile) then begin
      dir:=ExtractFileDir(TraceFile);
      if dir<>'' then try ForceDirectories(dir); except end;
      try
        traceStream:=TFileStream.Create(TraceFile, fmCreate);
        traceStream.Free;
      except
        traceStream:=nil;
      end;
    end;
    try
      traceStream:=TFileStream.Create(TraceFile, fmOpenReadWrite or fmShareDenyNone);
      traceStream.Position:=traceStream.Size;
    except
      traceStream:=nil;
    end;
  end;
  if Assigned(traceStream) then begin
    traceStream.Write(s[1], length(s));
    t:=LineEnding;
    traceStream.Write(t[1], Length(t));
  end;
end;

function TDocuSignRest.PostEditRecipients(const envelopeid: string;
  const rawData: string): Boolean;
var
  u : string;
  rd : string;
begin
  u:=GetRecipientsURL(envelopeid);
  u:=RemoveTrailingSlash(fbaseurl)+u;
  Result:=SendRequest(u, 'PUT', rawdata, rd);
end;

constructor TDocuSignRest.Create(AConn: THTTPConn);
begin
  inherited Create;
  fConn:=AConn;
end;

destructor TDocuSignRest.Destroy;
begin
  fConn.Free;
  traceStream.Free;
  inherited Destroy;
end;

function TDocuSignRest.Login(const aloginurl, aname, apassword, akey: string): Boolean;
var
  res   : string;
begin
  floginurl:=aloginurl;
  // initializing authentication parameter
  fAuthParam:='X-DocuSign-Authentication:'+DocuSignJsonLoginStr(aname, apassword, akey);

  // sending no extra data, but need to set Content-Type to json
  AddRequestDataStr('', 'application/json', '','');
  Result:=SendRequest(floginurl+'/restapi/v2/login_information','GET', res);

  if Result then begin
    DocuSignJsonParseAccounts(res, logins);
    if length(logins)>0 then begin
      fbaseurl:=logins[0].baseurl;
    end;

  (* Example of error messages:
  {
    "errorCode": "INVALID_TOKEN_FORMAT",
    "message": "The security token format does not conform to expected schema."
  }

  {
    "errorCode": "PARTNER_AUTHENTICATION_FAILED",
    "message": "The specified Integrator Key was not found or is disabled."
  }

  *)
    Result:=True;
  end;
end;

function TDocuSignRest.GetStatus(const accountid: string;
  StatusCheck: TDocuSignStatusCheck; var st: TStatusV2): Boolean;
var
  u : string;
  r : string;
begin
  if StatusCheck.flags=[] then begin
    Result:=False;
    Exit;
  end;
  u:=DocuSignURLStatusStr(StatusCheck);
  u:=RemoveTrailingSlash(fbaseurl)+u;

  Result:=SendRequest(u, 'GET', r);
  if Result then
    Result:=DocuSignJsonParseStatus(r, st);
end;

function TDocuSignRest.GetRecipients(const envelopeid: string;
  var rp: TDocuSignRecipients): Boolean;
var
  u : string;
  r : string;
begin
  FillChar(rp, sizeof(rp),0);
  if (envelopeid='') then begin
    Result:=False;
    Exit;
  end;
  u:=GetRecipientsURL(envelopeid);;

  Result:=SendRequest(u, 'GET', r);
  if Result then
    Result:=DocuSignJsonParseRecipients(r, rp);
end;

function TDocuSignRest.EditRecipients(const envelopeid: string;
  const signers: array of TDocuSignRecipient; signCount: integer): Boolean;
var
  data : string;
  i    : integer;
begin
  data:='{'#10;
  if signCount>0 then begin
    data:=data+'"signers": ['#10;
    for i:=0 to signCount-1 do begin
      if i>0 then data:=data+',';
      data:=data+'{'+#10+'"recipientId":"'+JsonStr(signers[i].id)+'"';
      if signers[i].name<>'' then
        data:=data+ ',"name":"'+JsonStr(signers[i].name)+'"';
      data:=data+ ',"email":"'+JsonStr(signers[i].email)+'"';
      data:=data+#10+'}'+#10;
    end;
    data:=data+']'+#10;
  end;
  data:=data+'}'+#10;
  data:=data+'}'+#10;
(*

    {
  "signers": [
    {
      "recipientId": "1",
      "email": "dboyarintsev@maxprocessing.com"
    }
  ]
} *)
  Result:=PostEditRecipients(envelopeid, data);

end;

function TDocuSignRest.RequestSign(const request: TDocuSignRequest; var env: TEnvelopeInfoV2): Boolean;
var
  st : string;
  fs : TFileStream;
  r  : string;
  i  : Integer;
  fd : string;
begin
  Result := request.subj<>'';
  if not Result then Exit;

  //  "message": "The Envelope is not Complete. A Complete Envelope Requires Documents, Recipients, Tabs, and a Subject Line. Envelope definition missing
  (*st:='{"status": "sent","emailBlurb": "Test Email Body", "emailSubject": "Test Email Subject", '
   +' "documents": [ { "name": "test.pdf", "documentId": "1","order": "1" }  ], '
   +' "recipients": {   "signers": [  {  "email": "dboyarintsev@maxprocessing.com ",  "name": "John Doe", "recipientId": "1", '
   + ' "tabs": {  "signHereTabs": [ { "xPosition": "100", "yPosition": "100", "documentId": "1", "pageNumber": "1" } ]  } }  ]   } } ';

          {"status":"sent","emailSubj":"Hello World","emailBlurb":"Ok, so this is another email that goes out there!\t\nANJOY!\t\n","docum
   ents":[{"name":"document.pdf","documentId":"1","order":"1"}],"recipients":{ "signers":[{"email":"dboyarintsev@maxprocessing.com"
   ,"name":"Dmitry Boyarintsev","recipientId":"1"}]}}
  writeln(st);*)

  st:=DocuSignRequestSignStr('sent', request);
  if DebugLog then begin
    writeln('Send request:');
    writeln(st);
  end;

  AddRequestDataStr(st, 'application/json', 'form-data', '');
  for i:=0 to request.doccount-1 do begin
    fd:=Format('file; filename="%s"; documentid=%s',[request.documents[i].name, request.documents[i].id]);
    if request.documents[i].stream = nil then begin
      if DebugLog then begin
        writeln('openning file: ', request.documents[i].filename);
      end;
      fs := TFileStream.Create(request.documents[i].filename, fmOpenRead or fmShareDenyNone);

      AddRequestData(fs, true, 'application/pdf', fd, '');
    end else begin
      AddRequestData(request.documents[i].stream , false, 'application/pdf', fd, '');
    end;
  end;

  r:='';
  Result:= SendRequest( GetEnvelopesURL, 'POST', r);
  if Result then
    Result := DocuSignJsonParseEnvelope(r, env);
end;

function TDocuSignRest.RequestSignRaw(const rawData, rawFileDir: string; var env: TEnvelopeInfoV2): Boolean;
var
  fl : TStringList;
  nm : string;
  id : string;
  fd : string;
  i  : Integer;
  fs : TFileStream;
  r  : string;
  dir : string;
begin
  dir:=rawFileDir;
  if (dir='') then dir:=GetCurrentDir;
  dir:=IncludeTrailingPathDelimiter(dir);

  fl := TStringList.Create;
  try
    Result:=DocuSignReqParseDocuments(rawData, fl);
    if not Result  then Exit;
    if fl.Count=0 then begin
      writeln('no files!');
      Exit;
    end;
    if DebugLog then begin
      writeln('Send request:');
      writeln(rawData);
    end;

    AddRequestDataStr(rawData, 'application/json', 'form-data', '');

    for i:=0 to fl.Count-1 do begin
      id:=fl.Names[i];
      nm:=fl.ValueFromIndex[i];
      fd:=Format('file; filename="%s"; documentid=%s',[nm, id]);
      fs:=TFileStream.Create(dir+nm, fmOpenRead or fmShareDenyNone);
      AddRequestData(fs, true, 'application/pdf', fd, '');
    end;
  finally
    fl.Free;
  end;

  r:='';
  Result:= SendRequest( GetEnvelopesURL, 'POST', r);
  if Result then
    Result := DocuSignJsonParseEnvelope(r, env);
end;

function TDocuSignRest.GetDocument(doc: TDocuSignGetDoc; dst: TStream): Boolean;
var
  resp : string;
begin
  isFileResp:=true;
  try
    Result:=SendRequest( RemoveTrailingSlash(fbaseurl)+DocuSignURLGetDoc(doc), 'GET', resp);
    if Result and DebugLog then writeln('file size: ', length(resp));
    if Assigned(dst) and (length(resp)>0) then
      dst.Write(resp[1], length(resp));
  finally
    isFileResp:=false;
  end;
end;

function TDocuSignRest.Void(void: TDocuSignVoid): Boolean;
var
  u : string;
  r : string;
  st : string;
begin
  u:=GetEnvelopeURL(void.EnvelopeId);
  st:=DocuSignRequestVoid(void.Reason);
  AddRequestDataStr(st, 'application/json', 'form-data', '');
  Result:=SendRequest(u, 'PUT', r);
end;

function TDocuSignRest.GetRecpViewURL(const EnvId: string;
  const cl: TDocuSignRecipient; const ReturnURL: string;
  const AuthMethod: String): string;
var
  u     : string;
  rsp   : string;
  body  : string;
begin
  Result:='';
  if (cl.clientUserID='') or (EnvId='') then begin
    if DebugLog then
      WriteLn('no client: "',cl.clientUserID,'" or envelope id: "',EnvID,'"');
    Exit;
  end;

  u:=GetEnvelopeURL(EnvId)+UrlViewRecp;
  body:='{"UserName":"'+JsonStr(cl.name)+'"'
       +',"email":"'+JsonStr(cl.email)+'"'
       +',"recipientId":"'+JsonStr(cl.id)+'"'
       +',"clientUserId":"'+JsonStr(cl.clientUserID)+'"';
  if AuthMethod<>'' then
    body:=body+',"authenticationMethod": "'+JsonStr(AuthMethod)+'"'
  else
   body:=body+',"authenticationMethod": "email"';

  if ReturnURL<>'' then
    body:=body+',"returnUrl": "'+JsonStr(ReturnURL)+'"'
  else
    body:=body+',"returnUrl": "#"';
  body:=body+'}'+#13+#10  ;
  rsp:='';

  if not SendRequest(u, 'POST', body, rsp) then Exit;

  if not DocuSignJsonParseViewURL(rsp, Result) then
    Result:='';
end;


function TDocuSignRest.GetAccountId: string;
begin
  if length(logins)>0 then Result:=logins[0].accountid
  else Result:='';
end;

function TDocuSignRest.RawSend(const Url, HttpMethod, Body: string;
  var Resp: String): Boolean;
begin
  Result:=SendRequest(Url, HttpMethod, Body, Resp);
end;

function TDocuSignRest.GetEnvelopesURL: string;
begin
  Result:=RemoveTrailingSlash(fbaseurl)+UrlEnvelopes;
end;

function TDocuSignRest.GetEnvelopeURL(const envelopeid: string): string;
begin
  if envelopeid='' then
    Result:=GetEnvelopesURL
  else
    Result:=GetEnvelopesURL+'/'+envelopeid;
end;

function TDocuSignRest.GetRecipientsURL(const envelopeid: string
  ): string;
begin
  Result:=GetEnvelopeURL(envelopeid)+UrlRecipients;
end;

function TDocuSignRest.GetTabsURL(const envelopeid, recipientid: string
  ): string;
begin
  Result:=GetEnvelopeURL(envelopeid)+UrlRecipients+'/'+encodeURIComponent(recipientid)+'/tabs';
end;

function DocuSignJsonParseViewURL(j: TJSONObject; var url: string): Boolean;
var
  jurl : TJSONData;
begin
  jurl:=j.Find('url');
  if Assigned(jurl) then begin
    url:=jurl.AsString;
    Result:=true
  end else begin
    url:='';
    Result:=false;
  end;
end;

function DocuSignJsonParseViewURL(const rest: string; var url: string): Boolean;
var
  j : TJSONData;
begin
  j:=JsonStrParse(rest);
  try
    Result:=Assigned(j) and (j.JSONType = jtObject);
    if not Result then Exit;
    Result:=DocuSignJsonParseViewURL(TJSONObject(j), url);
  finally
    j.Free;
  end;
end;

function DocuSignURLStatusStr(const st: TDocuSignStatusCheck): string;
var
  u : string;
begin
  if (st.flags=[]) then begin
    Result:='';
    Exit;
  end;
  u := '/envelopes?';
  if dcsFromDate in st.flags then
    u := u + 'from_date='+encodeURIComponent(FormatDateTime('MM/DD/YYYY', st.from_date))+'&';
  if st.from_to_status <>'' then
    u := u + 'from_to_status='+encodeURIComponent(st.from_to_status);
  if dcsToDate in st.flags then begin
    u := u + 'to_date='+encodeURIComponent(FormatDateTime('MM/DD/YYYY', st.from_date))+'&';
    u := u + 'to_status='+encodeURIComponent(st.to_status)+'&';
  end;
  Result:=RemoveTrailingAmp(u);
end;

function DocuSignURLGetDoc(const st: TDocuSignGetDoc): string;
var
  u : string;
const
  BollStr : array [Boolean] of string = ('false', 'true');
begin
  u:='/envelopes/'+encodeURIComponent(st.envelopeId)+'/documents/';
  if st.documentId=''
    then u:=u+'combined'
    else u:=u+encodeURIComponent(st.documentId);
  u:=u
    +'?show_changes='+BollStr[st.show_changes]
    +'&watermark='+BollStr[st.watermark]
    +'&certificate='+BollStr[st.certificate];
  Result:=u
end;

procedure TestRadioGroups;
var
  tabs : array of TDocuSignTab;
  i    : Integer;
begin
  SetLength(tabs, 4);
  for i:=0 to length(tabs)-1 do begin
    tabs[i]._tabtype:='radio';
    tabs[i].tabLabel:='HappyGroup';
    tabs[i].recipientId:='1';
    tabs[i].documentId:='1';
  end;
  writeln( DocuSignArrayOfTabsStr ( tabs, length(tabs), ''));
end;

function DocuSignReqParseDocuments(const json: string; str: TStrings): Boolean;
var
  d   : TJSONData;
  obj : TJSONObject;
  nm  : string;
  id  : string;
  arr : TJSONArray;
  doc : TJSONObject;
  j   : Integer;
  i   : Integer;
begin
  d:=JsonStrParse(json);
  Result:=Assigned(d) and Assigned(str);
  if not Result then Exit;
  try
    if d.JSONType <> jtObject then Exit;
    obj:=TJSONObject(d);
    for i:=0 to obj.Count-1 do begin
      nm:=AnsiLowerCase(obj.Names[i]);
      if nm='documents' then begin
        if obj.Items[i].JSONType<>jtArray then Exit;
        arr := TJSONArray(obj.Items[i]);
        for j:=0 to arr.Count-1 do begin
          doc:=TJSONObject(arr.Items[j]);
          if doc.JSONType <> jtObject then Continue;
          nm:=doc.Get('name','');
          id:=doc.Get('documentId','');
          if (id<>'') and (nm<>'') then str.Values[id]:=nm;
        end;
        Exit;
      end;
    end;
  finally
    d.Free;
  end;
end;

initialization
  // forcing each float to be an integer
  if DocuSignIntCoord then DocuSignFloatFmt := '%0.0f';

end.

