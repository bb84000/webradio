{*******************************************************************************
  radios1 - Radios list and FRadios form
  bb - sdtp - january 2023
  Uses Indy DNS Resolver and API from Radio-Browser.info
********************************************************************************}
unit Radios1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ExtDlgs, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, lazbbutils, fileutil,
  IdDNSResolver, fphttpclient, fpopenssl, openssl, opensslsockets, lclintf,
  Menus, lazbbinifiles, ExProgressbar ;

type

  TChampsCompare = (cdcName, cdcOrder, cdcUrl, cdcComment, cdcNone);
  TSortDirections = (ascend, descend);
  TSaveType = (selection, all);

  PRadio = ^TRadio;
  TRadio= record
    order : Integer;
    name, url, comment, favicon, faviconurl: string;
    // partial save, tag is true
    tag: Boolean;
    UID: Int64;
  end;

  TRadiosList = class(TList)
  private
    FOnChange: TNotifyEvent;
    FSortType: TChampsCompare;
    FSortDirection: TSortDirections;
    FAppName: string;
    FPresets: array [1..20] of Integer;
    procedure SetSortDirection(Value: TSortDirections);
    procedure SetSortType (Value: TChampsCompare);
    procedure DoSort;
    function SaveItem(iNode: TDOMNode; sname, svalue: string): TDOMNode;
  public
    Duplicates : TDuplicates;
    constructor Create (AppName: String);
    procedure Delete (const i : Integer);
    procedure DeleteMulti (j, k : Integer);
    procedure Reset;
    function GetPreset(Index: Integer): Integer;
    procedure SetPreset(Index: Integer; value: Integer);
    procedure AddRadio(Radio : TRadio);
    procedure ModifyRadio (const i: integer; Radio: TRadio);
    procedure ModifyField (const i: integer; field: string; value: variant);
    function GetItem(const i: Integer): TRadio;
    function FindbyUID(value: int64; var ndx: Int64): TRadio;
    function FindbyUID(value: int64): TRadio;
    function FindByName(value: String; var ndx: int64): TRadio;
    function FindByName(value: String): TRadio;
    function GetItemFieldString(const i: Integer; field: string): String;
    function SaveToXMLnode(iNode: TDOMNode; typ: TSaveType= all): Boolean;
    function SaveToXMLfile(filename: string; typ: TSaveType= all): Boolean;
    function LoadXMLnode(iNode: TDOMNode): Boolean;
    function LoadXMLfile(filename: string): Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property SortDirection: TSortDirections read FSortDirection write SetSortDirection default ascend;
    property AppName: string read FAppName write FAppName;
    Property SortType : TChampsCompare read FSortType write SetSortType default cdcNone;
    property Presets [Index: Integer]: Integer read GetPreset write SetPreset;
  end;


  { TFRadios }
  // Event to play radio in main form
  TOnPlayEvent = procedure(Sender: TObject; rad: TRadio) of object;

  TShowMode = (smEdit, smSearch);

  TFRadios = class(TForm)
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    BtnApply: TBitBtn;
    EComment: TEdit;
    EFaviconURL: TEdit;
    ELimit: TEdit;
    ESearchName: TEdit;
    EFavicon: TEdit;
    EName: TEdit;
    ESearchCountry: TEdit;
    EUrl: TEdit;
    LFaviconURL: TLabel;
    LLimit: TLabel;
    LRadioBrowser: TLabel;
    LSearchName: TLabel;
    LBRadios: TListBox;
    LPresets: TLabel;
    LComment: TLabel;
    LFavicon: TLabel;
    Lname: TLabel;
    LSearchCountry: TLabel;
    LUrl: TLabel;
    MnuAddRadio: TMenuItem;
    MnuEditRadio: TMenuItem;
    MnuDeleteRadio: TMenuItem;
    MnuPlayRadio: TMenuItem;
    OPDLogo: TOpenPictureDialog;
    PButtons: TPanel;
    PMain: TPanel;
    PMain1: TPanel;
    PMnuList: TPopupMenu;
    PTools: TPanel;
    SBAddRadio: TSpeedButton;
    SBDeleteRadio: TSpeedButton;
    SBPlayRadio: TSpeedButton;
    SBEditRadio: TSpeedButton;
    SBPreset3: TSpeedButton;
    SBPreset4: TSpeedButton;
    SBFavicon: TSpeedButton;
    SBPreset1: TSpeedButton;
    SBPreset2: TSpeedButton;
    SBSearchBrwRadio: TSpeedButton;
    procedure EFaviconURLClick(Sender: TObject);
    procedure ERadioChange(Sender: TObject);
    procedure ESearchNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBRadiosClick(Sender: TObject);
    procedure LBRadiosMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LBRadiosSelectionChange(Sender: TObject; User: boolean);
    procedure LRadioBrowserClick(Sender: TObject);
    procedure LRadioBrowserMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LRadioBrowserMouseEnter(Sender: TObject);
    procedure LRadioBrowserMouseLeave(Sender: TObject);
    procedure LRadioBrowserMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PButtonsClick(Sender: TObject);
    procedure PMnuListPopup(Sender: TObject);
    procedure SBAddRadioClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure SBDeleteRadioClick(Sender: TObject);
    procedure SBEditRadioClick(Sender: TObject);
    procedure SBFaviconClick(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure SBPlayRadioClick(Sender: TObject);
    procedure SBSearchBrwRadioClick(Sender: TObject);
  private
    FOnPlay: TOnPlayEvent;
    NewRadio: Boolean;
    prevradio: Integer;
    apiurl: String;
    dns: TIdDNSResolver;
    MouseIndex: Integer;
    RadiosXML: TXMLDocument;
    procedure DefBtnState;
    procedure EditBtnState;
    procedure DisplayRadio;
    function getRadioBrowserApi(host, apiname: string; qt: TQueryType): string;
  public
    DefRadio, CurRadio: TRadio;
    RadioUID: Integer;
    RadioFavicon: String;
    Radios: TRadiosList;
    ShowMode: TShowMode;
    BrwRadios: TRadiosList;
    BrwSearch: Boolean;
    WebradioAppsData: String;
    sConfirmDeleteRadio: String;
    sRadioBrowserUnavail: string;
    sDNSHost: String;
    sSrvRecAPI, sARecAPI: string;
    sRadioBrowserURL: String;
    sNoRadioFound: String;
    OKBtn, YesBtn, NoBtn, CancelBtn: String;
    isearch: int64;
    function LoadRadios(filename: string): Boolean;
    function SaveRadios(filename:String): Boolean;
    procedure PopulateList(rdl: TRadiosList);
    property OnPlay: TOnPlayEvent read FOnPlay write FOnPlay;
    procedure Translate(LngFile: TBbIniFile);
  end;

var
  FRadios: TFRadios;
  ClesTri: array[0..4] of TChampsCompare;
  AFieldNames : array [0..7] of string  =('order',
                                           'name',
                                           'url',
                                           'comment',
                                           'faviconurl',
                                           'favicon',
                                           'tag',
                                           'uid');


implementation

{$R *.lfm}

//Radio list routines

function stringCompare(Item1, Item2: String): Integer;
begin
  result := Comparestr(UpperCase(Item1), UpperCase(Item2));
end;

function NumericCompare(Item1, Item2: Integer): Integer;
begin
  if Item1 > Item2 then result := 1
  else
  if Item1 = Item2 then result := 0
  else result := -1;
end;

function CompareMulti(Item1, Item2: Pointer): Integer;
var
  Entry1, Entry2: PRadio;
  R, J: integer;
  ResComp: array[TChampsCompare] of integer;
begin
  Entry1:= PRadio(Item1);
  Entry2:= PRadio(Item2);
  ResComp[cdcNone]  := 0;
  ResComp[cdcName]  := StringCompare(Entry1^.name, Entry2^.name);
  ResComp[cdcUrl] := StringCompare(Entry1^.url , Entry2^.url);
  ResComp[cdcOrder]:= NumericCompare(Entry1^.Order, Entry2^.Order);

  R := 0;
  for J := 0 to high(ClesTri) do
  begin
    if ResComp[ClesTri[J]] <> 0 then
     begin
       R := ResComp[ClesTri[J]];
       break;
     end;
  end;
  result :=  R;
end;

function CompareMultiDesc(Item1, Item2: Pointer): Integer;
begin
  result:=  -CompareMulti(Item1, Item2);
end;

{ TFRadios }

procedure TRadiosList.DoSort;
begin
  if FSortType <> cdcNone then
  begin
    ClesTri[1] := FSortType;
    if FSortDirection = ascend then sort(@comparemulti) else sort(@comparemultidesc);
  end;
end;

procedure TRadiosList.SetSortDirection(Value: TSortDirections);
begin
  FSortDirection:= Value;
  if FSortDirection = ascend then sort(@comparemulti) else sort(@comparemultidesc);
end;

procedure TRadiosList.SetSortType (Value: TChampsCompare);
begin
  FSortType:= Value;
  if Assigned(FOnChange) then FOnChange(Self);
  DoSort;
end;

constructor TRadiosList.Create(AppName: String);
begin
  inherited Create;
  FAppName:= AppName;
end;

procedure TRadiosList.Delete(const i: Integer);
begin
  inherited delete(i);
  DoSort;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TRadiosList.DeleteMulti(j, k : Integer);
var
  i : Integer;
begin
  // On commence par le dernier, ajouter des sécurités dans la forme appelante
  For i:= k downto j do
  begin
     inherited delete(i);
  end;
  DoSort;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TRadiosList.GetPreset(Index: Integer): Integer;
begin
  result:= FPresets[Index];
end;

procedure TRadiosList.SetPreset(Index: Integer; value: Integer);
begin
  if FPresets[Index] <> value then
  begin
    FPresets[Index]:= value;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;
procedure TRadiosList.Reset;
var
 i: Integer;
begin
 for i := 0 to Count-1 do
  if Items[i] <> nil then Items[i]:= nil;
 Clear;
end;

procedure TRadiosList.AddRadio(Radio : TRadio);
var
 K: PRadio;
begin
  new(K);
  if Radio.UID= 0 then Radio.UID:= charsum(Radio.Name+Radio.url)+charsum(DateTimeToStr(now));
  K^:= Radio;
  add(K);
  DoSort;
  K:= nil;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TRadiosList.ModifyRadio (const i: integer; Radio: TRadio);
begin
  TRadio(Items[i]^):= Radio;
  DoSort;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TRadiosList.ModifyField (const i: integer; field: string; value: variant);
begin
  field:= Uppercase(field);
  if field='NAME' then TRadio(Items[i]^).name:= value;
  if field='ORDER' then TRadio(Items[i]^).order := value;
  if field='URL' then TRadio(Items[i]^).url:= value;
  if field='COMMENT' then TRadio(Items[i]^).comment:= value;
  if field='FAVICON' then TRadio(Items[i]^).favicon:= value;
  if field='FAVICONURL' then TRadio(Items[i]^).faviconurl:= value;
  if field='TAG' then TRadio(Items[i]^).tag:= value;
  DoSort;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TRadiosList.GetItem(const i: Integer): TRadio;
begin
 Result := TRadio(Items[i]^);
end;

function TRadiosList.GetItemFieldString(const i: Integer; field: string): String;
begin
 field:= Uppercase(field);
 result:= '';
  if field='NAME' then result:= TRadio(Items[i]^).name;
  if field='ORDER' then result:= InttoStr(TRadio(Items[i]^).order);
  if field='URL' then result:= TRadio(Items[i]^).url;
  if field='COMMENT' then result:= TRadio(Items[i]^).comment;
  if field='FAVICON' then result:= TRadio(Items[i]^).favicon;
  if field='FAVICONURL' then result:= TRadio(Items[i]^).faviconurl;
  if field='TAG' then result:= BoolToStr(TRadio(Items[i]^).tag);
  if field='UID' then result:= InttoStr(TRadio(Items[i]^).uid);
end;

function TRadiosList.FindbyUID(value: Int64): TRadio;
var
  ndx: int64;
begin
  result:= FindbyUID(value, ndx);
end;

function TRadiosList.FindbyUID(value: Int64; var ndx: int64): Tradio;
var
  i: integer;
begin
  result:= Default(TRadio);
  ndx:= -1;
  for i:= 0 to count-1 do
  begin
    if GetItem(i).uid = value then
    begin
      result:= GetItem(i);
      ndx:= i;
      break;
    end;
  end;
end;

function TRadiosList.FindbyName(value: String): TRadio;
var
  ndx: int64;
begin
  ndx:= 0;
  result:= FindbyName(value, ndx);
end;

function TRadiosList.FindbyName(value: String; var ndx: int64): Tradio;
var
  i:integer;
begin
  result:= Default(TRadio);
  //ndx:= -1;
  if (ndx<0) or (ndx>count-1) then exit;
  for i:= ndx to count-1 do
  begin
    if pos(UpperCase(value), UpperCase(GetItem(i).name))>0 then
    begin
      result:= GetItem(i);
      ndx:= i;
      break;
    end else ndx:= -1;
  end;
end;

function TRadiosList.SaveItem(iNode: TDOMNode; sname, svalue: string): TDOMNode;
begin
  result:= iNode.OwnerDocument.CreateElement(sname);
  result.TextContent:= svalue;
end;

function TRadiosList.SaveToXMLnode(iNode: TDOMNode; typ: TSaveType= all): Boolean;
var
  i: Integer;
  RadNode: TDOMNode;
begin
  Result:= True;
  If Count > 0 Then
   begin
     TDOMElement(iNode).SetAttribute('sort', IntToStr(Ord(SortType)));
     // Save presets
     for i:= 1 to 20 do TDOMElement(iNode).SetAttribute ('preset'+IntToStr(i), IntToStr(FPresets[i]));
     // Save radios
     For i:= 0 to Count-1 do
     Try
       // Skip tagged contact if in selection typ
       if (typ=selection) and not(GetItem(i).Tag) then continue;
       // Reset tag to false when processed
       if GetItem(i).tag then TRadio(Items[i]^).Tag:= false;
       RadNode := iNode.OwnerDocument.CreateElement('radio');
       iNode.Appendchild(RadNode);
       RadNode.AppendChild(SaveItem(RadNode, 'name', GetItem(i).name));
       RadNode.AppendChild(SaveItem(RadNode, 'url', GetItem(i).url));
       RadNode.AppendChild(SaveItem(RadNode, 'comment', GetItem(i).comment));
       RadNode.AppendChild(SaveItem(RadNode, 'favicon', GetItem(i).favicon));
       RadNode.AppendChild(SaveItem(RadNode, 'faviconurl', GetItem(i).faviconurl));
       RadNode.AppendChild(SaveItem(RadNode, 'order', IntToStr(GetItem(i).order)));
       RadNode.AppendChild(SaveItem(RadNode, 'tag', BoolToString(GetItem(i).tag)));
       RadNode.AppendChild(SaveItem(RadNode, 'uid', IntToStr(GetItem(i).uid)));
       RadNode.AppendChild(SaveItem(RadNode, 'favicon', GetItem(i).favicon));

     except
       Result:= False;
     end;
   end;
end;

function TRadiosList.SaveToXMLfile(filename: string; typ: TSaveType= all): Boolean;
var
  RadiosXML: TXMLDocument;
  RootNode, RadiosNode :TDOMNode;
begin
  result:= false;
  if FileExists(filename)then
  begin
    ReadXMLFile(RadiosXML, filename);
    RootNode := RadiosXML.DocumentElement;
  end else
  begin
    RadiosXML := TXMLDocument.Create;
    RootNode := RadiosXML.CreateElement(lowercase(FAppName));
    RadiosXML.Appendchild(RootNode);

  end;
  RadiosNode:= RootNode.FindNode('radios');
  if RadiosNode <> nil then RootNode.RemoveChild(RadiosNode);
  RadiosNode:= RadiosXML.CreateElement('radios');
  if Count = 0 then AddRadio(DEfault(TRadio));
  if Count > 0 then
  begin
    SaveToXMLnode(RadiosNode);
    RootNode.Appendchild(RadiosNode);
    writeXMLFile(RadiosXML, filename);
    result:= true;
  end;
  if assigned(RadiosXML) then RadiosXML.free;;
end;

function TRadiosList.LoadXMLNode(iNode: TDOMNode): Boolean;
var
  chNode: TDOMNode;
  subNode: TDOMNode;
  k: PRadio;
  s: string;
  upNodeName: string;
  i, j: integer;
  UpCaseAttrib: String;
begin
  SortType:= TChampsCompare(StringToInt(TDOMElement(iNode).GetAttribute('sort')));
  for i:= 0 to iNode.Attributes.Length-1 do
  try
    UpCaseAttrib:=UpperCase(iNode.Attributes.Item[i].NodeName);
    for j:= 1 to 20 do
    begin
      if UpCaseAttrib='PRESET'+InttoStr(j) then FPresets[j]:= StringToInt(iNode.Attributes.Item[i].NodeValue);
    end;
  except
  end;
  chNode := iNode.FirstChild;
  while (chNode <> nil) and (UpperCase(chnode.NodeName)='RADIO')  do
  begin
    Try
      new(K);
      subNode:= chNode.FirstChild;
      while subNode <> nil do
      try
        upNodeName:= UpperCase(subNode.NodeName);
        s:= subNode.TextContent;
        if upNodeName = 'NAME' then K^.name:= s;
        if upNodeName = 'ORDER' then K^.order := StringToInt(s);
        if upNodeName = 'URL' then K^.url:= s;
        if upNodeName = 'COMMENT' then K^.comment:= s;
        if upNodeName = 'FAVICON' then K^.favicon := s;
        if upNodeName = 'FAVICONURL' then K^.faviconurl := s;
        if upNodeName = 'TAG' then K^.Tag:= StringToBool(s);
        if upNodeName = 'UID' then K^.uid := StringToInt(s);
      finally
        subnode:= subnode.NextSibling;
      end;
      add(K);
    finally
      chNode := chNode.NextSibling;
      //K:= nil;
    end;
  end;
  result:= true;
end;

function TRadiosList.LoadXMLFile(filename: string): Boolean;
var
  RadiosXML: TXMLDocument;
  RootNode,RadiosNode : TDOMNode;
begin
  result:= false;
  if not FileExists(filename) then
  begin

    //SaveToXMLfile(filename);

  end;
  ReadXMLFile(RadiosXML, filename);
  RootNode := RadiosXML.DocumentElement;
  RadiosNode:= RootNode.FindNode('radios');
  if RadiosNode= nil then exit;
  LoadXMLnode(RadiosNode);
  If assigned(RadiosNode) then RadiosNode.free;
  result:= true;
end;


// FRadios form

procedure TFRadios.FormCreate(Sender: TObject);
begin
  inherited;
  Radios:= TRadiosList.Create('progname');
  BrwRadios:= TRadiosList.Create('progname');
  ShowMode:= smEdit;
end;

procedure TFRadios.FormDestroy(Sender: TObject);
begin
  inherited;
  if assigned (Radios) then Radios.Free;
  if assigned (BrwRadios) then BrwRadios.Free;
end;

function TFRadios.LoadRadios(filename: string): Boolean;
begin
  if not FileExists(Filename) then
  begin
    Radios.AddRadio(DefRadio);
    Radios.SaveToXMLFile(Filename);
  end else Radios.LoadXMLFile(Filename);
end;

function TFRadios.SaveRadios(filename: string): Boolean;
var
  FilNamWoExt: String;
  i: Integer;
begin
 result:=false;
  if FileExists (FileName) then
  begin
    FilNamWoExt:= TrimFileExt(FileName);
    if FileExists (FilNamWoExt+'.bk5') then  DeleteFile(FilNamWoExt+'.bk5');  // Efface la plus ancienne si elle existe
    For i:= 4 downto 0 do
    begin
      if FileExists (FilNamWoExt+'.bk'+IntToStr(i))     // Renomme les précédentes si elles existent
           then  RenameFile(FilNamWoExt+'.bk'+IntToStr(i), FilNamWoExt+'.bk'+IntToStr(i+1));
    end;
    RenameFile(FileName, FilNamWoExt+'.bk0');
    Radios.SaveToXMLfile(Filename);
  end else
  begin
    Radios.SaveToXMLfile(Filename);
  end;
  result:= true;
end;

// Default buttons states

procedure TFRadios.DefBtnState;
begin
  case ShowMode of
    smEdit: begin
      SBDeleteRadio.Enabled:= true;
      MnuDeleteRadio.Enabled:= true;
    end;
    smSearch: begin
      SBDeleteRadio.Enabled:= false;
      MnuDeleteRadio.Enabled:= false;
    end;
  end;
  SBAddRadio.Enabled:= true;
  MnuAddRadio.Enabled:= true;
  SBEditRadio.Enabled:= true;
  MnuEditRadio.Enabled:= true;
  SBPlayRadio.enabled:= true;
  MnuPlayRadio.enabled:= true;
  BtnCancel.Enabled:= False;
  BtnApply.Enabled:= False;
  BtnOK.Enabled:= true;
  EName.ReadOnly:= true;
  EUrl.ReadOnly:= true;
  EComment.ReadOnly:= true;
  EFavicon.ShowHint:= false;
  SBFavicon.Enabled:= false;
end;

procedure TFRadios.FormShow(Sender: TObject);
begin
  isearch:= 0;
  LBRadios.Clear;
  LBRadios.PopupMenu:= nil;
  EName.Text:='';
  EUrl.Text:= '';
  EComment.Text:= '';
  EFavicon.Text:= '';
  EFaviconURL.Text:= '';
  ESearchName.Text:= '';
  ESearchCountry.Text:= '';
  BtnApply.enabled:= false;
  BtnCancel.Enabled:= false;
  case ShowMode of
    smEdit: begin
      //LBRadios.MultiSelect:= false;
      LPresets.visible:= true;
      LRadioBrowser.visible:= false;
      MnuAddRadio.visible:= false;
      LSearchCountry.enabled:= false;
      ESearchCountry.enabled:= false;
      SBDeleteRadio.enabled:= true;
      LLimit.Enabled:= False;
      ELimit.Enabled:= false;
      PopulateList(Radios);
      if LBRadios.Count >0 then
      begin
        CurRadio:= Radios.GetItem(0);
        LBRadios.Selected[0]:= true;
        DisplayRadio;
      end;
    end;
    smSearch: begin
     //LBRadios.MultiSelect:= true;
      LPresets.visible:= false;
      LRadioBrowser.left:= LPresets.Left;
      LRadioBrowser.visible:= true;
      MnuAddRadio.visible:= true;
      LSearchCountry.enabled:= true;
      ESearchCountry.enabled:= true;
      LLimit.Enabled:= true;
      ELimit.Enabled:= true;
      //DefBtnState;
      SBAddRadio.Enabled:= False;
      SBEditRadio.Enabled:= False;
      SBPlayRadio.Enabled:= False;
      SBDeleteRadio.Enabled:= False;
      apiurl:= getRadioBrowserApi(sDNSHost, sSrvRecAPI, [qtService]) ;
      //apiurl:= getRadioBrowserApi(sDNSHost, sARecAPI, [qtA]) ;
    end;
  end;
end;

procedure TFRadios.LBRadiosClick(Sender: TObject);
begin
  isearch:= 0;
end;

procedure TFRadios.LBRadiosMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if LBRadios.Count = 0 then exit;
  MouseIndex := LBRadios.ItemAtPos(Point(X, Y), False);

end;




function TFRadios.getRadioBrowserApi(host, apiname: string; qt: TQueryType): string;
var
  radiobrowserurls: TStringList;
  i: integer;
  r: TResultRecord;
  cl: TFPHTTPClient;
begin
  result:= '';
  radiobrowserurls:= TStringList.Create;
  dns:= TIdDNSResolver.Create(self);
  dns.Host:= host;
  dns.QueryType := qt;
  dns.Resolve(apiname);
  for i := 0 to dns.QueryResult.Count-1  do
  begin
    r := dns.QueryResult[i];
    case r.RecType of
      qtA: begin
        radiobrowserurls.Add(TARecord(r).IPAddress);
      end;
      qtService: begin
        radiobrowserurls.Add(TSRVRecord(r).Target);
       end;
    end;
  end;
  if radiobrowserurls.Count>0 then
  begin
    cl := TFPHTTPClient.Create(nil);
    cl.IOTimeout:= 5000;
    cl.AllowRedirect:= true;
    cl.AddHeader('User-Agent','Mozilla 5.0 (bb84000 - Webradio)');
    for i:=0 to radiobrowserurls.Count-1 do
    begin
      if qt= [qtA] then
      begin
        dns.QueryType := [qtPTR];
        dns.Resolve(radiobrowserurls.Strings[i]);
        // we have IPs, convert to names
        if dns.QueryResult.Count > 0 then
        begin
          r:= dns.QueryResult[0];
          radiobrowserurls.Strings[i]:= TPTRRecord(r).HostName;
        end;
      end;
      try
        cl.GET('https://'+radiobrowserurls[i]);
        if cl.ResponseStatusCode=200 then
        begin
          result:= radiobrowserurls[i];
          break;
       end;
     except
     end;
    end;
    if assigned(cl) then cl.free;
  end;
  if assigned(radiobrowserurls) then radiobrowserurls.Free;
end;

procedure TFRadios.LBRadiosSelectionChange(Sender: TObject; User: boolean);
begin
  if LBRadios.Count = 0 then exit;
  case ShowMode of
    smEdit: CurRadio := Radios.GetItem(LBRadios.ItemIndex);
    smSearch: CurRadio := BrwRadios.GetItem(LBRadios.ItemIndex);
  end;
  DisplayRadio;
end;

procedure TFRadios.LRadioBrowserClick(Sender: TObject);
begin
  OpenURL(sRadioBrowserURL);
end;

procedure TFRadios.LRadioBrowserMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
      TLabel(Sender).font.style:= [fsBold];
end;

procedure TFRadios.LRadioBrowserMouseEnter(Sender: TObject);
begin
   TLabel(Sender).Cursor:=  crHandPoint;

end;

procedure TFRadios.LRadioBrowserMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Cursor:= crDefault;

end;

procedure TFRadios.LRadioBrowserMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    TLabel(Sender).Font.style:= [];
end;

procedure TFRadios.PButtonsClick(Sender: TObject);
begin

end;

procedure TFRadios.PMnuListPopup(Sender: TObject);
begin
  if (MouseIndex>=0) and (MouseIndex<LBRadios.Count) then LBRadios.Selected[MouseIndex]:= true;
end;

procedure TFRadios.SBAddRadioClick(Sender: TObject);
var
  i: Integer;
  TSB: TSpeedButton;
begin
  NewRadio:= true;
    // Hide all presets buttons
  for i:= 1 to 10 do
  begin
    TSB:= FindComponent('SBPreset' + IntToStr(i)) as TSpeedButton;
    if TSB <> nil then TSB.visible:= false;
  end;
  EditBtnState;
  Case ShowMode of
    smEdit: begin
      prevradio:= LBRadios.ItemIndex;
      LBRadios.OnSelectionChange:= nil;
      EName.Text:= '';
      EUrl.Text:= '';
      EComment.Text:= '';
      EFavicon.Text:= '';
      EFaviconURL.Text:= '';
      EName.OnChange:= @ERadioChange;
      EUrl.OnChange:= @ERadioChange;
    end;
    smSearch: begin
      LBRadios.OnSelectionChange:= nil;
      EName.OnChange:= @ERadioChange;
      EUrl.OnChange:= @ERadioChange;
      ERadioChange(self);
    end;
  end;

  //RadioUID:= 0;  //charsum(Radio.Name+Radio.url)+charsum(DateTimeToStr(now));
end;

procedure TFRadios.SBEditRadioClick(Sender: TObject);
begin
  NewRadio:= false;
  prevradio:= LBRadios.ItemIndex;
  EditBtnState
end;

procedure TFRadios.EditBtnState;
begin
  SBAddRadio.Enabled:= false;
  MnuAddRadio.Enabled:= False;
  SBEditRadio.Enabled:= False;
  MnuEditRadio.Enabled:= False;
  SBDeleteRadio.Enabled:= false;
  MnuDeleteRadio.Enabled:= False;
  BtnCancel.Enabled:= true;
  BtnApply.Enabled:= true;
  BtnOK.Enabled:= false;
  EName.ReadOnly:= false;
  EUrl.ReadOnly:= false;
  EComment.ReadOnly:= false;
  EFavicon.ReadOnly:= true;
  EFaviconURL.ReadOnly:= true;
  EFavicon.ShowHint:= true;
  SBFavicon.enabled:= true;
end;

procedure TFRadios.ERadioChange(Sender: TObject);
begin
  RadioUID:= CharSum(EName.text+EURL.Text)+charsum(DateTimeToStr(now));
  //LUID.Caption:= InttoStr(RadioUID);
end;

procedure TFRadios.ESearchNameChange(Sender: TObject);
begin
  isearch:= 0;
end;

procedure TFRadios.EFaviconURLClick(Sender: TObject);
begin
  if length(EFaviconURL.text)>0 then  OpenURL(EFaviconURL.text);
end;

procedure TFRadios.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not BtnOK.Enabled then CloseAction := caNone;
end;

procedure TFRadios.BtnCancelClick(Sender: TObject);
begin
  RadioFavicon:='';
  EName.OnChange:= nil;
  EUrl.OnChange:= nil;
  NewRadio:= False;
  DefBtnState;
  LBRadios.OnSelectionChange:= @LBRadiosSelectionChange;
  case ShowMode of
    smEdit: PopulateList(Radios);
    smSearch: begin
     PopulateList(BrwRadios);
     SBEditRadio.Enabled:= False;
    end;
  end;
  LBRadios.ItemIndex:= prevradio;
  LBRadios.Selected[LBRadios.ItemIndex]:=true;
end;

procedure TFRadios.SBDeleteRadioClick(Sender: TObject);
begin
  if MsgDlg(Caption, Format(sConfirmDeleteRadio,
      [Radios.GetItem(LBRadios.ItemIndex).name]) , mtWarning,
      mbYesNo, [YesBtn, NoBtn], 0) = mrYes then
  begin
    DefBtnState;
    Radios.Delete(LBRadios.ItemIndex);
    PopulateList(Radios);
    LBRadios.ItemIndex:= 0;
  end;
end;



procedure TFRadios.SBFaviconClick(Sender: TObject);
begin
  RadioFavicon:='';
  if OPDLogo.Execute then
  begin
    //RadioFavicon:= OPDLogo.FileName;
    if fileexists(OPDLogo.FileName) then
    begin
      // check if file already exists
      //CopyFile(s, WebradioAppsData+PathDelim+'images'+PathDelim+ExtractFileName(s));
      RadioFavicon:= OPDLogo.FileName;
      EFavicon.Text:= ExtractFileName(RadioFavicon);
    end else ;
  end;
end;

procedure TFRadios.BtnApplyClick(Sender: TObject);
var
  tmpRadio: TRadio;
  faviconext: string;
  i: integer;
begin
  if NewRadio then
   begin
     EName.OnChange:= nil;
     EUrl.OnChange:= nil;
     tmpRadio.name:= EName.Text;
     tmpRadio.url:= EUrl.Text;
     tmpRadio.comment:= EComment.Text;
     tmpRadio.UID:= RadioUID;
     tmpRadio.faviconurl:= EFaviconURL.Text;
     if length(RadioFavicon)>0 then
     begin
       faviconext:= ExtractFileExt(RadioFavicon);
       CopyFile(RadioFavicon, WebradioAppsData+PathDelim+'images'+PathDelim+IntToStr(RadioUID)+faviconext);
       tmpRadio.favicon:= IntToStr(RadioUID)+faviconext; //EFavicon.Text;
     end;
     Radios.AddRadio(tmpRadio)
   end else
   begin
     Curradio.name:= EName.Text;
     Curradio.url:= EUrl.Text;
     Curradio.comment:= EComment.Text;
     CurRadio.faviconurl:= EFaviconURL.Text;
     if length(RadioFavicon)>0 then
     begin
       faviconext:= ExtractFileExt(RadioFavicon);
       CopyFile(RadioFavicon, WebradioAppsData+PathDelim+'images'+PathDelim+IntToStr(CurRadio.UID)+faviconext);
       CurRadio.favicon:= IntToStr(CurRadio.UID)+faviconext; //EFavicon.Text;
     end;
     Radios.ModifyRadio(LBRadios.ItemIndex, CurRadio);
   end;
   DefBtnState;
   LBRadios.OnSelectionChange:= @LBRadiosSelectionChange;
   Case ShowMode of
     smEdit: begin
       PopulateList(Radios);
       try
         if NewRadio then
         begin
           for i:= 0 to LBRadios.count-1 do
           begin
             if LBRadios.Items[i]= tmpRadio.name then break;
           end;
           LBRadios.ItemIndex:= i; // LBRadios.Count-1;
         end else LBRadios.ItemIndex:= prevradio;
       except
         LBRadios.ItemIndex:= prevradio;
       end;
     end;
     smSearch: begin
       //PopulateList(BrwRadios);
     end;
   end;
  NewRadio:= false;
  LBRadios.Selected[LBRadios.ItemIndex]:=true;
  BtnOK.Enabled:= true;
end;

procedure TFRadios.SBPlayRadioClick(Sender: TObject);
begin
  if assigned(FOnPlay) then FOnPlay(self, CurRadio);
end;

procedure TFRadios.SBSearchBrwRadioClick(Sender: TObject);
var
  cl: TFPHTTPClient;
  Params : TStringList;
  xml : TStringStream;
  url, s: string;
  RootNode, RadioNode :TDOMNode;
  TmpRadio: TRadio;
  UpCaseAttrib: string;
  i: integer;
begin
  Case ShowMode of
    smEdit: begin
      TmpRadio:= Radios.FindByName(ESearchName.Text, isearch);
      if isearch>=0 then
      begin
        LBRadios.ItemIndex:= isearch;
        inc(isearch);           // Next search
      end else ShowMessage(sNoRadioFound);
    end;
    smSearch: begin
      if apiurl='' then
      begin
        showMessage(sRadioBrowserUnavail);
        exit;
      end;
       Params:= TstringList.create;
       Params.Text:='name='+ESearchName.text+LineEnding+'country='+ESearchCountry.Text+LineEnding+LineEnding+'limit='+ELimit.Text;
       cl := TFPHTTPClient.Create(nil);
       cl.IOTimeout:= 5000;
       cl.AllowRedirect:= true;
       cl.AddHeader('User-Agent','Mozilla 5.0 (bb84000 - Webradio)');
       url:= Format('https://%s/xml/stations/search', [apiurl]);
       s:= cl.FormPost(url, Params);
       Application.ProcessMessages; //let loading complete
       xml:= TStringStream.Create(s);
       ReadXMLFile(RadiosXML, xml);
       RootNode:= RadiosXML.DocumentElement;
       BrwRadios.Reset;
       LBRadios.Clear ;
       TmpRadio:= Default(TRadio);
       RadioNode := RootNode.FirstChild;
       while (RadioNode <> nil)  do
       begin
        for i:= 0 to RadioNode.Attributes.Length-1 do
        try
          UpCaseAttrib:=UpperCase(RadioNode.Attributes.Item[i].NodeName);
          if UpCaseAttrib='NAME' then TmpRadio.name:= RadioNode.Attributes.Item[i].NodeValue;
          if UpCaseAttrib='URL_RESOLVED' then TmpRadio.url:= RadioNode.Attributes.Item[i].NodeValue;
          if UpCaseAttrib='FAVICON' then TmpRadio.faviconurl:= RadioNode.Attributes.Item[i].NodeValue;
        except
        end;
        BrwRadios.AddRadio(TmpRadio);
        RadioNode := RadioNode.NextSibling;
       end;
       if BrwRadios.count > 0 then
       begin
         PopulateList(BrwRadios);
         CurRadio:= BrwRadios.GetItem(0);
         LBRadios.Selected[0]:= true;
         DisplayRadio;
         SBAddRadio.enabled:= true;
         MnuAddRadio.enabled:= true;
         SBPlayRadio.enabled:= true;
         MnuPlayRadio.Enabled:= True;
       end else ShowMessage(sNoRadioFound);

       if assigned(xml) then xml.free;
       if assigned(cl) then cl.free;
       if assigned(Params) then Params.free;

    end;
  end;
end;

procedure TFRadios.DisplayRadio;
var
  i, j, uid: integer;
  TSB: TSpeedButton;
begin
  EComment.Color := cldefault;
  EComment.OnChange:= nil;
  EFavicon.OnChange:= nil;
  EFaviconURL.OnChange:= nil;
  EFavicon.Color := cldefault;
  EFaviconURL.Color:= clDefault;
  EName.Text:= CurRadio.name;
  EUrl.Text:= CurRadio.url;
  EComment.Text:= CurRadio.comment;
  EFavicon.text:= CurRadio.Favicon;
  EFaviconURL.Text:= CurRadio.faviconurl;
  uid:= CurRadio.UID;
  j:= 0;
  // Hide all presets buttons
  for i:= 1 to 10 do
  begin
    TSB:= FindComponent('SBPreset' + IntToStr(i)) as TSpeedButton;
    if TSB <> nil then TSB.visible:= false;
  end;
  // Now display only presets for this radio
  if ShowMode = smEdit then
  for i:= 1 to 20 do
  begin
    if Radios.Presets[i]= uid then
    begin
      inc (j);
      TSB:= FindComponent('SBPreset' + IntToStr(j)) as TSpeedButton;
      if TSB <> nil then
      begin
        TSB.Caption:= InttoStr(i);
        TSB.Visible:= true;
      end;
    end else
    begin

    end;
  end;
end;

procedure TFRadios.PopulateList(rdl: TRadiosList) ;
var
  i: integer;
begin
  LBRadios.Clear;
  if RDl.Count>0 then LBRadios.PopupMenu:= PmnuList;
  for i:= 0 to rdl.count-1 do
  begin
    LBRadios.Items.Add(rdl.GetItem(i).name);
  end;
end;

// Self localization procedure. LangFile parameter is language related ini file

procedure TFRadios.Translate(LngFile: TBbIniFile);
var
  DefaultCaption: String;
begin
  if assigned (Lngfile) then
  with LngFile do
  begin
    //Caption:= ReadString(FRadios', 'Caption', Caption);   // Not set. Use related main menu item caption
    BtnOK.Caption:= ReadString('Common', 'OKBtn', BtnOK.Caption);
    BtnCancel.Caption:= ReadString('Common', 'CancelBtn', BtnCancel.Caption);
    BtnApply.Caption:= ReadString('Common', 'ApplyBtn', BtnApply.Caption);;
    OPDLogo.Title:= ReadString('FRadios', 'OPDLogo.Title', 'Ouvrir une image');
    sConfirmDeleteRadio:= ReadString('FRadios', 'sConfirmDeleteRadio', 'Voulez-vous vraiment supprimer la radio %s ?');
    sRadioBrowserUnavail:= ReadString('FRadios', 'sRadioBrowserUnavail', 'Site Radio browser non disponible');
    sNoRadioFound:= ReadString('FRadios', 'sNoRadioFound', 'Aucune radio trouvée');
    SBAddRadio.Hint:= ReadString('FRadios', 'SBAddRadio.Hint', SBAddRadio.Hint);
    MnuAddRadio.Caption:= ReadString('FRadios', 'MnuAddRadio.Caption', MnuAddRadio.Caption);
    SBEditRadio.Hint:= ReadString('FRadios', 'SBEditRadio.Hint', SBEditRadio.Hint);
    MnuEditRadio.Caption:= ReadString('FRadios', 'MnuEditRadio.Caption', MnuEditRadio.Caption);
    SBDeleteRadio.Hint:= ReadString('FRadios', 'SBDeleteRadio.Hint', SBDeleteRadio.Hint);
    MnuDeleteRadio.Caption:= ReadString('FRadios', 'MnuDeleteRadio.Caption', MnuDeleteRadio.Caption);
    SBPlayRadio.Hint:= ReadString('FRadios', 'SBPlayRadio.Hint', SBPlayRadio.Hint);
    MnuPlayRadio.Caption:= ReadString('Fradios', 'MnuPlayRadio.Caption', MnuPlayRadio.Caption);
    BtnApply.Hint:= ReadString('FRadios', 'BtnApply.Hint', BtnApply.Hint);
    BtnCancel.Hint:= ReadString('FRadios', 'BtnCancel.Hint', BtnCancel.Hint);
    LLimit.Caption:= ReadString('FRadios','LLimit.Caption', LLimit.Caption);
    SBSearchBrwRadio.Hint:= ReadString('FRadios', 'SBSearchBrwRadio.Hint', SBSearchBrwRadio.Hint);
    LSearchName.Caption:= ReadString('FRadios', 'LSearchName.Caption', LSearchName.Caption);
    LSearchCountry.Caption:= ReadString('FRadios', 'LSearchCountry.Caption', LSearchCountry.Caption);
    Lname.Caption:= ReadString('FRadios', 'Lname.Caption', Lname.Caption);
    LUrl.Caption:= ReadString('FRadios', 'LUrl.Caption', LUrl.Caption);
    LComment.Caption:= ReadString('FRadios', 'LComment.Caption', LComment.Caption);
    LFavicon.Caption:= ReadString('FRadios', 'LFavicon.Caption', LFavicon.Caption);
    LPresets.Caption:= ReadString('FRadios', 'LPresets.Caption', LPresets.Caption);
    LRadioBrowser.Caption:= ReadString('FRadios', 'LRadioBrowser.Caption', LRadioBrowser.Caption);
    EFavicon.Hint:= ReadString('FRadios', 'EFavicon.Hint', EFavicon.Hint);


  end;

end;

end.

