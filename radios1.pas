unit Radios1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, Buttons, ExtDlgs, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, lazbbutils,
  fileutil;

type

  TChampsCompare = (cdcName, cdcOrder, cdcUrl, cdcComment, cdcNone);
  TSortDirections = (ascend, descend);
  TSaveType = (selection, all);

  PRadio = ^TRadio;
  TRadio= record
    order : Integer;
    name, url, comment, favicon: string;
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
    function charsum(s: string): int64;
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

  TFRadios = class(TForm)
    BtnOK: TBitBtn;
    EComment: TEdit;
    EFavicon: TEdit;
    EName: TEdit;
    EUrl: TEdit;
    LPresets: TLabel;
    LComment: TLabel;
    LFavicon: TLabel;
    LBRadios: TListBox;
    Lname: TLabel;
    LUrl: TLabel;
    OPDLogo: TOpenPictureDialog;
    PTools: TPanel;
    PMain: TPanel;
    PMain1: TPanel;
    PButtons: TPanel;
    SBEditRadio: TSpeedButton;
    SBPreset3: TSpeedButton;
    SBPreset4: TSpeedButton;
    SBValidRadio: TSpeedButton;
    SBCancelChanges: TSpeedButton;
    SBFavicon: TSpeedButton;
    SBPreset1: TSpeedButton;
    SBPreset2: TSpeedButton;
    SBAddRadio: TSpeedButton;
    SBDeleteRadio: TSpeedButton;
    procedure EEditRadioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBRadiosSelectionChange(Sender: TObject; User: boolean);
    procedure SBAddRadioClick(Sender: TObject);
    procedure SBCancelChangesClick(Sender: TObject);
    procedure SBDeleteRadioClick(Sender: TObject);
    procedure SBEditRadioClick(Sender: TObject);
    procedure SBFaviconClick(Sender: TObject);
    procedure SBValidRadioClick(Sender: TObject);
  private
    NewRadio: Boolean;
    prevradio: Integer;
    procedure DefBtnState;
    procedure EditBtnState;
    procedure DisplayRadio;

  public
    DefRadio, CurRadio: TRadio;
    Radios: TRadiosList;
    WebradioAppsData: String;
    sConfirmDeleteRadio: String;
    OKBtn, YesBtn, NoBtn, CancelBtn: String;
    function LoadRadios(filename: string): Boolean;
    function SaveRadios(filename:String): Boolean;
    function FindbyUID (value: int64): TRadio;
    procedure PopulateList;
  end;

var
  FRadios: TFRadios;
  ClesTri: array[0..4] of TChampsCompare;
  AFieldNames : array [0..6] of string  =('order',
                                           'name',
                                           'url',
                                           'comment',
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
  for J := 0 to 4 do
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
var
  j: integer;
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

function TRadiosList.charsum(s: string): int64;
var
 i: integer;
begin
  result:=0;
  for i:=1 to length(s) do result:=result+ord(s[i]);
end;


procedure TRadiosList.AddRadio(Radio : TRadio);
var
 K: PRadio;
begin
  new(K);
  Radio.UID:= charsum(Radio.Name+Radio.url)+charsum(DateTimeToStr(now));
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
  if field='TAG' then result:= BoolToStr(TRadio(Items[i]^).tag);
  if field='UID' then result:= InttoStr(TRadio(Items[i]^).uid);
end;

function TRadiosList.SaveItem(iNode: TDOMNode; sname, svalue: string): TDOMNode;
begin
  result:= iNode.OwnerDocument.CreateElement(sname);
  result.TextContent:= svalue;
end;

function TRadiosList.SaveToXMLnode(iNode: TDOMNode; typ: TSaveType= all): Boolean;
var
  i, j: Integer;
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
       RadNode.AppendChild(SaveItem(RadNode, 'order', IntToStr(GetItem(i).order)));
       RadNode.AppendChild(SaveItem(RadNode, 'tag', BoolToString(GetItem(i).tag)));
       RadNode.AppendChild(SaveItem(RadNode, 'uid', IntToStr(GetItem(i).uid)));
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
        if upNodeName = 'TAG' then K^.Tag:= StringToBool(s);
        if upNodeName = 'UID' then K^.uid := StringToInt(s);
      finally
        subnode:= subnode.NextSibling;
      end;
      add(K);
    finally
      chNode := chNode.NextSibling;
      K:= nil;
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
  Radios:= TRadiosList.Create('progname');
end;

procedure TFRadios.FormDestroy(Sender: TObject);
begin
  if assigned (Radios) then Radios.Free;
end;

function TFRadios.LoadRadios(filename: string): Boolean;
begin
  if not FileExists(Filename) then
  begin
    Radios.AddRadio(DefRadio);
    Radios.SaveToXMLFile(Filename);
    //ShowModal;
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
  SBAddRadio.Enabled:= true;
  SBEditRadio.Enabled:= true;
  SBDeleteRadio.Enabled:= true;
  SBCancelChanges.Enabled:= False;
  SBValidRadio.Enabled:= False;
  BtnOK.Enabled:= true;
  EName.ReadOnly:= true;
  EUrl.ReadOnly:= true;
  EComment.ReadOnly:= true;
  EFavicon.ReadOnly:= true;
end;

procedure TFRadios.FormShow(Sender: TObject);
var
  i: integer;
begin
  PopulateList;
  DefBtnState;
  if LBRadios.Count >0 then
  begin
    CurRadio:= Radios.GetItem(0);
    LBRadios.Selected[0]:= true;
    DisplayRadio;
  end;
end;

procedure TFRadios.LBRadiosSelectionChange(Sender: TObject; User: boolean);
begin
  if LBRadios.Count = 0 then exit;
  CurRadio := Radios.GetItem(LBRadios.ItemIndex);
  DisplayRadio;
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
  prevradio:= LBRadios.ItemIndex;
  LBRadios.OnSelectionChange:= nil;
  EName.Text:= '';
  EUrl.Text:= '';
  EComment.Text:= '';
  EFavicon.Text:= '';
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
  SBEditRadio.Enabled:= False;
  SBDeleteRadio.Enabled:= false;
  SBCancelChanges.Enabled:= true;
  SBValidRadio.Enabled:= true;
  BtnOK.Enabled:= false;
  EName.ReadOnly:= false;
  EUrl.ReadOnly:= false;
  EComment.ReadOnly:= false;
  EFavicon.ReadOnly:= false;
end;


// Not used
procedure TFRadios.EEditRadioChange(Sender: TObject);
begin
  prevradio:= LBRadios.ItemIndex;
  SBDeleteRadio.Enabled:= false;
  SBAddRadio.Enabled:= false;
  SBCancelChanges.Enabled:= true;
  SBValidRadio.Enabled:= true;
  LBRadios.OnSelectionChange:= nil;
  TEdit(Sender).Color := clGradientActiveCaption;
  BtnOK.Enabled:= false;
end;

procedure TFRadios.SBCancelChangesClick(Sender: TObject);
begin
  NewRadio:= False;
  DefBtnState;
  LBRadios.OnSelectionChange:= @LBRadiosSelectionChange;
  PopulateList;
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
    PopulateList;
    LBRadios.ItemIndex:= 0;
  end;
end;



procedure TFRadios.SBFaviconClick(Sender: TObject);
var
  s: String;
begin
  if OPDLogo.Execute then
  begin
    s:= OPDLogo.FileName;
    if fileexists(s) then
    begin
      // check if file already exists
      CopyFile(s, WebradioAppsData+PathDelim+'images'+PathDelim+ExtractFileName(s));
      EFavicon.Text:= ExtractFileName(s);
    end;
  end;
end;

procedure TFRadios.SBValidRadioClick(Sender: TObject);
var
  tmpRadio: TRadio;
begin
   if NewRadio then
   begin
     tmpRadio.name:= EName.Text;
     tmpRadio.url:= EUrl.Text;
     tmpRadio.comment:= EComment.Text;
     tmpRadio.favicon:= EFavicon.Text;
     Radios.AddRadio(tmpRadio)
   end else
   begin
     Curradio.name:= EName.Text;
     Curradio.url:= EUrl.Text;
     Curradio.comment:= EComment.Text;
     Curradio.favicon:= EFavicon.Text;
     Radios.ModifyRadio(LBRadios.ItemIndex, CurRadio);
   end;
   DefBtnState;
   LBRadios.OnSelectionChange:= @LBRadiosSelectionChange;
  PopulateList;
  try
    if NewRadio then LBRadios.ItemIndex:= LBRadios.Count-1
    else LBRadios.ItemIndex:= prevradio;;
  except
    LBRadios.ItemIndex:= prevradio;
  end;
  NewRadio:= false;
  LBRadios.Selected[LBRadios.ItemIndex]:=true;
  BtnOK.Enabled:= true;
end;

procedure TFRadios.DisplayRadio;
var
  i, j, uid: integer;
  s: string;
  TSB: TSpeedButton;
begin
  //EName.OnChange:= nil;
  //EName.Color := cldefault;
  //EUrl.OnChange:= nil;
  //EUrl.Color := cldefault;
  EComment.OnChange:= nil;
  EComment.Color := cldefault;
  EFavicon.OnChange:= nil;
  EFavicon.Color := cldefault;
  EName.Text:= CurRadio.name;
  EUrl.Text:= CurRadio.url;
  EComment.Text:= CurRadio.comment;
  EFavicon.text:= CurRadio.Favicon;
  //EName.OnChange:= @EEditRadioChange;
  //EUrl.OnChange:= @EEditRadioChange;
  //EComment.OnChange:= @EEditRadioChange;
  //EFavicon.OnChange:= @EEditRadioChange;
  uid:= CurRadio.UID;
  s:= '';
  j:= 0;
  // Hide all presets buttons
  for i:= 1 to 10 do
  begin
    TSB:= FindComponent('SBPreset' + IntToStr(i)) as TSpeedButton;
    if TSB <> nil then TSB.visible:= false;
  end;
  // Now display only presets for this radio
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
    end;
  end;


end;










procedure TFRadios.PopulateList;
var
  i, j: integer;
begin
  LBRadios.Clear; //SGRadios.RowCount:= Radios.count;
  for i:= 0 to Radios.count-1 do
  begin
    LBRadios.Items.Add(Radios.GetItem(i).name);
  end;
end;

function TFRadios.FindbyUID(value: int64): TRadio;
var
  i: integer;
  tmpuid: int64;
begin
  result:= default(TRadio);
  for i:= 0 to Radios.count-1 do
  begin
    if Radios.GetItem(i).uid = value then
    begin
      result:= Radios.GetItem(i);
      break;

    end;
  end;
end;

end.

