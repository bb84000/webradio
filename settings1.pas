{****************************************************************************** }
{ settings1 - Modify FSettings form and record                                                }
{ bb - sdtp - january 2022                                                     }
{*******************************************************************************}

unit settings1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, laz2_DOM, laz2_XMLRead, laz2_XMLWrite, lazbbutils, lazbbcontrols;

type

  //

  // Define the classes in this Unit at the very start for clarity
  TFSettings = Class;          // This is a forward class definition

  // FSettings record management
  TConfig = class
  private
    FOnChange: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FSavSizePos: Boolean;
    FWState: string;
    FLastUpdChk: Tdatetime;
    FNoChkNewVer: Boolean;
    FStartup: Boolean;
    FStartMini: Boolean;
    FHideInTaskbar: Boolean;
    FShowBtnBar: Boolean;
    FRestart: Boolean;
    FLangStr: String;
    FRadioFont: String;
    FDataFolder: String;
    FAppName: String;
    FVersion: String;
    FLastVersion: String;
    FLastRadio: Integer;
    FLastUrl: String;
    FLastVolume: Integer;
    FEncoding: String;
    FSampling: Integer;
    FBitrate: Integer;
    fEquFreqs: array [1..9] of Integer;
    fEquEnabled: Boolean;
    FDisplayText: TColor;
    FDisplayBack: TColor;
    FGenText: TColor;
    FGenBack: TColor;
  public

    constructor Create (AppName: string);
    procedure SetSavSizePos (b: Boolean);
    procedure SetWState (s: string);
    procedure SetLastUpdChk (dt: TDateTime);
    procedure SetNoChkNewVer (b: Boolean);
    procedure SetStartup (b: Boolean);
    procedure SetStartmini (b: Boolean);
    procedure SetHideInTaskbar(b: Boolean);
    procedure SetShowBtnBar(b: Boolean);
    procedure SetRestart(b: boolean);
    procedure SetLangStr (s: string);
    procedure SetRadioFont(s: string);
    procedure SetDataFolder(s: string);
    procedure SetVersion(s: string);
    procedure SetLastVersion(s: String);
    procedure SetLastRadio(i: Integer);
    procedure SetLastUrl(s: string);
    procedure SetLastVolume(i: Integer);
    procedure SetEncoding(s: String);
    procedure SetSampling(i: Integer);
    procedure SetBitrate(i: Integer);
    procedure SetEquFreqs(Index: Integer; i: Integer);
    function GetEquFreqs(Index: Integer): Integer;
    procedure SetEquEnabled(b:Boolean);
    procedure SetDisplayText(c: TColor);
    procedure SetDisplayBack(c: TColor);
    procedure SetGenText(c: TColor);
    procedure SetGenBack(c: TColor);
    function SaveXMLnode(iNode: TDOMNode): Boolean;
    function SaveToXMLfile(filename: string): Boolean;
    function LoadXMLNode(iNode: TDOMNode): Boolean;
    function LoadXMLFile(filename: string): Boolean;
    property EquFreqs [i: Integer]: integer read GetEquFreqs write SetEquFreqs;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property SavSizePos: Boolean read FSavSizePos write SetSavSizePos;
    property WState: string read FWState write SetWState;
    property LastUpdChk: Tdatetime read FLastUpdChk write SetLastUpdChk;
    property NoChkNewVer: Boolean read FNoChkNewVer write SetNoChkNewVer;
    property Startup: Boolean read FStartup write SetStartup;
    property StartMini: Boolean read FStartMini write SetStartMini;
    property HideInTaskbar: Boolean read FHideInTaskbar write SetHideInTaskbar;
    property ShowBtnBar: Boolean read FShowBtnBar write SetShowBtnBar;
    property Restart: boolean read FRestart write SetRestart;
    property LangStr: String read FLangStr write SetLangStr;
    property RadioFont: String read FRadioFont write SetRadioFont;
    property DataFolder: string read FDataFolder write setDataFolder;
    property AppName: string read FAppName write FAppName;
    property Version: string read FVersion write SetVersion;
    property LastVersion: string read FLastVersion write SetLastVersion;
    property LastRadio: Integer read FLastRadio write SetLastRadio;
    property LastUrl: string read FLastUrl write SetLastUrl;
    property LastVolume: Integer read FLastVolume write SetLastVolume;
    property Encoding: String read FEncoding write SetEncoding;
    property Sampling: Integer read FSampling write SetSampling;
    property Bitrate: Integer read FBitrate write SetBitrate;
    property EquEnabled: Boolean read fEquEnabled write SetEquEnabled;
    property DisplayText: Tcolor read FDisplayText write SetDisplayText;
    property DisplayBack: Tcolor read FDisplayBack write SetDisplayBack;
    property GenText: Tcolor read FGenText write SetGenText;
    property GenBack: Tcolor read FGenBack write SetGenBack;
end;


  { TFSettings }

  TFSettings = class(TForm)
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    CBBitrate: TComboBox;
    CBLangue: TComboBox;
    CBShowBtnBar: TCheckBox;
    CBSampling: TComboBox;
    CBNoChkNewVer: TCheckBox;
    CBSavSizePos: TCheckBox;
    CBStartMini: TCheckBox;
    CBStartup: TCheckBox;
    CBHideInTaskBar: TCheckBox;
    CBFonts: TComboBox;
    CPDisplayBack: TColorPicker;
    CPDisplayText: TColorPicker;
    CPGenBack: TColorPicker;
    CPGenText: TColorPicker;
    EDataFolder: TEdit;
    LBitrate: TLabel;
    LDataFolder: TLabel;
    LDisplayBack: TLabel;
    LDisplayText: TLabel;
    LGenBack: TLabel;
    LGenText: TLabel;
    LLangue: TLabel;
    LFont: TLabel;
    LSampling: TLabel;
    LOSVer: TLabel;
    PSystem: TPanel;
    PWinVer: TPanel;
    PButtons: TPanel;
    RBMP3: TRadioButton;
    RBAAC: TRadioButton;
    RBWAV: TRadioButton;
    RBOGG: TRadioButton;
    SBDatafolder: TSpeedButton;
    SDDataFolder: TSelectDirectoryDialog;
    TPColors: TTitlePanel;
    TPEncode: TTitlePanel;
    TPSystem: TTitlePanel;
    procedure CBStartupChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RBEnCodingChange(Sender: TObject);
    procedure SBDatafolderClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private

  public
    Settings: TConfig;
    sMnuCopy: string;
    sMnuPaste: string;
  end;

var
  FSettings: TFSettings;

implementation

{$R *.lfm}

uses webradio1;

constructor TConfig.Create(AppName: string);
begin
  inherited Create;
  FAppName:= 'Test';
  Flangstr:= '';
  FSampling:= 44100;
  FBitrate:= 128;
end;

procedure TConfig.SetSavSizePos(b: Boolean);
begin
  if FSavSizePos =b then exit;
  FSavSizePos:= b;
  if Assigned(FOnStateChange) then FOnStateChange(Self);
end;

procedure TConfig.SetWState(s: string);
begin
  if FWState = s then exit;
  FWState:= s;
  if Assigned(FOnStateChange) then FOnStateChange(Self);
end;

procedure TConfig.SetLastUpdChk(dt: TDateTime);
begin
  if FLastUpdChk = dt then exit;
  FLastUpdChk:= dt;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetNoChkNewVer(b: Boolean);
begin
  if FNoChkNewVer = b then exit;
  FNoChkNewVer:= b;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetStartup (b: Boolean);
begin
  if FStartup = b then exit;
  FStartup:= b;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetStartMini (b: Boolean);
begin
  if FStartMini = b then exit;
  FStartMini:= b;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetHideInTaskbar (b: Boolean);
begin
  if FHideInTaskbar = b then exit;
  FHideInTaskbar:= b;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetShowBtnBar(b: Boolean);
begin
  if FShowBtnBar = b then exit;
  FShowBtnBar:= b;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetRestart(b:boolean);
begin
  if FRestart = b then exit;
  FRestart:= b;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetLangStr (s: string);
begin
  if FLangStr = s then exit;
  FLangStr:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetRadioFont (s: string);
begin
  if FRadioFont = s then exit;
  FRadioFont:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetDataFolder (s: string);
begin
  if FDataFolder = s then exit;
  FDataFolder:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetVersion(s:string);
begin
  if FVersion = s then exit;
  FVersion:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetLastVersion(s:string);
begin
  if FLastVersion = s then exit;
  FLastVersion:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetLastRadio(i:Integer);
begin
  if FLastRadio = i then exit;
  FLastRadio:= i;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetLastUrl(s:string);
begin
  if FLastUrl = s then exit;
  FLastUrl:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetLastVolume(i:Integer);
begin
  if FLastVolume = i then exit;
  FLastVolume:= i;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetEncoding(s: String);
begin
  if FEncoding = s then exit;
  FEncoding:= s;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetSampling(i: Integer);
begin
  if FSampling = i then exit;
  FSampling:= i;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetBitrate(i: Integer);
begin
  if FBitrate = i then exit;
  FBitrate:= i;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TConfig.GetEquFreqs(Index: Integer): Integer;
begin
  result:= fEquFreqs[index];
end;

procedure TConfig.SetEquFreqs(Index: Integer; i: Integer);
begin
  if fEquFreqs[Index] = i then exit;
  fEquFreqs[Index]:= i;
  if Assigned(FOnChange) then FOnChange(Self);
end;


procedure TConfig.SetEquEnabled(b: Boolean);
begin
  if fEquEnabled = b then exit;
  fEquEnabled:= b;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetDisplayText(c: Tcolor);
begin
  if FDisplayText = c then exit;
  FDisplayText:= c;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetDisplayBack(c: Tcolor);
begin
  if FDisplayBack = c then exit;
  FDisplayBack:= c;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetGenText(c: Tcolor);
begin
  if FGenText = c then exit;
  FGenText:= c;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TConfig.SetGenBack(c: Tcolor);
begin
  if FGenBack = c then exit;
  FGenBack:= c;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TConfig.SaveXMLnode(iNode: TDOMNode): Boolean;
var
  j: Integer;
begin
  Try

      TDOMElement(iNode).SetAttribute ('version', FVersion);
      TDOMElement(iNode).SetAttribute ('lastversion', FLastVersion);
      TDOMElement(iNode).SetAttribute ('savsizepos', BoolToString(FSavSizePos));
      TDOMElement(iNode).SetAttribute ('wstate', FWState);
      TDOMElement(iNode).SetAttribute ('lastupdchk', TimeDateToString(FLastUpdChk));
      TDOMElement(iNode).SetAttribute ('nochknewver', BoolToString(FNoChkNewVer));
      TDOMElement(iNode).SetAttribute ('startup', BoolToString(FStartup));
      TDOMElement(iNode).SetAttribute ('startmini', BoolToString(FStartMini));
      TDOMElement(iNode).SetAttribute ('hideintaskbar', BoolToString(FHideInTaskbar));
      TDOMElement(iNode).SetAttribute ('showbtnbar', BoolToString(FShowBtnBar));
      TDOMElement(iNode).SetAttribute ('restart',BoolToString(FRestart));
      TDOMElement(iNode).SetAttribute ('langstr', FLangStr);
      TDOMElement(iNode).SetAttribute ('radiofont', FRadioFont);
      TDOMElement(iNode).SetAttribute ('datafolder', FDataFolder);
      TDOMElement(iNode).SetAttribute ('lastradio', IntToStr(FLastRadio));
      TDOMElement(iNode).SetAttribute ('lasturl', FLastUrl);
      TDOMElement(iNode).SetAttribute ('lastvolume', IntToStr(FLastVolume));
      TDOMElement(iNode).SetAttribute ('encoding', UpperCase(FEncoding));
      TDOMElement(iNode).SetAttribute ('sampling', IntToStr(FSampling));
      TDOMElement(iNode).SetAttribute ('bitrate', IntToStr(FBitrate));
      TDOMElement(iNode).SetAttribute ('equenabled', BoolToString(fEquEnabled));
      TDOMElement(iNode).SetAttribute ('displaytext', ColorToString(FDisplayText));
      TDOMElement(iNode).SetAttribute ('displayback', ColorToString(FDisplayBack));
      TDOMElement(iNode).SetAttribute ('gentext', ColorToString(FGenText));
      TDOMElement(iNode).SetAttribute ('genback', ColorToString(FGenBack));
      for j:=1 to high(FEquFreqs) do TDOMElement(iNode).SetAttribute ('equfreq'+InttoStr(j), IntToStr(FEquFreqs[j]));

    Result:= True;
  except
    result:= False;
  end;
end;

function TConfig.SaveToXMLfile(filename: string): Boolean;
var
  SettingsXML: TXMLDocument;
  RootNode, SettingsNode :TDOMNode;
begin
  result:= false;
  if FileExists(filename)then
  begin
    ReadXMLFile(SettingsXML, filename);
    RootNode := SettingsXML.DocumentElement;
  end else
  begin
    SettingsXML := TXMLDocument.Create;
    RootNode := SettingsXML.CreateElement(lowercase(FAppName));
    SettingsXML.Appendchild(RootNode);
  end;
  SettingsNode:= RootNode.FindNode('settings');
  if SettingsNode <> nil then RootNode.RemoveChild(SettingsNode);
  SettingsNode:= SettingsXML.CreateElement('settings');
  SaveXMLnode(SettingsNode);
  RootNode.Appendchild(SettingsNode);
  writeXMLFile(SettingsXML, filename);
  result:= true;
  if assigned(SettingsXML) then SettingsXML.free;
end;

function TConfig.LoadXMLNode(iNode: TDOMNode): Boolean;
var
  i, j: integer;
  UpCaseAttrib: string;
begin
  Result := false;
  if (iNode = nil) or (iNode.Attributes = nil) then exit;
  // Browse FSettings attributes
  for i:= 0 to iNode.Attributes.Length-1 do
  try
    UpCaseAttrib:=UpperCase(iNode.Attributes.Item[i].NodeName);
    if UpCaseAttrib='VERSION' then FVersion:= iNode.Attributes.Item[i].NodeValue;
    if UpCaseAttrib='LASTVERSION' then FLastVersion:= iNode.Attributes.Item[i].NodeValue;
    if UpCaseAttrib='SAVSIZEPOS' then FSavSizePos:= StringToBool(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='WSTATE' then  FWState:= iNode.Attributes.Item[i].NodeValue;
    if UpCaseAttrib='LASTUPDCHK' then FLastUpdChk:= StringToTimeDate(iNode.Attributes.Item[i].NodeValue,'dd/mm/yyyy hh:nn:ss');
    if UpCaseAttrib='NOCHKNEWVER' then FNoChkNewVer:= StringToBool(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='STARTUP' then FStartup:= StringToBool(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='STARTMINI' then FStartMini:= StringToBool(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='HIDEINTASKBAR' then FHideInTaskbar:= StringToBool(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='SHOWBTNBAR' then FShowBtnBar:= StringToBool(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='RESTART' then FRestart:= StringToBool(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='LANGSTR' then FLangStr:= iNode.Attributes.Item[i].NodeValue;
    if UpCaseAttrib='RADIOFONT' then FRadioFont:= iNode.Attributes.Item[i].NodeValue;
    if UpCaseAttrib='DATAFOLDER' then FDataFolder:= iNode.Attributes.Item[i].NodeValue;
    if UpCaseAttrib='LASTRADIO' then FLastRadio:= StringToInt(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='LASTURL' then FLastUrl:= iNode.Attributes.Item[i].NodeValue;
    if UpCaseAttrib='LASTVOLUME' then FLastVolume:= StringToInt(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='ENCODING' then FEncoding:= iNode.Attributes.Item[i].NodeValue;
    if UpCaseAttrib='SAMPLING' then FSampling:= StringToInt(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='BITRATE' then FBitrate:= StringToInt(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='EQUENABLED' then FEquEnabled:= StringToBool(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='DISPLAYTEXT' then FDisplayText:= StringToColour(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='DISPLAYBACK' then FDisplayBack:= StringToColour(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='GENTEXT' then FGenText:= StringToColour(iNode.Attributes.Item[i].NodeValue);
    if UpCaseAttrib='GENBACK' then FGenBack:= StringToColour(iNode.Attributes.Item[i].NodeValue);
    for j:= 1 to high(fEquFreqs) do if UpCaseAttrib='EQUFREQ'+InttoStr(j) then fEquFreqs[j]:= StringToInt(iNode.Attributes.Item[i].NodeValue);
    result:= true;
  except
    Result:= False;
  end;
end;

function TConfig.LoadXMLFile(filename: string): Boolean;
var
  SettingsXML: TXMLDocument;
  RootNode,SettingsNode : TDOMNode;
begin
  result:= false;
  if not FileExists(filename) then
  begin

    SaveToXMLfile(filename);
  end;
  ReadXMLFile(SettingsXML, filename);
  RootNode := SettingsXML.DocumentElement;
  SettingsNode:= RootNode.FindNode('settings');
  if SettingsNode= nil then exit;
  LoadXMLnode(SettingsNode);
  If assigned(SettingsNode) then SettingsNode.free;
  result:= true;
end;

{ TFSettings : FSettings dialog }

procedure TFSettings.FormCreate(Sender: TObject);
begin
  inherited;
  Settings:= TConfig.Create('progname');
  Settings.ShowBtnBar:= true;
  Settings.DisplayBack:= clBlack;
  Settings.FDisplayText:= clYellow;
  Settings.GenBack:= clDefault;
  Settings.GenText:= clDefault;
  RBWAV.Visible:= false;
  RBWAV.Enabled:= false;
  {$IFDEF WINDOWS}
    RBWAV.Visible:= true;
    RBWAV.Enabled:= true;
  {$ENDIF}
  CBFonts.Items:= Screen.Fonts;
  CBFonts.Items.Add('DotMatrix');

end;

procedure TFSettings.FormDestroy(Sender: TObject);
begin
  //if assigned (Settings) then Settings.free;
end;

procedure TFSettings.FormShow(Sender: TObject);
begin

end;

procedure TFSettings.RBEnCodingChange(Sender: TObject);
begin
  if RBMP3.checked or RBOGG.checked then
  begin
    CBBitrate.Enabled:= true;
    CBSampling.Enabled:= true;
  end;
  if RBAAC.checked or RBOGG.checked then
  begin
    CBBitrate.Enabled:= true;
    CBSampling.Enabled:= false;
  end;
  if RBWAV.checked then
  begin
    CBBitrate.Enabled:= false;
    CBSampling.Enabled:= false;
  end;
end;

procedure TFSettings.SBDatafolderClick(Sender: TObject);
begin
  if SDDataFolder.Execute then
  begin
    EDataFolder.Text:= SDDataFolder.FileName+PathDelim;
  end;
end;

procedure TFSettings.SpeedButton1Click(Sender: TObject);
var
  TCP: TColorPicker;
  i: Integer;
const
  TCPArr: array of string = ('CPDisplayText', 'CPDisplayBack', 'CPGenText', 'CPGenBack');
begin
  for i:= 0 to length(TCPArr)-1 do
  begin
    TCP:= FindComponent('CPDisplayText') as TColorPicker;
    if TCP<>nil then
    begin
      TCP.MnuCopyCaption:= sMnuCopy;
      TCP.MnuPasteCaption:= sMnuPaste;
    end;
  end;
end;

procedure TFSettings.CBStartupChange(Sender: TObject);
begin
  //CBMinimized.Enabled:= CBStartup.Checked;
end;

end.

