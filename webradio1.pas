{*******************************************************************************}
{ webradio1 : main unit code                                                    }
{ bb - sdtp - february 2022                                                     }
{ Using Un4seen BASS libraries www.un4seen.com                                  }
{*******************************************************************************}

unit webradio1;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS}
Win32Proc, windows,
{$ENDIF} LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics,
Dialogs, ExtCtrls, StdCtrls, Menus, bass, basswma, bass_aac, bassenc,
bassenc_mp3, bassenc_aac, bassenc_ogg, bassflac, Equalizer1, lazbbcontrols,
settings1, radios1, lazbbosver, lazbbutils, lazUTF8, lazbbinifiles, registry,
lazbbaboutupdate, lazbbautostart, LResources, ComCtrls, Buttons,
ColorSpeedButton, UniqueInstance, fptimer, variants,
BGRABitmap, BGRABitmapTypes, LazFileUtils, Types;

const
  // Message identifiers used to mimic message loop processing
  //
  WP_Connecting = 0;
  WP_Error      = 1;
  WP_Progress   = 2;
  WP_RadioName  = 3;
  WP_BitSecond  = 4;
  WP_Connected  = 5;
  WP_Status     = 6;
  WP_Title      = 7;
  WP_Length     = 8;
  WP_FilePlay   = 9;
  WP_VuMeter    = 10;
  WP_BitRate    = 11;
  WP_Recing     = 13;
  WP_Hint       = 14;
  WP_NewVersion = 15;
  BASS_ERROR_SSL= 10;

  // HLS definitions (copied from BASSHLS.H)
  BASS_SYNC_HLS_SEGMENT= $10300;
  BASS_TAG_HLS_EXTINF  = $14000;


  WMA_STRM= 1;
  MP3_STRM= 2;
  OGG_STRM= 3;
  AAC_STRM= 4;
  WAV_STRM= 5;
  FLAC_STRM= 6;
  UNK_STRM= 7;

  SrtmTyp: array [WMA_STRM..UNK_STRM] of String = ('WMA',' MP3',' OGG',' AAC', ' WAV', ' FLAC', ' UNK');

type
  // Record type for messages array
  sMsgRec = record
    state: Boolean;
    value: variant;
  end;

 // Wave format header
  _WAVEFORMATEX   = packed Record
                         wFormatTag: WORD;
                         nChannels: WORD;
                         nSamplesPerSec: DWORD;
                         nAvgBytesPerSec: DWORD;
                         nBlockAlign: WORD;
                         wBitsPerSample: WORD;
                         cbSize: WORD;
 End;

 WAVEFORMATEX    = _WAVEFORMATEX;
 PWAVEFORMATEX   = ^_WAVEFORMATEX;

 // TIcyTag structure
 TIcyTag = packed Record
   HTTPResponse: String;
   //server: string;    // Server:
   date: String;   // Date:
   content_type: string;  //content-type: audio/mpeg
   //cache_control: string; // Cache-Control:
   //expires: string;  // Expires:
   //pragma: String;  // Pragma:
   //Access_Control_Allow_Origin: string;                // Access-Control-Allow-Origin
   //Access_Control_Allow_Headers: string;
   //Access_Control_Allow_Methods: string;
   //Connection: string; // Connection:
   //notice1: string; //icy-notice1: This stream requires Winamp
   //notice2: string; //icy-notice2: SHOUTcast Distributed Network Audio Server/Linux v1.9.5
   name: string;    //icy-name: RadioABF.net - Paris Electro Spirit Live From FRANCE
   genre: string;   //icy-genre: Techno House Electronic
   url: string;     //icy-url: http://www.radioabf.net/
   description: string; // icy-description:
   pub: string;  //icy-pub: 1
   metaint: Integer; //icy-metaint: 32768
   bitrate: Integer; //icy-br: 160
   audio_info: string; // ice-audio-info:ice-samplerate=44100;ice-quality=4,00;ice-channels=2
   samplerate: Integer;
   channels: Integer;
   text: String;
 end;

  { int64 or longint type for Application.QueueAsyncCall }
  {$IFDEF CPU32}
    iDays= LongInt;
  {$ENDIF}
  {$IFDEF CPU64}
    iDays= Int64;
  {$ENDIF}

  TSaveMode = (None, Setting, All);

  { TFWebRadioMain }

  TFWebRadioMain = class(TForm)
    ILButtons: TImageList;
    LLeft: TLabel;
    Lposition: TLabel;
    LRight: TLabel;
    LStatus: TLabel;
    LTag: TSCrollLabel;
    PMnuHelp: TMenuItem;
    PMnuChooseRadio: TMenuItem;
    PTMnuMute: TMenuItem;
    PTMnuQuit: TMenuItem;
    PTMnuAbout: TMenuItem;
    PTMnuIconize: TMenuItem;
    PTMnuRestore: TMenuItem;
    PMnuTray: TPopupMenu;
    PnlTop: TPanel;
    PMnuEqualizer: TMenuItem;
    PMnuOpenRadio: TMenuItem;
    PBLength: TProgressBar;
    SBQuit: TSpeedButton;
    SBMute: TSpeedButton;
    SBHelp: TSpeedButton;
    SBSEtings: TSpeedButton;
    SBOpenUrl: TSpeedButton;
    SBAbout: TSpeedButton;
    SBReadFile: TSpeedButton;
    SBEqualizer: TSpeedButton;
    SBRadList: TSpeedButton;
    LRadioName: TSCrollLabel;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    SignalMeterL: TSignalMeter;
    SignalMeterR: TSignalMeter;
    SBOpenRadio: TSpeedButton;
    TBVolume: TTrackBar;
    TrayRadio: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    VuTimer: TLFPTimer;
    MsgTimer: TLFPTimer;
    RecordBtnTimer: TLFPTimer;
    ODAudio: TOpenDialog;
    PRight: TPanel;
    PMnuOpenURL: TMenuItem;
    PMnuReadFile: TMenuItem;
    SBPause: TSpeedButton;
    SBPlay: TSpeedButton;
    SBPreset19: TColorSpeedButton;
    SBPreset10: TColorSpeedButton;
    ImgLogo: TImage;
    LRecording: TLabel;
    LPause: TLabel;
    LBitrate: TLabel;
    LFrequency: TLabel;
    LStereo: TLabel;
    PMnuRadList: TMenuItem;
    PMnuDeletePreset: TMenuItem;
    PMnuPreset: TPopupMenu;
    PMnuRadiosList: TPopupMenu;
    PPresets: TPanel;
    PMnuQuit: TMenuItem;
    PMnuSettings: TMenuItem;
    PMnuAbout: TMenuItem;
    PMnuMain: TPopupMenu;
    PDisplay: TPanel;
    PMain: TPanel;
    SBPreset9: TColorSpeedButton;
    SBPreset8: TColorSpeedButton;
    SBPreset7: TColorSpeedButton;
    SBPreset6: TColorSpeedButton;
    SBPreset5: TColorSpeedButton;
    SBPreset4: TColorSpeedButton;
    SBPreset3: TColorSpeedButton;
    SBPreset2: TColorSpeedButton;
    SBPreset1: TColorSpeedButton;
    SBPreset20: TColorSpeedButton;
    SBPreset11: TColorSpeedButton;
    SBPreset12: TColorSpeedButton;
    SBPreset13: TColorSpeedButton;
    SBPreset14: TColorSpeedButton;
    SBPreset15: TColorSpeedButton;
    SBPreset16: TColorSpeedButton;
    SBPreset17: TColorSpeedButton;
    SBPreset18: TColorSpeedButton;
    SBRecord: TSpeedButton;
    SBStop: TSpeedButton;
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MsgTimerTimer(Sender: TObject);
    procedure PMnuEqualizerClick(Sender: TObject);
    procedure PMnuRadListClick(Sender: TObject);
    procedure PMnuChooseRadioClick(Sender: TObject);
    procedure PMnuDeletePresetClick(Sender: TObject);
    procedure PMainClick(Sender: TObject);
    procedure PMnuAboutClick(Sender: TObject);
    procedure PMnuOpenURLClick(Sender: TObject);
    procedure PMnuQuitClick(Sender: TObject);
    procedure PMnuReadFileClick(Sender: TObject);
    procedure PTMnuRestoreClick(Sender: TObject);
    procedure PMnuSettingsClick(Sender: TObject);
    procedure PMnuTrayPopup(Sender: TObject);
    procedure PTMnuIconizeClick(Sender: TObject);
    procedure RecordBtnTimerTimer(Sender: TObject);
    procedure SBHelpClick(Sender: TObject);
    procedure SBMuteClick(Sender: TObject);
    procedure SBPauseClick(Sender: TObject);
    procedure SBPlayClick(Sender: TObject);
    procedure SBPresetContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure SBPresetClick(Sender: TObject);
    procedure SBRecordChangeBounds(Sender: TObject);
    procedure SBRecordClick(Sender: TObject);
    procedure SBStopClick(Sender: TObject);
    procedure TBVolumeChange(Sender: TObject);
    procedure VuTimerTimer(Sender: TObject);
  private
    CanClose: boolean;
    Initialized: boolean;
    Iconized: boolean;
    OS, OSTarget, CRLF: string;
    CompileDateTime: TDateTime;
    UserPath, UserAppsDataPath: string;
    MusicPath: String;
    WRExecPath:  String;
    LangStr: string;
    ProgName: String;
    LangFile: TBbIniFile;
    LangNums: TStringList;
    LangFound: boolean;
    OsVersion: TOSVersion;
    version: String;
    WebRadioAppsData: String;
    OKBtn, YesBtn, NoBtn, CancelBtn: string;
    sConnectStr, sConnectedStr, sNotConnectedStr, sLoadStr: String;
    ErrNoBassPLAY, ErrNoBassEnc, ErrBassVersion, ErrBassEncVer: String;
    ErrUnsupportedEnc, ErrEncoding : String;
    ErrConnection: String;
    sBufferStr: String;
    ConfigFileName, RadiosFileName: String;
    ChkVerInterval: Int64;
    sCannotGetNewVerList: String;
    sUpdateAlertBox: String;
    sNoLongerChkUpdates: String;
    sStereoCaption, sMonoCaption, sRecordingCaption: String;
    sMuteHint, sMutedHint: String;
    sMuteMenu, sMutedMenu: String;
    sEnterUrl: String;
    sRecordingHint, sStopRecordingHint: String;
    HttpErrMsgNames: array [0..16] of string;
    PrevTop, PrevLeft: integer;
    SettingsChanged, RadiosChanged: Boolean;
    FileLength: Int64;
    VuTimercount: int64;
    LastVol: Integer;
    TmpPreset: integer;
    PluginsDir: string;
    CurPreset: TColorSpeedButton;
    sStopCaption: String;
    sPauseCaption: String;
    Paused: Boolean;
    Stopped: Boolean;
    IsRadio : Boolean;
    Recording: Boolean;
    sUse64bit: String;
    Connecting_beg: Int64;
    Recording_beg: TDateTime;
    RecordBtnTimerCount: Int64;
    MsgTimerCount: Int64;
    Muted: Boolean;
    mp3enclib, aacenclib, oggenclib: Boolean;
    acmForm: PWaveFormatEx;
    RegularHeight: Integer;
    CurRadioTitle: string;
    FontDir: String;
    DMFontRes: Cardinal;
    ResFnt: TResourceStream;
    FontId: integer;
    FontCount: cardinal;

    procedure OnAppMinimize(Sender: TObject);
    procedure OnQueryendSession(var Cancel: Boolean);
    procedure InitButtons;
    procedure Initialize;
    procedure LoadSettings(Filename: string);
    procedure PlayRadio (radio:TRadio);
    procedure ModLangue;
    procedure CheckUpdate(days: iDays);
    procedure ShowBtnBar;
    function SaveConfig(Typ: TSaveMode): boolean;
    procedure SettingsOnChange(Sender: TObject);
    procedure SettingsOnStateChange(Sender: TObject);
    procedure RadiosOnChange(Sender: TObject);
    procedure BeforeClose;
    procedure PMnuRadioListItemClick(Sender: TObject);
    procedure PMnuOpenRadioItemClick(Sender: TObject);
    procedure LoadPresets;
    procedure PlayFile(Filename: string);
    procedure SetEqual(b: boolean);
    function HideOnTaskbar: boolean;
    procedure ChangeColors;
  public
    BassWMA, BassAAC, BassFLAC, BassEnc, BassEncMP3: HPlugin;
    ID3Tag: TAG_ID3;
    StreamSave:boolean;
    mp3file: String;
    DefaultCaption: String;
  end;

// Global variables
var
  FWebRadioMain: TFWebRadioMain;
  chan: HSTREAM = 0;
  CanalInfo: BASS_CHANNELINFO;
  FileStream:TFileStream;
  cthread: Int64 = 0;
  error: Boolean;
  BufSize, Bitrate: Integer;
  CurRadio: TRadio;
  lparamArr: array [0..15] of sMsgRec;
  BassErrArr: array [0..48] of String;
  fileduration: double;
  // Equalizer variables
  equParam: BASS_DX8_PARAMEQ;
  CenterFreqs : array [1..9] of Integer = (62, 125, 250, 500, 1000, 2000, 4000, 8000, 16000);
  fx: array[1..9] of integer;
  IcyTag: TIcyTag;

  function AddFontMemResourceEx(pbFont: Pointer; cbFont: DWORD; pdv: Pointer; pcFonts: LPDWORD): LongWord; stdcall;
    external 'gdi32.dll' Name 'AddFontMemResourceEx';
  function RemoveFontMemResourceEx(fh: LongWord): LongBool; stdcall;
    external 'gdi32.dll' Name 'RemoveFontMemResourceEx';

implementation

{$R *.lfm}


// This procedure replace windows message loop processing to make this app portable
// on non windows OS by a processing inside a fptimer OnEventTimer procedure
// MsgId : WP message identifier
// state : true to allow processing in the event, which set the value to false after processing
// l_param : string or integer according message value
// uses lparamArr: array [0..15] of sMsgRec to store values;

procedure SimulateMsg(MsgId: Integer; state: Boolean; l_param: variant);
begin
  lparamArr[MsgId].state:= state;
  lparamArr[MsgId].value:= l_param;
end;

//Convert Pchar tags to strings as we can
// easily concatenate several tags (ie in wma and ogg streams)

function TagToStrings (tag: Pchar): String;
begin
  result:= '';
  while (tag^ <> #0) do
  begin
    result:= result+Copy(tag, 1, MaxInt)+LineEnding;
    tag := tag + Length(tag) + 1;
  end;
end;

// parse shoutcast and icecast header informations

function GetIcyTag(sTag: String): TIcyTag;
var
  OGGQual: array [-2..10] of Integer = (32,48,64,80,96,112,128,160,192,224,256,320,500);
  AudioInf: array of String;
  s: string;
  i: integer;
  strlist: TStringList;
begin
  result:= default(TIcyTag);
  if length(sTag)= 0 then exit;
  result.text:= sTag;
  strList:= TstringList.Create;
  strList.Text:= sTag;
  for i:= 0 to strlist.Count-1 do
  begin
    s:= strlist.Strings[i];
    if (Copy(s, 1, 9) = 'icy-name:') then result.name:= Copy(s, 10, MaxInt);
    if (Copy(s, 1, 7) = 'icy-br:') then
    try
      result.Bitrate:= StrToInt(Copy(s, 8, MaxInt));
    except
    end;
    // OGG quality
    if (UpperCase(Copy(s, 1, 14)) = 'ICY-BR:QUALITY') then
    try
      Bitrate:= OGGQual[Round(StrToFloat(Copy(s, 15, MaxInt)))];
    except
    end;
    if (Copy(s, 1, 15) = 'ice-audio-info:') then result.audio_info:= Copy(s, 16, MaxInt);
    // WMA bitrate
    if copy(s, 1, 8) = 'Bitrate=' then   // to exclude 'CurrentBitrate=', 'OptimalBitrate='
    try
      Result.Bitrate := round (strToInt(copy(s, 9, MaxInt)) / 1024);
    except
    end;
  end;
  // Parse OGG audio info (ice-audio-info:channel=2;quality=0.3;samplerate:=44100;bitrate=128)
  if assigned(strlist) then strlist.free;;
  if (length(result.audio_info)>0) then
  try
    AudioInf:= (result.audio_info).Split(';');
    if length(AudioInf)>0 then
    begin
      for i:=0 to length(AudioInf)-1 do
      begin
        if (Copy(UpperCase(AudioInf[i]), 1, 7))='QUALITY' then
        begin
          s:= Copy(AudioInf[i], 9, MaxInt);
          if Copy(s, 1, 2)='0.' then s:= Copy(AudioInf[i], 11, MaxInt);   // Old wauli
          Result.Bitrate:= OGGQual[Round(StrToFloat(s))];
        end;
      end;
    end;
  except
  end;
end;

// Special Bass and callback functions
// update stream title from metadata for mp3, acc and ogg streams

procedure DoMeta();
var
  meta: PChar;
  p: Integer;
  supp, s, s1: String;
begin
  // (Shoutcast: MP3, AAC, HLS)
  meta:= BASS_ChannelGetTags(Chan, BASS_TAG_META);
  if (meta <> nil) then
   begin
     supp:= UpperCase(meta);;
     p := Pos('STREAMTITLE=', supp);
     if (p = 0) then Exit;
     p := p + 13;
     s:= Copy(meta, p, Pos(';', meta) - p - 1);
  end else
  // Try to get other Tags
  begin
    meta:= BASS_ChannelGetTags(chan, BASS_TAG_OGG);
    if meta <> nil then
    begin
      // get Icecast/OGG tags
      supp:= UpperCase(meta);
      p:= Pos('ARTIST=', supp);
      if p > 0 then
      begin
        p := p + 7;
        s:=  Copy(meta, p, length(meta)-7);
      end;
      p := Pos('TITLE=', supp);
      if p > 0 then
      begin
        p := p + 6;
        s1:=  Copy(meta, p, length(meta)-6);
      end;
      if length(s+s1)= 0 then exit;
      if (length(s)>0) and (length(s1) > 0) then s:= s+' - '+s1
      else s:= s+s1;
     end else
     // try WMA tags
     begin
       meta:= BASS_ChannelGetTags(Chan, BASS_TAG_WMA_META);
        if meta <> nil then
        begin
          p := Pos('TEXT=', meta);
          if (p = 0) then Exit;
          p:= p+5;
          s:= Copy(meta, p, MaxInt);
        end else
        // Try HLS tags
        begin
           meta:= BASS_ChannelGetTags(chan, BASS_TAG_HLS_EXTINF);
	   if meta <> nil then
           begin  // got HLS segment info
             p:= Pos(',', meta);
             if (p = 0) then exit;
             s:= Copy(meta, p+1, length(meta)-1);
           end;
        end;
     end;
  end ;
  SimulateMsg(WP_Title, True, s);
end;

// Synchronize when meta tag change in stream

procedure MetaSync(handle: HSYNC; channel, data, user: DWORD); stdcall;
begin
  DoMeta();
end;

// Procedure de download et de détection du statut

procedure StatusProc(buffer: Pointer; len, user: DWORD); stdcall;
var
  Buff : array [0..127] of Char;
  i: integer;
begin
  //Initialze buffer
  for i:= 0 to length(Buff)-1 do Buff[i]:= char(0);
  // Copy tag in a buffer
  Move(FWebRadioMain.ID3Tag, Buff, SizeOf(Buff));
  //FileStreamStuff
  if FWebRadioMain.StreamSave then
  begin
    if FileStream = nil then
    FileStream := TFileStream.Create(FWebRadioMain.mp3file, fmCreate);
    if (buffer <> nil) then
    begin
      FileStream.Write(buffer^, len) ;// Okay we have Data write The File
    end else
    begin
      FileStream.Write(buff, SizeOf(Buff)) ;
      FreeandNil(FileStream);
    end;
  end else
  begin
    if FileStream <> nil then FileStream.Write(buff, 128) ;
    FreeandNil(FileStream);
  end;
  //FileStreamStuff End
end;

// Open an Url

function OpenURL(url: String): Integer;
var
  icy: PChar;
  Len, Progress: Int64;
  ptag: pchar;
  stag: String;
  HintStr : string;
  ByteSec: Cardinal;
  Strm_Type: integer;
  stag1: string;
begin
  error:= false;
  HintStr:= '';
  Result:= 0;
  SimulateMsg(WP_Title, true, ' ');
  // close old stream
  if Chan > 0 then
    if not BASS_StreamFree(chan) then
    begin
       SimulateMsg(WP_Error, True, BASS_ErrorGetCode);
      exit;
    end;
  Bitrate:= 0;
  progress := 0;
  chan := BASS_StreamCreateURL(PChar(url), 0, BASS_STREAM_STATUS, DOWNLOADPROC(@StatusProc), nil);

  if (chan = 0) or (BASS_ErrorGetCode() <> 0) then
  begin
     SimulateMsg(WP_Error, true, BASS_ErrorGetCode);
     chan:= 0;
     if chan <> 0 then BASS_StreamFree(chan) ;
  end else
  begin
    SimulateMsg(WP_Connecting, True, Copy(url, 1, length(url)));
    // Progress
    repeat
      len := BASS_StreamGetFilePosition(chan, BASS_FILEPOS_END);
      if (len = DW_Error) then
        break; // something's gone wrong! (eg. BASS_Free called)
      progress := (BASS_StreamGetFilePosition(chan, BASS_FILEPOS_DOWNLOAD) -
        BASS_StreamGetFilePosition(chan, BASS_FILEPOS_CURRENT)) * 100 div len;
      // percentage of buffer filled
      SimulateMsg(WP_Progress, true, progress);
    until progress > 75;
    // Infos sur le flux
BASS_ChannelGetInfo(chan, CanalInfo);
    ByteSec:= BASS_ChannelSeconds2Bytes(Chan, 1);
    SimulateMsg(WP_RadioName, true, '');
    // Get icy tag
    icy := BASS_ChannelGetTags(chan, BASS_TAG_ICY);
    if (icy = nil) then icy := BASS_ChannelGetTags(chan, BASS_TAG_HTTP); // no ICY tags, try HTTP
    Case CanalInfo.ctype of
      BASS_CTYPE_STREAM_WMA:
        begin
          ptag:= BASS_ChannelGetTags(Chan, BASS_TAG_WMA);
          if (ptag <> nil) then stag1:= TagToStrings(ptag);
          Strm_Type:= WMA_STRM;
        end;
      BASS_CTYPE_STREAM_MP3: Strm_Type:= MP3_STRM;
      BASS_CTYPE_STREAM_AAC: Strm_Type:= AAC_STRM ;
      BASS_CTYPE_STREAM_OGG:
        begin
          pTag := BASS_ChannelGetTags(chan, BASS_TAG_OGG);
          if (ptag <> nil) then stag1:= TagToStrings(ptag);
          Strm_Type:= OGG_STRM;
        end;
    end;
    if (icy <> nil) then
    begin
      sTag:= TagToStrings(icy);
      // if there is another tag add it to icy
      if length(stag1)>0 then sTag:= sTag+sTag1;
      IcyTag:= GetIcyTag(stag);
      HintStr:= IcyTag.text;
      SimulateMsg(WP_RadioName, true, IcyTag.name);
      BitRate:= IcyTag.bitrate;
    end;

    SimulateMsg(WP_Connected, true, Strm_Type);
    if Strm_Type=WMA_STRM then BASS_ChannelSetSync(chan, BASS_SYNC_WMA_META, 0, SYNCPROC(@MetaSync), nil)
    else BASS_ChannelSetSync(chan, BASS_SYNC_META, 0, SYNCPROC(@MetaSync), nil); ;
    if Bitrate = 0 then Bitrate:= (ByteSec*745) div 1024000 ;
    SimulateMsg(WP_BitRate, True, Bitrate);
    SimulateMsg(WP_Hint, True, HintStr);
    // get the stream title and set sync for subsequent titles
    DoMeta();
    len:= BASS_ChannelGetLength(chan, BASS_POS_BYTE);
    SimulateMsg(WP_Length, true, len);
    // play it!
    BASS_ChannelPlay(chan, FALSE);
    //FWebRadioMain.SetEqual(true);
  end;
  cthread := 0;
end;

// Enable or disable equalizer

procedure TFWebRadioMain.SetEqual(b: boolean);
var
  i: Integer;
begin
  For i:= 1 to 9 do
  begin
    if b then
    begin
      fx[i] := BASS_ChannelSetFX(chan, BASS_FX_DX8_PARAMEQ, 1);
      equParam.fBandwidth := 16;
      equParam.fCenter := CenterFreqs [i];
      BASS_FXSetParameters(fx[i], @equParam);
      if  FSettings.Settings.EquEnabled then
        equParam.fgain :=  Fsettings.Settings.EquFreqs[i]; //TBE.Position-15
    end else BASS_ChannelRemoveFX(chan, fx [i]);
  end;
end;

// Play an audio file

procedure TFWebRadioMain.PlayFile (filename: string);
var
  P: Pointer;
  Float_Time: Double;
  len, bitrate : Integer;
  filtyp: Integer;
begin
  if cthread <> 0 then cthread:=0;
  BASS_StreamFree(chan);
  chan:= BASS_StreamCreateFile(
     False,
     PChar(filename),
     0,
     0,
     BASS_STREAM_AUTOFREE );
    SimulateMsg(WP_Connecting, True, ' ');
   BASS_ChannelGetInfo(Chan, CanalInfo);
   Case CanalInfo.ctype of
      BASS_CTYPE_STREAM_WMA : filtyp:= 1;
      BASS_CTYPE_STREAM_MP3 : filtyp:= 2;
      BASS_CTYPE_STREAM_OGG : filtyp:= 3;
      BASS_CTYPE_STREAM_AAC : filtyp:= 4;
      BASS_CTYPE_STREAM_WAV_PCM : filtyp:= 5;
      BASS_CTYPE_STREAM_WAV_FLOAT : filtyp:= 5;
   end;
   SetEqual(true);
   SimulateMsg(WP_Connected, true, filtyp);
   float_time:=BASS_ChannelBytes2Seconds(chan,BASS_ChannelGetLength(chan, BASS_POS_BYTE)); // playback duration
   len:=BASS_StreamGetFilePosition(chan,BASS_FILEPOS_END); // file length
   fileduration:= float_time;
   bitrate:= Trunc(len/(125*float_time)+0.5); // bitrate (Kbps)
   SimulateMsg(WP_BitRate, true, Bitrate);
   // todo use idv3v2
   if BASS_ChannelGetTags(chan, BASS_TAG_ID3) = nil then
   begin
     caption:= ExtractFileName(filename);
     SimulateMsg(WP_Title, true, ' ');
   end else
   begin
     P:= BASS_ChannelGetTags(chan, BASS_TAG_ID3);
     caption:= TAG_ID3(P^).title;
     SimulateMsg(WP_Title, true, String(TAG_ID3(P^).artist)+' - '+String(TAG_ID3(P^).album));
   end;
   try
       CurPreset.Font.Color:= clDefault;
   except
   end;
   CurRadio.Name:= '';
   if length(caption)< 2 then caption:= Extractfilename(filename);
   SimulateMsg(WP_RadioName, true, Caption);
   BASS_ChannelPlay( chan, false);
   IsRadio:= False;
   // Set generic logi
   ImgLogo.Stretch:= true;
   ImgLogo.Picture.LoadFromLazarusResource('webradio256');
   SetEqual (FEqualizer.CBEqualize.Checked);
end;

{ TFWebRadioMain }

procedure TFWebRadioMain.FormCreate(Sender: TObject);
var
  s: String;
begin
  // install memory font DotMatrix
  {$IFDEF WINDOWS}
    FontDir:= GetTempDir(false);
    FontId := Screen.Fonts.IndexOf('DotMatrix');
    if FontId < 0 then
    begin
      ResFnt := TResourceStream.Create(hInstance, 'DOTMATRIX', RT_RCDATA);
      DMFontRes:= AddFontMemResourceEx(ResFnt.Memory, ResFnt.Size, nil, @FontCount);
      PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0) ;
      Application.ProcessMessages;
   end;
    {$ENDIF}
  inherited;

  // Variables initialization
  CanClose:= false;
  // Flag needed to execute once some processes in Form activation
  Initialized := False;
  iconized:= false;
  CompileDateTime:= StringToTimeDate({$I %DATE%}+' '+{$I %TIME%}, 'yyyy/mm/dd hh:nn:ss');
  // Intercept minimize system command
  Application.OnMinimize:=@OnAppMinimize;
  Application.OnQueryEndSession:= @OnQueryendSession;
  OS := 'Unk';
  // Some useful paths
  WRExecPath:=ExtractFilePath(Application.ExeName);
  UserPath := GetUserDir;
  UserAppsDataPath := UserPath;
  MusicPath:= Userpath+PathDelim+'Music'+PathDelim;
  If not DirectoryExists(MusicPath) then
  MusicPath:= '';
  {$IFDEF Linux}
    OS := 'Linux';
    CRLF := #10;
    LangStr := GetEnvironmentVariable('LANG');
    // Music folder
    MusicPath:= UserPath+PathDelim+Music+PathDelim;
    if not DirectoryExists(MusicPath) then
    MusicPath:= '';
    x := pos('.', LangStr);
    LangStr := Copy(LangStr, 0, 2);
    wxbitsrun := 0;
    OSTarget:= '';
  {$ENDIF}
  {$IFDEF WINDOWS}
    OS := 'Windows ';
    CRLF := #13#10;
    // get user data folder
    s := ExtractFilePath(ExcludeTrailingPathDelimiter(GetAppConfigDir(False)));
    if Ord(WindowsVersion) < 7 then
      UserAppsDataPath := s                     // NT to XP
    else
    UserAppsDataPath := ExtractFilePath(ExcludeTrailingPathDelimiter(s)) + 'Roaming'; // Vista to W10
    LazGetShortLanguageID(LangStr);
    Application.MainFormOnTaskBar:= true;
  {$ENDIF}
  ProgName := 'WebRadio';
  {$IFDEF CPU32}
     OSTarget := '32 bits';
     PluginsDir:= WRExecPath+PathDelim+'Plugins'+PathDelim;
  {$ENDIF}
  {$IFDEF CPU64}
     OSTarget := '64 bits';
     PluginsDir:= WRExecPath+PathDelim+'Plugins'+PathDelim;
  {$ENDIF}
  // Chargement des chaînes de langue...
  LangFile := TBbIniFile.Create(WRExecPath + LowerCase(ProgName)+'.lng');
  OSVersion:= TOSVersion.Create(LangStr, LangFile);
  version := GetVersionInfo.ProductVersion;
  LangNums := TStringList.Create;
  WebRadioAppsData := UserAppsDataPath + PathDelim + ProgName + PathDelim;
  if not DirectoryExists(WebradioAppsData) then CreateDir(WebRadioAppsData);
  if not DirectoryExists(WebRadioAppsData + PathDelim + 'images') then
  CreateDir(WebradioAppsData + PathDelim + 'images');
  // FPTimer must be enabled manually at startup
  MsgTimer.Enabled:= true;
  VuTimer.Enabled:= True;
  Application.ProcessMessages;
end;

procedure TFWebRadioMain.FormDestroy(Sender: TObject);
begin
  if assigned(FileStream) then FileStream.free;
  if cthread <> 0 then cthread:= 0;
  if assigned(LangFile) then FreeAndNil(LangFile);
  if assigned(LangNums) then FreeAndNil(LangNums)
end;

// Intercept minimize system system command to correct
// wrong window placement on restore from tray

procedure TFWebRadioMain.OnAppMinimize(Sender: TObject);
begin
  if FSettings.Settings.HideInTaskbar then
  begin
    PrevLeft:=self.left;
    PrevTop:= self.top;
    PTMnuIconizeClick(self);
    Iconized:= HideOnTaskbar;
  end;
end;

// Intercept end session : save all pending settings and load the program
// on next logon (Windows only for the moment. On linux, we use autostart
// and we delete autostart on next startup

procedure TFWebRadioMain.OnQueryendSession(var Cancel: Boolean);
{$IFDEF WINDOWS}
var
  reg: TRegistry;
  RunRegKeyVal, RunRegKeySz: string;
{$ENDIF}
begin
  if not FSettings.Settings.Startup then
  begin
    FSettings.Settings.Restart:= true;
    {$IFDEF WINDOWS}
      reg := TRegistry.Create;
      reg.RootKey := HKEY_CURRENT_USER;
      reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\RunOnce', True) ;
      RunRegKeyVal:= UTF8ToAnsi(ProgName);
      RunRegKeySz:= UTF8ToAnsi('"'+Application.ExeName+'"');
      reg.WriteString(RunRegKeyVal, RunRegKeySz) ;
      reg.CloseKey;
      reg.free;
    {$ENDIF}
    {$IFDEF Linux}
       SetAutostart(ProgName, Application.exename);
    {$ENDIF}
  end;
  BeforeClose;
  Application.ProcessMessages;
end;


procedure TFWebRadioMain.FormActivate(Sender: TObject);
begin
  if not Initialized then
  begin
    InitButtons;

    RegularHeight:= height;
    Initialize;
    //Check Update, async call to let stuff loading
    Application.QueueAsyncCall(@CheckUpdate, ChkVerInterval);

  end;
  // Initialisation du flux à zéro
  Chan:= 0;
end;

procedure TFWebRadioMain.InitButtons;
begin
  // init menu and buttons images
  CropBitmap(ILButtons, PMnuOpenRadio.Bitmap, true, 4);
  CropBitmap(ILButtons, PMnuReadFile.Bitmap, true, 5);
  CropBitmap(ILButtons, PMnuOpenURL.Bitmap, true, 6);
  CropBitmap(ILButtons, PMnuEqualizer.Bitmap, true, 7);
  CropBitmap(ILButtons, PMnuSettings.Bitmap, true, 8);
  CropBitmap(ILButtons, PMnuRadList.Bitmap, true, 9);
  CropBitmap(ILButtons, PMnuAbout.Bitmap, true, 10);
  CropBitmap(ILButtons, PMnuQuit.Bitmap, true, 11);
  CropBitmap(ILButtons, PMnuHelp.Bitmap, true, 15);
  CropBitmap(ILButtons, PTMnuAbout.Bitmap, true, 10);
  CropBitmap(ILButtons, PTMnuQuit.Bitmap, true, 11);
  CropBitmap(ILButtons, PTMnuMute.Bitmap, true, 2);
  CropBitmap(ILButtons, PMnuChooseRadio.Bitmap, true, 4);
  CropBitmap(ILButtons, PMnuDeletePreset.Bitmap, true, 14);
  ILButtons.GetBitmap(4, SBOpenRadio.Glyph);;
  ILButtons.GetBitmap(5, SBReadFile.Glyph);
  ILButtons.GetBitmap(6, SBOpenUrl.Glyph);
  ILButtons.GetBitmap(7, SBEqualizer.Glyph);
  ILButtons.GetBitmap(8, SBSEtings.Glyph);;
  ILButtons.GetBitmap(9, SBRadList.Glyph);
  ILButtons.GetBitmap(15, SBHelp.Glyph);
  ILButtons.GetBitmap(10, SBAbout.Glyph);
  ILButtons.GetBitmap(11, SBQuit.Glyph);
end;

// Initialization procedure on first form activation

procedure TFWebRadioMain.Initialize;
var
  i: integer;
  bass_timout: Integer;
  IniFile: TBbIniFile;
  TSB: TColorSpeedButton;
  EncVer: Int64;
begin
  if initialized then exit;
  // Now, main settings
  FSettings.Settings.AppName:= LowerCase(ProgName);
  FSettings.LOSVer.Caption:= OsVersion.VerDetail;
  FRadios.Radios.AppName := LowerCase(ProgName);
  ConfigFileName:= WebRadioAppsData+'settings.xml';
  RadiosFileName:= WebRadioAppsData+'wradios.xml';
  FSettings.Settings.LangStr:= LangStr;
  // Check inifile with URLs, if not present, then use default
  IniFile:= TBbInifile.Create('webradio.ini');
  AboutBox.ChkVerURL := IniFile.ReadString('urls', 'ChkVerURL','https://github.com/bb84000/webradio/releases/latest');
  AboutBox.UrlWebsite:= IniFile.ReadString('urls', 'UrlWebSite','https://www.sdtp.com');
  AboutBox.UrlSourceCode:=IniFile.ReadString('urls', 'UrlSourceCode','https://github.com/bb84000/webradio');
  // Language dependent variables are updated in ModLangue procedure
  //AboutBox.Width:= 400; // to have more place for the long product name
  AboutBox.Image1.Picture.LoadFromLazarusResource('webradio32');
  AboutBox.LProductName.Caption := GetVersionInfo.FileDescription;
  AboutBox.LCopyright.Caption := GetVersionInfo.CompanyName + ' - ' + DateTimeToStr(CompileDateTime);
  AboutBox.LVersion.Caption := 'Version: ' + Version + ' (' + OS + OSTarget + ')';
  AboutBox.LUpdate.Hint := AboutBox.sLastUpdateSearch + ': ' + DateToStr(FSettings.Settings.LastUpdChk);
  AboutBox.Version:= Version;
  AboutBox.ProgName:= ProgName;
  ChkVerInterval:= IniFile.ReadInt64('urls', 'ChkVerInterval', 3);
  if Assigned(IniFile) then IniFile.free;
  LoadSettings(ConfigFileName);
  ModLangue;
  if (Pos('64', OSVersion.Architecture)>0) and (OsTarget='32 bits') then
    MsgDlg(Caption, sUse64bit, mtInformation,  [mbOK], [OKBtn]);
  ShowBtnBar;

  // Test version de Bass.DLL
  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then
  begin
    ShowMessage(ErrBassVersion);
    Close;
  end;
  if (not BASS_Init(-1, 44100, 0, Handle, nil)) then
  begin
     ShowMessage(BassErrArr[BASS_ERROR_INIT]);
    Close;
  end;
  bass_timout:= 15000;
  BASS_SetConfig(BASS_CONFIG_NET_TIMEOUT, bass_timout);
  BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1); // enable playlist processing
  BASS_SetConfig(BASS_CONFIG_NET_PREBUF, 0); // minimize automatic pre-buffering, so we can do it (and display it) instead

  // in case startup was done after a session end
  if FSettings.Settings.Restart then
  begin
    FSettings.Settings.Restart:= false;
  end;
  {$IFDEF Linux}
     if not FSettings.Settings.Startup  then UnsetAutostart(ProgName);
  {$ENDIF}
  // In case of program's first use, create settings and radio files
  if length(FSettings.Settings.LastVersion)=0 then FSettings.Settings.LastVersion:= version;
  FSettings.Settings.OnChange := @SettingsOnChange;
  FSettings.Settings.OnStateChange := @SettingsOnStateChange;
  // First time launch set default radio
  FRadios.DefRadio:= Default(TRadio);
  FRadios.DefRadio.Name:= 'France Inter';
  FRadios.DefRadio.url:= 'http://direct.franceinter.fr/live/franceinter-midfi.mp3';
  FRadios.DefRadio.comment:= 'Radio par défaut';
  FRadios.DefRadio.favicon:= 'France_Inter.png';
  FRadios.WebradioAppsData:= WebradioAppsData;
  FRadios.DefRadio.Tag:= False;
  // Now load radios list
  FRadios.LoadRadios(RadiosFileName);
  FRadios.Radios.OnChange:= @RadiosOnChange;
  LoadPresets;
  Application.Title:=Caption;
  // Chargement des plugins
  {$IFDEF WINDOWS}
    // Chargement du plugin WMA (Windows uniquement)
    BassWMA:= BASS_PluginLoad(PChar(PluginsDir+'basswma.dll'), 0);  //'plugins\basswma.dll', 0);
    if BassWMA = 0 then  ShowMessage(Format(ErrNoBassPLAY, ['WMA', LineEnding, 'Microsoft']));
    // Chargement des plugins AAC et FLAC
    BassAAC:= BASS_PluginLoad(PChar(PluginsDir+'bass_aac.dll'), 0);
    BassFLAC:= BASS_PluginLoad(PChar(PluginsDir+'bassflac.dll'), 0);
  {$ENDIF}
  {$IFDEF LINUX}
    BassAAC:= BASS_PluginLoad(PChar(PluginsDir+'libbass_aac.so', 0);
    BassFLAC:= BASS_PluginLoad(PChar(PluginsDir+'libbassflac.so', 0);
  {$ENDIF}
  if BassAAC = 0 then ShowMessage(Format(ErrNoBassPLAY, ['AAC', LineEnding, 'AAC']));
  if BassFLAC = 0 then ShowMessage(Format(ErrNoBassPLAY, ['FLAC', LineEnding, 'FLAC']));
  EncVer:= BASS_Encode_GetVersion;
  if Hiword(EncVer) <> BASSVERSION then ShowMessage(Format(ErrBassEncVer, [LineEnding]));
  mp3enclib:= Load_bassencmp3dll (PluginsDir+bassencmp3dll) ;
  aacenclib:= Load_bassencaacdll (PluginsDir+bassencaacdll) ;
  oggenclib:= Load_bassencoggdll (PluginsDir+bassencoggdll) ;
  Application.ProcessMessages;
  if not mp3enclib then ShowMessage(Format(ErrNoBassEnc, ['MP3', LineEnding, 'MP3']));
  if not aacenclib then ShowMessage(Format(ErrNoBassEnc, ['AAC', LineEnding, 'AAC']));
  if not oggenclib then ShowMessage(Format(ErrNoBassEnc, ['OGG', LineEnding, 'OGG']));
  try
    CurRadio:= FRadios.FindbyUID(FSettings.Settings.LastRadio);
  except
    CurRadio.url:= FSettings.Settings.LastUrl;
  end;
  if CurRadio.uid=0 then CurRadio.url:= FSettings.Settings.LastUrl;
  LastVol:= FSettings.Settings.LastVolume;
  TBVolume.Position:= LastVol;
  TBVolumeChange(nil);
  if CurRadio.uid > 0 then
  begin
  // Search if there is a preset for the saved radio
    for i:= 1 to 20 do
    begin
      if Curradio.UID= FRadios.Radios.Presets[i] then
      begin
        TSB:= FindComponent('SBPreset' + IntToStr(i)) as TColorSpeedButton;
        break;
      end;
    end;
    if TSB<> nil then SBPresetClick(TSB)
    else PlayRadio(CurRadio);
  end else PlayRadio(CurRadio);
  Initialized:= true;
end;

// Load preset radios

procedure TFWebRadioMain.LoadPresets;
var
  i: Integer;
  tmpradio: TRadio;
  TSB: TColorSpeedButton;
begin
  for i := 1 to 20 do
  begin
    TSB:= FindComponent('SBPreset' + IntToStr(i)) as TColorSpeedButton;
    if TSB <> nil then
    begin
      tmpradio:= FRadios.FindbyUID(FRadios.Radios.Presets[i]);
      if tmpradio.uid>0 then
      begin
        TSB.Font.Style:= [fsBold];
        TSB.Font.Color:= clDefault;
        TSB.StateNormal.Color:= clBtnFace;
        TSB.StateHover.Color:= clDefault;
        TSB.Hint:= tmpradio.Name;
      end else
      begin
        TSB.Font.Style:= [];
        TSB.Font.Color:= clScrollBar;
        TSB.StateNormal.Color:= clMedGray;
        TSB.StateHover.Color:= clMedGray;
        TSB.Hint:= '';
      end;
    end;
  end;
end;

// Change colors after settings change

procedure TFWebRadioMain.ChangeColors;
begin
  LRadioName.Font.Color:= FSettings.Settings.DisplayText;
  LStatus.Font.Color:= FSettings.Settings.DisplayText;
  Lstereo.Font.Color:= FSettings.Settings.DisplayText;
  LBitrate.Font.Color:= FSettings.Settings.DisplayText;
  LFrequency.Font.Color:= FSettings.Settings.DisplayText;
  LRecording.Font.Color:= FSettings.Settings.DisplayText;
  LPause.Font.Color:= FSettings.Settings.DisplayText;
  LTag.Font.Color:= FSettings.Settings.DisplayText;
  PDisplay.Color:= FSettings.Settings.DisplayBack;
  Color:= FSettings.Settings.GenBack;
  PMain.Font.Color:= FSettings.Settings.GenText;
end;

procedure TFWebRadioMain.LoadSettings(Filename: string);
var
  winstate: TWindowState;
  i: integer;
begin
  with FSettings do
  begin
    Settings.LoadXMLFile(Filename);
    self.Position:= poDesktopCenter;
    if Settings.SavSizePos then
    try
      WinState := TWindowState(StrToInt('$' + Copy(Settings.WState, 1, 4)));
      self.Top := StrToInt('$' + Copy(Settings.WState, 5, 4));
      self.Left := StrToInt('$' + Copy(Settings.WState, 9, 4));
      //self.Height := StrToInt('$' + Copy(Settings.WState, 13, 4));
      //self.Width := StrToInt('$' + Copy(Settings.WState, 17, 4));
      self.WindowState := WinState;
      PrevLeft:= self.left;
      PrevTop:= self.top;
      if WinState = wsMinimized then
      begin
        //Application.Minimize;
        PTMnuIconizeClick(self);
      end;
    except
    end;
    TrayRadio.visible:= Settings.HideInTaskbar;
    //if self.width < 625 then self.width:= 625 ; // Change when equakuzer will bei present
    if settings.StartMini then
    begin
      //Application.Minimize;
      PTMnuIconizeClick(self);
    end;
    // Détermination de la langue (si pas dans settings, langue par défaut)
    if Settings.LangStr = '' then Settings.LangStr := LangStr;
    LangFile.ReadSections(LangNums);
    if LangNums.Count > 1 then
    begin
      CBLangue.Clear;;
      for i := 0 to LangNums.Count - 1 do
      begin
        FSettings.CBLangue.Items.Add(LangFile.ReadString(LangNums.Strings[i], 'Language',
          'Aucune'));
        if LangNums.Strings[i] = Settings.LangStr then
        begin
          LangFound := True;
          FSettings.CBLangue.ItemIndex:= i;
        end;
      end;
    end;
    // Si la langue n'est pas traduite, alors on passe en Anglais
    if not LangFound then
    begin
      Settings.LangStr := 'en';
    end;
    if length(Settings.Encoding) = 0 then Settings.Encoding:= 'WAV';
  end;
  // LOad font
  If (FSettings.Settings.DataFolder='') and (MusicPath <> '') then
  FSettings.Settings.DataFolder:= MusicPath;
  if Fsettings.Settings.RadioFont <> '' then
  LRadioName.Font.Name:= Fsettings.Settings.RadioFont;
  ChangeColors;
  Modlangue;
  SettingsChanged := false;
end;

// Save configuration and database to file

function TFWebradioMain.SaveConfig(typ: TSaveMode): boolean;
var
  FilNamWoExt: string;
  i: integer;
begin
  Result := False;
  if (Typ= Setting) or (Typ = All) then
  begin
    //FSettings.Settings.DataFolder:= WebRadioAppsData;
    FSettings.Settings.WState:= '';
    if Top < 0 then Top:= 0;
    if Left < 0 then Left:= 0;
    FSettings.Settings.WState:= IntToHex(ord(WindowState), 4)+IntToHex(Top, 4)+IntToHex(Left, 4)+IntToHex(Height, 4)+IntToHex(width, 4);
    FSettings.Settings.Version:= version;
    if FileExists (ConfigFileName) then
    begin
      if (Typ = All) then
      begin
        // On sauvegarde les versions précédentes parce que la base de données a changé
        FilNamWoExt:= TrimFileExt(ConfigFileName);
        if FileExists (FilNamWoExt+'.bk5')                   // Efface la plus ancienne
        then  DeleteFile(FilNamWoExt+'.bk5');                // si elle existe
        For i:= 4 downto 0
        do if FileExists (FilNamWoExt+'.bk'+IntToStr(i))     // Renomme les précédentes si elles existent
           then  RenameFile(FilNamWoExt+'.bk'+IntToStr(i), FilNamWoExt+'.bk'+IntToStr(i+1));
        RenameFile(ConfigFileName, FilNamWoExt+'.bk0');
        FRadios.SaveRadios(RadiosFileName);
      end;
      // la base n'a pas changé, on ne fait pas de backup
      FSettings.settings.SaveToXMLfile(ConfigFileName);
    end else
    begin
      FRadios.SaveRadios(RadiosFileName);
      FSettings.settings.SaveToXMLfile(ConfigFileName); ;
    end;
    result:= true;
  end;
end;

function TFWebRadioMain.HideOnTaskbar: boolean;
begin
  result:= false;
  if (WindowState=wsMinimized) and FSettings.Settings.HideInTaskbar then
  begin
    result:= true;
    visible:= false;
  end;
end;

procedure TFWebRadioMain.FormChangeBounds(Sender: TObject);
begin
   SettingsChanged:= FSettings.Settings.SavSizePos;
end;

// Event fired by any change of settings values

procedure TFWebRadioMain.SettingsOnChange(Sender: TObject);
begin
  SettingsChanged := True;
end;


procedure TFWebRadioMain.MsgTimerTimer(Sender: TObject);
var
  s: string;
const
  SrtmTyp: array [1..6] of String = (' WMA',' MP3',' OGG',' AAC', ' WAV', ' FLAC');
begin
  if lparamArr[WP_Error].state then
  begin
    lparamArr[WP_Error].state:= false;
    lparamArr[WP_Connecting].state:= false;
    LStatus.Caption:= 'Non connecté';
    try
      LTag.Caption:= BassErrArr[lparamArr[WP_Error].value];
    except         // UNknown error
      LTag.Caption:= BassErrArr[length(BassErrArr)-1]; // We have set the unknown error in last position
    end;
    LRadioName.Caption:= CurRadio.name;
    LStereo.Caption:= '';
    LBitrate.Caption:= '';
    LFrequency.Caption:= '';
    LRecording.Caption:= '';
    LPause.Caption:= '';
    Error:= true;
     //LTag.Caption:= IntToStr(lparamArr[WP_Error].value);//BassErrArr[lparamArr[WP_Error].value];
     exit;
  end;
  // WP_Connecting message replacement
  if lparamArr[WP_Connecting].state then
  begin
    Connecting_beg:= 0;
    LStatus.Caption := sConnectStr;
    Caption:= DefaultCaption;
    Lstereo.Caption:= '';
    LBitrate.Caption:= '';
    LFrequency.Caption:= '';
    LPause.Caption:= '';
    if not error then LTag.caption := ' ';
    LRecording.Caption:= '';
    FileLength:= 0;
    if length(CurRadio.Name) > 0
    then s:= sLoadStr+' '+CurRadio.Name
    else s:= sLoadStr+' '+lparamArr[WP_Connecting].value;
    LRadioName.Caption:= (s);
    lparamArr[WP_Connecting].state:= False;
  end;
  // WP_Radioname message replacement
  if lparamArr[WP_RadioName].state then
  begin
    // Radio preset
    if length(CurRadio.Name) > 0 then
    begin
      Caption:= CurRadio.Name;
      s:= lparamArr[WP_RadioName].value;
      if length(s)< 2 then s:= Caption;
      LRadioName.Caption:= TrimLeft(UTF8ToAnsi(s));
    end else
    begin
      Caption:= lparamArr[WP_RadioName].value;
      if length(caption) =0 then Caption:= CurRadio.url;
      LRadioName.Caption:= Caption;
   end;
   Application.Title:= Caption ;
   TrayRadio.Hint:= Caption;
          //LastUrl:= CurRadio.url;
          //PauseBtn.Enabled:= True;
          //StopBtn.Enabled:= ;
          //PlayBtn.Enabled:= False;
          //RecBtn.Enabled:= True;
          //Recording:= False;
          //Paused:= False;
          //Stopped:= False;
    //b_radioname:= false
    lparamArr[WP_RadioName].state := false;
  end;
  // WP_Progress message replacement
  if lparamArr[WP_Progress].state then
  begin
    LStatus.Caption:= Format(sBufferStr+' %d%%', [int64(lparamArr[WP_Progress].value)]); //[i_progress]);
    lparamArr[WP_Progress].state:= false;
  end;
  // WP_Connected message replacement
  if lparamArr[WP_Connected].state then
  begin
    if CanalInfo.chans < 2 then LStereo.caption:= sMonoCaption else LStereo.caption:= sStereoCaption ;
    LFrequency.Caption := IntToStr(CanalInfo.freq)+' Hz';
    if ((lparamArr[WP_Connected].value>0) and (lparamArr[WP_Connected].value<7)) then LStatus.Caption:= sConnectedStr+SrtmTyp[lparamArr[WP_Connected].value]
      else LStatus.Caption:= sConnectedStr;
  end ;
    lparamArr[WP_Connected].state:= False;
    // WP_Title message replacement
  if lparamArr[WP_Title].state then;
  begin
  if error then exit;
  s:= lparamArr[WP_Title].value;
    if length(s) > 1 then CurRadioTitle:= s //LTag.Caption:= s
    else CurRadioTitle:= ' ';//LTag.Caption:= ' ';
    if length (s) > 1 then
    begin
      Trayradio.Hint:= Caption+#13#10+s
     //If not TrayRadio.applicationvisible then TrayRadio.BalloonHint (Caption, s);
     end else Trayradio.Hint:= Caption;
    if (not Iconized) then LTag.Caption:= CurRadioTitle;
    lparamArr[WP_Title].state:= false;
  end ;
  // WP_Bitrate message replacement
  if lparamArr[WP_Bitrate].state then
  begin
    LBitrate.Caption := Inttostr(int64(lparamArr[WP_Bitrate].value))+' Kb/s';
    lparamArr[WP_Bitrate].state:= false;
  end;
  if lparamArr[WP_Hint].state then
  begin
    LRadioName.Hint:= lparamArr[WP_Hint].value;
    lparamArr[WP_Hint].state:= False;
  end;
  inc(MsgTimerCount);
end;

procedure TFWebRadioMain.PMnuEqualizerClick(Sender: TObject);
var
  i: integer;
  TBE: TTrackBar;
begin
  application.ProcessMessages;
  for i:= 1 to 9 do
  begin
    TBE:= FEqualizer.FindComponent('TBEq'+IntToStr(i)) as TTrackBar;
    TBE.Position:= FSettings.Settings.EquFreqs[i]+15;
  end;
  FEqualizer.CBEqualize.Checked:= FSettings.Settings.EquEnabled;
  if FEqualizer.showModal= mrOK then
  begin
    for i:= 1 to 9 do
    begin
      TBE:= FEqualizer.FindComponent('TBEq'+IntToStr(i)) as TTrackBar;
      FSettings.Settings.EquFreqs[i]:= TBE.Position-15;
    end;
    FSettings.Settings.EquEnabled:= FEqualizer.CBEqualize.Checked;
  end;
  SetEqual(false);
  if FSettings.Settings.EquEnabled then SetEqual(true);

end;



procedure TFWebRadioMain.PMnuRadListClick(Sender: TObject);
begin
  FRadios.showmodal;

  LoadPresets;
end;

procedure TFWebRadioMain.PMnuChooseRadioClick(Sender: TObject);
var
  Item: TMenuItem;
  i: integer;
begin
  PMnuRadiosList.Items.Clear;
  for i:= 0 to FRadios.Radios.Count-1 do
  begin
    Item := TMenuItem.Create(PMnuRadiosList);
    Item.Name:= 'PMnuRadio'+InttoStr(i);
    Item.Caption := FRadios.Radios.GetItem(i).name;
    Item.Tag:= i;
    if TMenuItem(SEnder).name='PMnuChooseRadio' then Item.OnClick := @PMnuRadioListItemClick;
    if TSpeedButton(Sender).Name='SBChooseRadio' then Item.OnClick := @PMnuRadioListItemClick;;
    if TMenuItem(SEnder).name='PMnuOpenRadio' then Item.OnClick := @PMnuOpenRadioItemClick;
    if TMenuItem(SEnder).name='SBOpenRadio' then Item.OnClick := @PMnuOpenRadioItemClick;
    PMnuRadiosList.Items.Add(Item);
  end;
  //LoadPresets;
  PMnuRadiosList.PopUp;
end;

procedure TFWebRadioMain.PMnuDeletePresetClick(Sender: TObject);
begin
  FRadios.Radios.Presets[TmpPreset]:= 0;
  LoadPresets;
end;

procedure TFWebRadioMain.PMainClick(Sender: TObject);
begin

end;

procedure TFWebRadioMain.PMnuOpenRadioItemClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  try
    s:= Copy(TMenuItem(SEnder).Name, 10, 2);
    i:= StringToInt(s);
    CurRadio:= FRadios.Radios.GetItem(i);
    PlayRadio(CurRadio);
    FSettings.Settings.LastRadio:= CurRadio.uid;
    LoadPresets;
  except
  end;
end;

procedure TFWebRadioMain.PMnuRadioListItemClick(Sender: TObject);
var
  s: String;
  i: Int64;
begin
  try
    s:= Copy(TSpeedButton(SEnder).Name, 10, 2);
    i:= StringToInt(s);
    FRadios.Radios.Presets[TmpPreset]:= FRadios.Radios.GetItem(i).UID;
    LoadPresets;
  except
  end;
end;

procedure TFWebRadioMain.PMnuAboutClick(Sender: TObject);
var
  chked: Boolean;
  alertmsg: String;
begin
  AboutBox.LastUpdate:= FSettings.Settings.LastUpdChk;
  chked:= AboutBox.Checked;
  AboutBox.ErrorMessage:='';
  AboutBox.ShowModal;
  // If we have checked update and got an error
  if length(AboutBox.ErrorMessage)>0 then
  begin
    alertmsg := TranslateHttpErrorMsg(AboutBox.ErrorMessage, HttpErrMsgNames);
    if AlertDlg(Caption,  alertmsg, [OKBtn, CancelBtn, sNoLongerChkUpdates],
                    true, mtError)= mrYesToAll then FSettings.Settings.NoChkNewVer:= true;
  end;
  // Truncate date to avoid changes if there is the same day (hh:mm are in the decimal part of the date)
  if (not chked) and AboutBox.Checked then FSettings.Settings.LastVersion:= AboutBox.LastVersion;
  if trunc(AboutBox.LastUpdate) > trunc(FSettings.Settings.LastUpdChk) then
  begin
    FSettings.Settings.LastUpdChk:= AboutBox.LastUpdate;
  end;
end;

procedure TFWebRadioMain.PMnuOpenURLClick(Sender: TObject);
var
 s: string;
begin
  s:= InputDlg(DefaultCaption, sEnterUrl, '', OKBtn, CancelBtn, 0 );
  if length(s) > 0 then
  try
    CurRadio.Name:='';
    Curradio.uid:= 0;
    Curradio.url:= s;
    Curradio.favicon:='';
    PlayRadio(CurRadio);
    FSettings.Settings.LastUrl:= s;
    FSettings.Settings.LastRadio:= 0;
    LoadPresets;
  except
  end;
end;

procedure TFWebRadioMain.PMnuQuitClick(Sender: TObject);
begin
  CanClose:= true;
  close;
end;

// File reading and recording

procedure TFWebRadioMain.PMnuReadFileClick(Sender: TObject);
begin
  ODAudio.Title:= 'Ouvrir un fichier audio';
  if ODAudio.Execute then
  begin;
    PlayFile(ODAudio.FileName);
    SBRecord.Enabled:= false;
    SBStop.Enabled:= true;
    SBPause.Enabled:= true;

  end;
end;



procedure TFWebRadioMain.SBStopClick(Sender: TObject);
begin
  LPause.Caption:= sStopCaption;
  BASS_ChannelPause(Chan);
  SBStop.Enabled:= False;
  SBPause.Enabled:= False;
  SBPlay.Enabled:= True;
  Paused:= False;
  Stopped:= True;
end;

procedure TFWebRadioMain.SBPlayClick(Sender: TObject);
begin
  BASS_ChannelPlay(Chan, not paused);
  SBStop.Enabled:= not IsRadio;
  SBPause.Enabled:= True;
  SBPlay.Enabled:= False;
  Paused:= False;
  Stopped:= False;
  LPause.Caption:= '';
end;

procedure TFWebRadioMain.SBPresetContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  s: String;
  i, uid: Int64;
begin
  s:= Copy(TColorSpeedButton(Sender).Name, 9, 2);
  i:= StringToInt(s);
  TmpPreset:= i;
  uid:= FRadios.Radios.Presets[i];
  if (uid <= 0) then
  begin
    PMnuDeletePreset.Enabled:= false;
    CropBitmap(ILButtons, PMnuDeletePreset.Bitmap, false, 14);
  end else
  begin
    PMnuDeletePreset.Enabled:= true;
    CropBitmap(ILButtons, PMnuDeletePreset.Bitmap, true, 14);
    TColorSpeedButton(Sender).Hint:= FRadios.FindbyUID(uid).name;
  end;
end;

procedure TFWebRadioMain.SBPauseClick(Sender: TObject);
begin
  BASS_ChannelPause(Chan);
  SBStop.Enabled:= False;
  SBPause.Enabled:= False;
  SBPlay.Enabled:= True;
  Paused:= True;
  Stopped:= False;
  LPause.Caption:= sPauseCaption;
end;


procedure TFWebRadioMain.ShowBtnBar;
begin
  If FSettings.Settings.ShowBtnBar then
  begin
    PnlTop.Visible:= true;
    Height:= RegularHeight;
        {


    FMinHeight:= BarsHeight+GetSystemMetrics(SM_CYCAPTION)+142;
    Settings.HideBars:= False;
    PnlTop.Visible:= True;
    PnlStatus.Visible:= True;
    PMnuHideBars.Caption:= SMnuMaskBars;
    Height:= HideBarsheight+BarsHeight;}
  end else
  begin
    PnlTop.Visible:= False;
    Height:= RegularHeight-PnlTop.Height ;
    {FMinHeight:= GetSystemMetrics(SM_CYCAPTION)+142;
    Settings.HideBars:= True;

    PnlStatus.Visible:= False;
    PMnuHideBars.Caption:= SMnuShowBars;
    Height:= ShowBarsHeight-BarsHeight;}
  end;
end;

procedure TFWebRadioMain.PMnuSettingsClick(Sender: TObject);
var
  oldlng, oldfont, i: integer;
  TRB: TRadioButton;
begin
  with FSettings do
  begin
    CBStartup.Checked:= Settings.Startup;
    CBStartMini.Checked:= Settings.StartMini;
    CBSavSizePos.Checked:= Settings.SavSizePos;
    CBNoChkNewVer.Checked:= Settings.NoChkNewVer;
    CBShowBtnBar.Checked:= Settings.ShowBtnBar;
    CBHideInTaskBar.Checked:= Settings.HideInTaskbar;
    EDataFolder.text:= Settings.DataFolder;
    RBMP3.Enabled:= mp3enclib;
    RBAAC.Enabled:= aacenclib;
    RBOGG.Enabled:= oggenclib;
    if LangNums.Count > 1 then
    begin
      CBLangue.Clear;;
      for i := 0 to LangNums.Count - 1 do
      begin
        FSettings.CBLangue.Items.Add(LangFile.ReadString(LangNums.Strings[i], 'Language',
          'Aucune'));
        if LangNums.Strings[i] = Settings.LangStr then
        begin
          LangFound := True;
          FSettings.CBLangue.ItemIndex:= i;
          oldlng:= i;
        end;
      end;
    end;
    for i:= 0 to CBFonts.Items.Count-1 do
    begin
      if CBFonts.Items[i]=LRadioName.Font.Name then
      begin
        CBFonts.ItemIndex:= i;
        break;
      end;
    end;
    oldfont:= CBFonts.ItemIndex;
    // Get encoding format
    TRB:= FindComponent('RB'+Settings.Encoding) as TRadioButton;
    if TRB <> nil then TRB.Checked:= true;
    // Disable bitrate and sampling settings in WAV recording, as Windows
    // display its own dialog box.
    RBEnCodingChange(nil);
    // Get sampling frequency
    for i:= 0 to CBSampling.Items.Count-1 do
    begin
      if StringToInt(CBSampling.Items[i])= Settings.Sampling then
      begin
        CBSampling.ItemIndex:= i;
        break;
      end;
    end;
    // Get bitrate
    for i:= 0 to CBBitrate.Items.Count-1 do
    begin
      if StringToInt(CBBitrate.Items[i])= Settings.Bitrate then
      begin
        CBBitrate.ItemIndex:= i;
        break;
      end;
    end;
    CPDisplayText.Color:= Settings.DisplayText;
    CPDisplayBack.Color:= Settings.DisplayBack;
    CPGenText.Color:= Settings.GenText;
    CPGenBack.Color:=Settings.GenBack ;
    if ShowModal = mrOK then
    // set new encoding format
    begin
      if RBMP3.Checked then Settings.Encoding:= 'MP3';
      if RBOGG.Checked then Settings.Encoding:= 'OGG';
      if RBAAC.Checked then Settings.Encoding:= 'AAC';
      if RBWAV.Checked then Settings.Encoding:= 'WAV';
      try
        Settings.Sampling:= StringToInt(CBSampling.Items[CBSampling.ItemIndex]);
      except
        Settings.Sampling:= StringToInt(CBSampling.Items[2]);
      end;
      try
        Settings.Bitrate:= StringToInt(CBBitrate.Items[CBBitrate.ItemIndex]);
      except
        Settings.Bitrate:= StringToInt(CBBitrate.Items[3]);
      end;
      Settings.Startup := CBStartup.Checked;
      Settings.StartMini := CBStartMini.Checked;
      Settings.SavSizePos := CBSavSizePos.Checked;
      Settings.HideInTaskbar:= CBHideInTaskBar.Checked;
      Settings.NoChkNewVer := CBNoChkNewVer.Checked;
      Settings.ShowBtnBar:= CBShowBtnBar.checked;
      Settings.DataFolder:= EDataFolder.text;
      if not DirectoryExists(Settings.DataFolder) then CreateDir(Settings.DataFolder);
      Settings.LangStr := LangNums.Strings[CBLangue.ItemIndex];
      if (CBLangue.ItemIndex<> oldlng) then ModLangue;
      try
        if CBFonts.ItemIndex <> oldfont then
        LRadioName.Font.Name:= CBFonts.SelText;
      except
      end;
      ShowBtnBar;
      TrayRadio.visible:= Settings.HideInTaskbar;   // in case of change
      Settings.DisplayText:= CPDisplayText.Color;
      Settings.DisplayBack:= CPDisplayBack.Color;
      Settings.GenText:= CPGenText.Color;
      Settings.GenBack:= CPGenBack.Color;
      ChangeColors;
    end;
  end;
end;

// Launch a preset radio

procedure TFWebRadioMain.SBPresetClick(Sender: TObject);
var
  s: String;
  i, uid: Int64;
begin
  isRadio:= true;
  s:= Copy(TColorSpeedButton(SEnder).Name, 9, 2);
  i:= StringToInt(s);
  uid:= FRadios.Radios.Presets[i];
  CurRadio:= FRadios.FindbyUID(uid);
  FSettings.Settings.LastRadio:= CurRadio.uid;
  if CurRadio.uid > 0 then
  begin
    LTag.Caption:= ' ';
    LStatus.Caption:= sConnectStr;
    PlayRadio(Curradio);
    LoadPresets;
    CurPreset:= TColorSpeedButton(SEnder);
    CurPreset.Font.Color:= clRed;
  end;
end;

procedure TFWebRadioMain.SBRecordChangeBounds(Sender: TObject);
begin

end;

procedure TFWebRadioMain.SBRecordClick(Sender: TObject);
var
  filename: string;
  ID_Tag: String;
  rectype: String;
  recBitrate: string;
  recSampling: string;
  TRB: TRadioButton;
  acmformlen: DWORD = 0;
begin
  RecType:= FSettings.Settings.Encoding;
  TRB:= FSettings.FindComponent('RB'+RecType) as TRadioButton;
  if not TRB.Enabled then
  begin
    ShowMessage(Format(ErrUnsupportedEnc, [RecType]));
    exit;
  end;
  recBitrate:=IntToStr(FSettings.Settings.Bitrate);
  recSampling:=IntToStr(FSettings.Settings.Sampling);
  // Launch recording if not recording
  if not recording then
  begin
    ILButtons.GetBitmap(1, SBRecord.Glyph); // before blinking with timer
    filename:=FSEttings.Settings.DataFolder+CurRadio.Name+FormatDateTime('_d_mm_yy_hh_nn_ss', Now)+'.'+LowerCase(rectype);
    filename:= StringReplace(filename, ' ', '_', [rfReplaceAll] );
    If RecType='MP3' then
    begin
      ID_Tag:= ' -s '+recSampling+'-abr  -b '+recBitrate+' --tt "'+CurRadio.name+'" --ta "WebRadio - bb - sdtp" --ty "'+InttoStr(CurrentYear)+'"';
      if BASS_Encode_MP3_StartFile(chan,PChar(ID_Tag) , BASS_ENCODE_AUTOFREE, PChar(filename)) = 0 then
      begin
        ShowMessage(Format(ErrEncoding, [BASS_ErrorGetCode]));
        exit;
      end  ;
    end;
    if rectype='WAV' then
    begin
      acmformlen:=BASS_Encode_GetACMFormat(0, nil, 0, nil, 0); // get suggested format buffer size
      acmForm := AllocMem(acmFormLen);
      //acmForm^.wFormatTag := 1 ; //WAVE_FORMAT_PCM;
      BASS_Encode_GetACMFormat(Chan, acmForm, acmFormLen, nil, BASS_ACM_DEFAULT);
      if BASS_Encode_StartACMFile(chan, acmform, BASS_ENCODE_AUTOFREE, PChar(filename)) = 0 then
      begin
        ShowMessage(Format(ErrEncoding, [BASS_ErrorGetCode]));
        exit;
      end;
    end;
    if rectype='AAC' then
    begin
      ID_Tag:=' --vbr 0 --bitrate '+recBitrate+'000 ';       //--vbr can be 0 (CBR) or 1-5 (VBR levels). --bitrate <value> ... bitrate (bps) for CBR.
      if BASS_Encode_AAC_StartFile(chan,PChar(ID_Tag) , BASS_ENCODE_AUTOFREE, PChar(filename)) = 0 then
      begin
        ShowMessage(Format(ErrEncoding, [BASS_ErrorGetCode]));
        exit;
      end  ;
    end;
    if rectype='OGG' then
    begin
      ID_Tag:=' -b '+recBitrate+' -t "'+CurRadio.name+'" -a "WebRadio - bb - sdtp"  -d "'+InttoStr(CurrentYear)+'"';
      if BASS_Encode_OGG_StartFile(chan,PChar(ID_Tag) , BASS_ENCODE_AUTOFREE, PChar(filename)) = 0 then
      begin
        ShowMessage(Format(ErrEncoding, [BASS_ErrorGetCode]));
        exit;
      end  ;
    end;
    BASS_ChannelPlay(chan, False); // start the channel encoding
    LRecording.Caption:= sRecordingCaption+' '+rectype;
    Recording_beg:= now;
    SBRecord.Hint:= sStopRecordingHint;
    SBPause.Enabled:= false;
    Recording:= true;
    RecordBtnTimer.Enabled:= true;
    RecordBtnTimerCount:= 0;
  end else
  begin
    ILButtons.GetBitmap(0, SBRecord.Glyph);
    BASS_Encode_Stop (chan);
    LRecording.Caption:= '';
    SBRecord.Hint:= sRecordingHint;
    SBPause.Enabled:= true;
    Recording:= false;
    RecordBtnTimer.Enabled:= false;
    if assigned (acmForm) then freemem(acmForm);
  end;
end;

procedure TFWebRadioMain.RecordBtnTimerTimer(Sender: TObject);
begin
   ILButtons.GetBitmap(RecordBtnTimerCount mod 2, SBRecord.Glyph);
  inc(RecordBtnTimerCount);
end;

procedure TFWebRadioMain.SBHelpClick(Sender: TObject);
begin
  OpenDocument(HelpFile);
end;

procedure TFWebRadioMain.SBMuteClick(Sender: TObject);
begin
  if Muted then
  begin
    BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, LastVol * 100);
    ILButtons.GetBitmap(2, SBMute.Glyph);
    CropBitmap(ILButtons, PTMnuMute.Bitmap, true, 2);
    SBMute.Hint:= sMuteHint;
    PTMnuMute.Caption:= sMuteMenu;
    Muted:= False;
  end else
  begin
    BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, 0);
    ILButtons.GetBitmap(3, SBMute.Glyph);
    CropBitmap(ILButtons, PTMnuMute.Bitmap, true, 3);
    SBMute.Hint:= sMutedHint;
    PTMnuMute.Caption:= sMutedMenu;
    Muted:= True;
  end;

end;

procedure TFWebRadioMain.TBVolumeChange(Sender: TObject);
begin
  LastVol:= TBVolume.Position;
  FSettings.Settings.LastVolume:= LastVol;
  BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, LastVol * 100);
end;

// Tray menu

procedure TFWebRadioMain.PMnuTrayPopup(Sender: TObject);
begin
  PTMnuRestore.Enabled:= (WindowState=wsMaximized) or (WindowState=wsMinimized);
  CropBitmap(ILButtons, PTMnuRestore.Bitmap, PTMnuRestore.Enabled, 12);
  PTMnuIconize.Enabled:= not (WindowState=wsMinimized);
  CropBitmap(ILButtons, PTMnuIconize.Bitmap, PTMnuIconize.Enabled, 13);
end;

procedure TFWebRadioMain.PTMnuRestoreClick(Sender: TObject);
begin
  iconized:= false;
  visible:= true;
  WindowState:=wsNormal;
 //Need to reload position as it can change during hide in taskbar process
  left:= PrevLeft;
  top:= PrevTop;
  // Tag is black when restore from tray, so we need to reload it
  Application.BringToFront;
  LTAg.Invalidate;
  LTag.Caption:= CurRadioTitle;
end;

procedure TFWebRadioMain.PTMnuIconizeClick(Sender: TObject);
begin
  Application.Minimize;
  Iconized:= HideOnTaskbar;
end;



procedure TFWebRadioMain.VuTimerTimer(Sender: TObject);
var
  level: Int64;
  float_time: double;
begin
  Inc (VuTimercount);
  level:=BASS_ChannelGetLevel(chan);
  SignalMeterL.Value:= LOWORD(level) div 328;   // the left level
  SignalMeterR.Value:= HIWORD(level) div 328;   // the right level
  if VuTimercount mod 20 = 0 then
  try
    float_time:= BASS_ChannelBytes2Seconds(chan, BASS_ChannelGetPosition(chan, BASS_POS_BYTE));
    if Fileduration > 0 then
     begin
       // Playing file
       PBLength.Position:= Trunc(100*float_time/fileduration) ;
       Lposition.Caption:= TimeToStr(float_time/86400)+' / '+TimeToStr(fileduration/86400); //Format('%d / %d s', [trunc(Float_time), Trunc(fileduration)]);
     end else
    begin
      // Playing radio: current position since radio plays or recording radio : recording time
      if not recording then Lposition.Caption:= TimeToStr(float_time/86400)
      else Lposition.Caption:= TimeToStr((now-Recording_beg)) ;
    end;
  except
  end;

end;





procedure TFWebRadioMain.SettingsOnStateChange(Sender: TObject);
begin
  SettingsChanged := True;
end;

procedure TFWebRadioMain.RadiosOnChange(Sender: TObject);
begin
  RadiosChanged := True;
end;

// Procedure used during QueryEndSession and Formclose function
procedure TFWebRadioMain.BeforeClose;
begin
  if FSettings.Settings.Startup then SetAutostart(progname, Application.exename)
      else UnSetAutostart(progname);
  if RadiosChanged then
  begin
    SaveConfig(All);
  end else
  begin
    if SettingsChanged then SaveConfig(Setting) ;
  end;
  Application.ProcessMessages;
end;

procedure TFWebRadioMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (not FSettings.Settings.HideInTaskbar) or CanClose then
  begin
    CloseAction := caFree;
    if FSettings.Settings.Startup then SetAutostart(progname, Application.exename)
    else UnSetAutostart(progname);
    BASS_Free;
    if RadiosChanged or (FSettings.Settings.version='') then SaveConfig(All) else
    if SettingsChanged then  SaveConfig(Setting) ;
    if chan <> 0 then BASS_StreamFree(chan);
    MsgTimer.Enabled:= false;
    VuTimer.Enabled:= false;
    RecordBtnTimer.Enabled:= false;
    // Delete memory font
    {$IFDEF WINDOWS}
       RemoveFontMemResourceEx(DMFontRes );
       PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0) ;
       Application.ProcessMessages;
    {$ENDIF}
    Application.ProcessMessages;

  end else
  begin
    PTMnuIconizeClick(sender);
    CloseAction := caNone;
  end;

end;



procedure TFWebRadioMain.PlayRadio (radio: TRadio);
var
  ThreadId: Cardinal;
  url: String;
  //LastRadio: TRecord;
  MyBmp : TBGRAbitmap;
  l, h: Integer;
  newl, newh : Integer;
  ar: double;
begin
  ThreadId:= 0;
  Isradio:= true;
  url:= radio.url;
  Lposition.Caption:= '';
  fileduration:= 0;
  LTag.Caption:= ' ';
  LStatus.Caption:= sConnectStr;
  // Resize radio logo and fit it in place
  try
    // Load image
    MyBmp := TBGRAbitmap.create(WebRadioAppsData+'images'+PathDelim+radio.favicon);
    // Compute aspect ratio
    l:= MyBmp.Width;
    h:= MyBmp.Height;
    ar:= l/h;
    If ar >= 1 then       // Horizontal or square image
    begin
      newl:= 64;
      newh:= Trunc(64/ar);
    end else
    begin                // vertical image
      newh:= 64;
      newl:= trunc(94*ar);
    end;
    BGRAReplace (MyBmp, MyBmp.resample(newl, newh) );
    ImgLogo.Stretch:= false;
    ImgLogo.Picture.Assign(MyBmp);
    MyBmp.Free;
  except
    ImgLogo.Stretch:= true;
    ImgLogo.Picture.LoadFromLazarusResource('webradio256');
  end;
  //Check if there is a preset

 // if not IsInternet (PingUrl, Application) then
 // begin
 //   SendMessage(win, WM_INFO_UPDATE, WP_Error, 51); // Oops Error
 //   exit;
 // end;
  Application.ProcessMessages;
  if cthread <> 0 then cthread:=0;
  if length(Url) > 0 then
  begin
    // Si la radio est présélectionnée, on la joue, sinon on joue l'url
    //LastRadio:= Radios.FindItem ('url', url);               //ByUrl (Url);
    //if LastRadio.Order > 0 then MnuRadioClick(FindComponent('MnuRadio' + IntToStr(LastRadio.Order)))
    //else
    //begin
    //  try
    //    TColorBtn(FindComponent('BtnRadio' + IntToStr(CurRadio.order))).Color:= clBtnFace;
    //  except
    //  end;
    //  Curradio.order:= 0;
    //  CurRadio.name:='';
    //  CurRadio.Url:= url;

     cthread := BeginThread(nil, 0, TThreadFunc(@OpenURL), PChar(url), 0, ThreadId);
     SBRecord.Enabled:= true;
  end;
end;

//Dernière recherche il y a "days" jours ou plus ?

procedure TFWebRadioMain.CheckUpdate(days: iDays);
var
  errmsg: string;
  sNewVer: string;
  CurVer, NewVer: int64;
  alertpos: TPosition;
  alertmsg: string;
begin
  //Dernière recherche il y a plus de 'days' jours ?
  errmsg := '';
  alertmsg:= '';
  if not visible then alertpos:= poDesktopCenter
  else alertpos:= poMainFormCenter;
  if (Trunc(Now)>Trunc(FSettings.Settings.LastUpdChk)+days) and (not FSettings.Settings.NoChkNewVer) then
  begin
     FSettings.Settings.LastUpdChk := Trunc(Now);
     AboutBox.Checked:= true;
     AboutBox.ErrorMessage:='';
     sNewVer:= AboutBox.ChkNewVersion;
     errmsg:= AboutBox.ErrorMessage;
     // Retry if nothing found the first time
     if (length(sNewVer)=0) and (length(errmsg)=0)then
     begin
       Application.ProcessMessages;
       sNewVer:= AboutBox.ChkNewVersion;
       errmsg:= AboutBox.ErrorMessage;
     end;
     if length(sNewVer)=0 then
     begin
       if length(errmsg)=0 then alertmsg:= sCannotGetNewVerList
       else alertmsg:= TranslateHttpErrorMsg(errmsg, HttpErrMsgNames);
       if AlertDlg(Caption,  alertmsg, [OKBtn, CancelBtn, sNoLongerChkUpdates],
                    true, mtError, alertpos)= mrYesToAll then FSettings.Settings.NoChkNewVer:= true;
       exit;
     end;
     NewVer := VersionToInt(sNewVer);
     // Cannot get new version
     if NewVer < 0 then exit;
     //CurVer := VersionToInt('0.1.0.0');     //Test version check
     CurVer := VersionToInt(version);
     if NewVer > CurVer then
     begin
       FSettings.Settings.LastVersion:= sNewVer;
       AboutBox.LUpdate.Caption := Format(AboutBox.sUpdateAvailable, [sNewVer]);
       AboutBox.NewVersion:= true;
       AboutBox.ShowModal;
     end else
     begin
       AboutBox.LUpdate.Caption:= AboutBox.sNoUpdateAvailable;
     end;
     FSettings.Settings.LastUpdChk:= now;
   end else
   begin
    if VersionToInt(FSettings.Settings.LastVersion)>VersionToInt(version) then
       AboutBox.LUpdate.Caption := Format(AboutBox.sUpdateAvailable, [FSettings.Settings.LastVersion]) else
       AboutBox.LUpdate.Caption:= AboutBox.sNoUpdateAvailable;
       //AboutBox.LUpdate.Hint:= AboutBox.sLastUpdateSearch + ': ' + DateToStr(FSettings.Settings.LastUpdChk);
   end;
   AboutBox.LUpdate.Hint:= AboutBox.sLastUpdateSearch + ': ' + DateToStr(FSettings.Settings.LastUpdChk);
end;

procedure TFWebRadioMain.ModLangue;
begin
  LangStr:=FSettings.Settings.LangStr;
  OSVersion.localize(LangStr, LangFile);
  AboutBox.LVersion.Hint:= OSVersion.VerDetail;
  With LangFile do
    begin
       // Form
      DefaultCaption:= ReadString(LangStr, 'DefaultCaption', Caption);
      OKBtn:= ReadString(LangStr, 'OKBtn','OK');
      YesBtn:=ReadString(LangStr,'YesBtn','Oui');
      NoBtn:=ReadString(LangStr,'NoBtn','Non');
      CancelBtn:=ReadString(LangStr,'CancelBtn','Annuler');
      sStereoCaption:= ReadString(LangStr,'sStereoCaption','Stéréo');
      sMonoCaption:= ReadString(LangStr,'sMonoCaption','Mono');
      LStereo.Caption:= sStereoCaption;
      sRecordingCaption:= ReadString(LangStr,'sRecordingCaption','Enregistrement');
      sStopCaption:= ReadString(LangStr, 'sStopCaption', 'Stoppé');
      sPauseCaption:= ReadString(LangStr, 'sPauseCaption','Pause');
      LLeft.Caption:= ReadString(LangStr, 'LLeft.Caption', LLeft.Caption);
      LRight.Caption:= ReadString(LangStr, 'LRight.Caption', LRight.Caption);
      LLeft.Hint:= ReadString(LangStr, 'LLeft.Hint', LLeft.Hint);
      LRight.Hint:= ReadString(LangStr, 'LRight.Hint', LRight.Hint);
      sMuteHint:= ReadString(LangStr, 'sMuteHint', 'Cliquer pour couper le son');
      sMutedHint:= ReadString(LangStr, 'sMutedHint', 'Cliquer pour rétablir le son');
      SBMute.Hint:= sMuteHint;
      sMuteMenu:= ReadString(LangStr, 'sMuteMenu', 'Couper le son');
      sMutedMenu:= ReadString(LangStr, 'sMutedMenu', 'Rétablir le son');
      sRecordingHint:= ReadString(LangStr, 'sRecordingHint', 'Cliquer pour enregistrer la radio');
      sStopRecordingHint:= ReadString(LangStr, 'sStopRecordingHint', 'Cliquer pour arrêter l''enregistrement');
      SBRecord.Hint:= sRecordingHint;
      sConnectStr:= ReadString(LangStr, 'sConnectStr', 'Connexion...');
      sConnectedStr:= ReadString(LangStr, 'sConnectedStr', 'Connecté');
      sNotConnectedStr:= ReadString(LangStr, 'sNotConnectedStr', 'Non connecté');
      sLoadStr:=  ReadString(LangStr, 'sLoadStr', 'Chargement de');
      sBufferStr:= ReadString(LangStr, 'sBufferStr', 'Buffering...');
      sEnterUrl:= ReadString(LangStr, 'sEnterUrl', sEnterUrl);
      sUse64bit:=ReadString(LangStr,'Use64bit','Utilisez la version 64 bits de ce programme');

      //Menus and buttons
      PMnuOpenRadio.Caption:= ReadString(LangStr, 'PMnuOpenRadio.Caption', PMnuOpenRadio.Caption);;
      PMnuReadFile.Caption:= ReadString(LangStr, 'PMnuReadFile.Caption', PMnuReadFile.Caption);
      PMnuOpenURL.Caption:= ReadString(LangStr, 'PMnuOpenURL.Caption', PMnuOpenURL.Caption);
      PMnuEqualizer.Caption:= ReadString(LangStr, 'PMnuEqualizer.Caption', PMnuEqualizer.Caption);
      PMnuSettings.Caption:= ReadString(LangStr, 'PMnuSettings.Caption', PMnuSettings.Caption);
      PMnuRadList.Caption:= ReadString(LangStr, 'PMnuRadList.Caption', PMnuRadList.Caption);
      PMnuHelp.Caption:= ReadString(LangStr, 'PMnuHelp.Caption', PMnuHelp.Caption);
      PMnuAbout.Caption:= ReadString(LangStr, 'PMnuAbout.Caption', PMnuAbout.Caption);
      PMnuQuit.Caption:= ReadString(LangStr, 'PMnuQuit.Caption', PMnuQuit.Caption);
      PTMnuRestore.Caption:= ReadString(LangStr, 'PTMnuRestore.Caption', PTMnuRestore.Caption);
      PTMnuIconize.Caption:= ReadString(LangStr, 'PTMnuIconize.Caption', PTMnuIconize.Caption);
      PTMnuAbout.Caption:= PMnuAbout.Caption;
      if Muted then PTMnuMute.Caption:= sMutedMenu else PTMnuMute.Caption:= sMuteMenu;

      SBOpenRadio.hint:= PMnuOpenRadio.Caption;
      SBReadFile.Hint:= PMnuReadFile.Caption;
      SBOpenUrl.Hint:= PMnuOpenURL.Caption;
      SBEqualizer.Hint:= PMnuEqualizer.Caption;
      SBSEtings.Hint:= PMnuSettings.Caption;
      SBRadList.Hint:= PMnuRadList.Caption;
      SBAbout.Hint:= PMnuAbout.Caption;
      SBQuit.Hint:= PMnuQuit.Caption;

      // About
      Aboutbox.Caption:= Format(ReadString(LangStr,'Aboutbox.Caption','A propos du %s'), [DefaultCaption]);
      AboutBox.sLastUpdateSearch:=ReadString(LangStr,'AboutBox.LastUpdateSearch','Dernière recherche de mise à jour');
      AboutBox.sUpdateAvailable:=ReadString(LangStr,'AboutBox.UpdateAvailable','Nouvelle version %s disponible');
      AboutBox.sNoUpdateAvailable:=ReadString(LangStr,'AboutBox.NoUpdateAvailable','Le Tuner radio Web est à jour');

      AboutBox.LProductName.Caption:= DefaultCaption;
      AboutBox.LProgPage.Caption:= ReadString(LangStr,'AboutBox.LProgPage.Caption', AboutBox.LProgPage.Caption);
      //AboutBox.UrlProgSite:= ReadString(LangStr,'AboutBox.UrlProgSite','https://github.com/bb84000/mailsinbox/wiki/Accueil');
      AboutBox.LWebSite.Caption:= ReadString(LangStr,'AboutBox.LWebSite.Caption', AboutBox.LWebSite.Caption);
      AboutBox.LSourceCode.Caption:= ReadString(LangStr,'AboutBox.LSourceCode.Caption', AboutBox.LSourceCode.Caption);
      if not AboutBox.checked then AboutBox.LUpdate.Caption:=ReadString(LangStr,'AboutBox.LUpdate.Caption',AboutBox.LUpdate.Caption) else
      begin
        if AboutBox.NewVersion then AboutBox.LUpdate.Caption:= Format(AboutBox.sUpdateAvailable, [AboutBox.LastVersion])
        else AboutBox.LUpdate.Caption:= AboutBox.sNoUpdateAvailable;
      end;
      HelpFile:= WRExecPath+'help'+PathDelim+ReadString(LangStr,'HelpFile', 'webradio.html');

      // Alert
      sUpdateAlertBox:=ReadString(LangStr,'sUpdateAlertBox','Version actuelle: %sUne nouvelle version %s est disponible. Cliquer pour la télécharger');
      sNoLongerChkUpdates:=ReadString(LangStr,'sNoLongerChkUpdates','Ne plus rechercher les mises à jour');

      // Settings form
      FSettings.Caption:= ReadString(LangStr, 'FSettings.Caption', FSettings.Caption);
      FSettings.BtnCancel.Caption:= CancelBtn;
      FSettings.BtnOK.Caption:= OKBtn;
      FSettings.TPSystem.Caption:= ReadString(LangStr, 'FSettings.TPSystem.Caption', FSettings.TPSystem.Caption);
      FSettings.TPEncode.Caption:= ReadString(LangStr, 'FSettings.TPEncode.Caption', FSettings.TPEncode.Caption);
      FSettings.CBSavSizePos.Caption:= ReadString(LangStr, 'FSettings.CBSavSizePos.Caption', FSettings.CBSavSizePos.Caption);
      FSettings.CBStartup.Caption:= ReadString(LangStr, 'FSettings.CBStartup.Caption', FSettings.CBStartup.Caption);
      FSettings.CBStartMini.Caption:= ReadString(LangStr, 'FSettings.CBStartMini.Caption', FSettings.CBStartMini.Caption);
      FSettings.CBNoChkNewVer.Caption:= ReadString(LangStr, 'FSettings.CBNoChkNewVer.Caption', FSettings.CBNoChkNewVer.Caption);
      FSettings.LBitrate.Caption:= ReadString(LangStr, 'FSettings.LBitrate.Caption', FSettings.LBitrate.Caption);
      FSettings.LSampling.Caption:= ReadString(LangStr, 'FSettings.LSampling.Caption', FSettings.LSampling.Caption);
      FSettings.LLangue.Caption:= ReadString(LangStr, 'FSettings.LLangue.Caption', FSettings.LLangue.Caption);
      FSettings.LDataFolder.Caption:= ReadString(LangStr, 'FSettings.LDataFolder.Caption', FSettings.LDataFolder.Caption);
      FSettings.LFont.Caption:= ReadString(LangStr, 'FSettings.LFont.Caption', FSettings.LFont.Caption);
      FSettings.CBFonts.Hint:= ReadString(LangStr, 'FSettings.CBFonts.Hint',FSettings.CBFonts.Hint);
      FSettings.TPColors.Caption:= ReadString(LangStr, 'FSettings.TPColors.Caption', FSettings.TPColors.Caption);
      FSettings.CPDisplayText.Caption:= ReadString(LangStr, 'FSettings.CPDisplayText.Caption', FSettings.CPDisplayText.Caption);
      FSettings.CPDisplayBack.Caption:= ReadString(LangStr, 'FSettings.CPDisplayBack.Caption', FSettings.CPDisplayBack.Caption);
      FSettings.sMnuCopy:= ReadString(LangStr, 'FSettings.sMnuCopy', 'Copier');
      FSettings.sMnuPaste:= ReadString(LangStr, 'FSettings.sMnuPaste', 'Coller');

      // Radios form
      FRadios.Caption:= ReadString(LangStr, 'FRadios.Caption', FRadios.Caption);
      FRadios.OKBtn:= OKBtn;
      FRadios.YesBtn:= YesBtn;
      FRadios.NoBtn:= NoBtn;
      FRadios.CancelBtn:= CancelBtn;
      FRadios.OPDLogo.Title:= ReadString(LangStr, 'FRadios.OPDLogo.Title', 'Ouvrir une image');
      FRadios.sConfirmDeleteRadio:= ReadString(LangStr,'FRadios.sConfirmDeleteRadio', 'Voulez-vous vraiment supprimer la radio %s ?');
      FRadios.SBAddRadio.Hint:= ReadString(LangStr,'FRadios.SBAddRadio.Hint', FRadios.SBAddRadio.Hint);
      FRadios.SBEditRadio.Hint:= ReadString(LangStr,'FRadios.SBEditRadio.Hint', FRadios.SBEditRadio.Hint);
      FRadios.SBValidRadio.Hint:= ReadString(LangStr,'FRadios.SBValidRadio.Hint', FRadios.SBValidRadio.Hint);
      FRadios.SBCancelChanges.Hint:= ReadString(LangStr,'FRadios.SBCancelChanges.Hint', FRadios.SBCancelChanges.Hint);
      FRadios.SBDeleteRadio.Hint:= ReadString(LangStr,'FRadios.SBDeleteRadio.Hint', FRadios.SBDeleteRadio.Hint);
      FRadios.Lname.Caption:= ReadString(LangStr,'FRadios.Lname.Caption', FRadios.Lname.Caption);
      FRadios.LUrl.Caption:= ReadString(LangStr,'FRadios.LUrl.Caption', FRadios.LUrl.Caption);
      FRadios.LComment.Caption:= ReadString(LangStr,'FRadios.LComment.Caption', FRadios.LComment.Caption);
      FRadios.LFavicon.Caption:= ReadString(LangStr,'FRadios.LFavicon.Caption', FRadios.LFavicon.Caption);
      FRadios.LPresets.Caption:= ReadString(LangStr,'FRadios.LPresets.Caption', FRadios.LPresets.Caption);

      FEqualizer.Caption:= ReadString(LangStr,'FEqualizer.Caption', FEqualizer.Caption);
      FEqualizer.CBEqualize.Caption:= ReadString(LangStr,'FEqualizer.CBEqualize.Caption', FEqualizer.CBEqualize.Caption);
      FEqualizer.LReset.Caption:= ReadString(LangStr,'FEqualizer.LReset.Caption', FEqualizer.LReset.Caption);
      FEqualizer.BtnCancel.Caption:= CancelBtn;
      FEqualizer.BtnOK.Caption:= OKBtn;

            // BASS Errors
      ErrBassVersion:= ReadString(LangStr, 'ErrBassVersion','Mauvaise version de BASS.DLL');

      BassErrArr[BASS_OK]:= 'OK';         //0
      BassErrArr[BASS_ERROR_MEM]:= ReadString(LangStr, 'BASS_ERROR_MEM', 'Erreur mémoire');
      BassErrArr[BASS_ERROR_FILEOPEN]:= ReadString(LangStr, 'BASS_ERROR_FILEOPEN', 'Erreur d''ouverture du fichier ou du flux');
      BassErrArr[BASS_ERROR_DRIVER]:= ReadString(LangStr, 'BASS_ERROR_DRIVER', 'Erreur de driver');
      BassErrArr[BASS_ERROR_BUFLOST]:= ReadString(LangStr, 'BASS_ERROR_BUFLOST', 'Erreur de mémoire tampon');
      BassErrArr[BASS_ERROR_HANDLE]:= ReadString(LangStr, 'BASS_ERROR_HANDLE', 'Erreur de handle');
      BassErrArr[BASS_ERROR_FORMAT]:= ReadString(LangStr, 'BASS_ERROR_FORMAT', 'Erreur de format d''échantillon');
      BassErrArr[BASS_ERROR_POSITION ]:= ReadString(LangStr, 'BASS_ERROR_POSITION ', 'Erreur de position');
      BassErrArr[BASS_ERROR_INIT]:= ReadString(LangStr, 'BASS_ERROR_INIT', 'Erreur d''initialisation de la bibiliothèque Bass');
      BassErrArr[BASS_ERROR_START]:= ReadString(LangStr, 'BASS_ERROR_START', 'Erreur de démarrage');
      BassErrArr[BASS_ERROR_SSL]:= ReadString(LangStr, 'BASS_ERROR_SSL', 'SSL non installé');
      BassErrArr[BASS_ERROR_ALREADY]:= ReadString(LangStr, 'BASS_ERROR_ALREADY', 'Fonction déjà en cours d''utilisation');
      BassErrArr[BASS_ERROR_NOTAUDIO]:= ReadString(LangStr, 'BASS_ERROR_NOTAUDIO', 'Le fichier ne contient pas d''audio'); //The file does not contain audio,
      BassErrArr[BASS_ERROR_NOCHAN]:= ReadString(LangStr, 'BASS_ERROR_NOCHAN', 'L''échantillon n''a pas de canal libre');  //The sample has no free channels
      BassErrArr[BASS_ERROR_ILLTYPE]:= ReadString(LangStr, 'BASS_ERROR_ILLTYPE', 'L''élément n''est pas valide');
      BassErrArr[BASS_ERROR_ILLPARAM]:= ReadString(LangStr, 'BASS_ERROR_ILLPARAM', 'Le paramètre n''est pas valide');
      BassErrArr[BASS_ERROR_NO3D]:= ReadString(LangStr, 'BASS_ERROR_NO3D', 'Fonctionenment 3D non suppporté'); //The channel does not have 3D functionality
      BassErrArr[BASS_ERROR_NOEAX]:= ReadString(LangStr, 'BASS_ERROR_NOEAX', 'Fonctionnement EAX non supporté');
      BassErrArr[BASS_ERROR_DEVICE]:= ReadString(LangStr, 'BASS_ERROR_DEVICE', 'Périphérique invalide');
      BassErrArr[BASS_ERROR_NOPLAY]:= ReadString(LangStr, 'BASS_ERROR_NOPLAY', 'Le canal n''est pas en cours de lecture'); //The channel is not playing
      BassErrArr[BASS_ERROR_FREQ]:= ReadString(LangStr, 'BASS_ERROR_FREQ', 'Erreur de fréquence');
      BassErrArr[BASS_ERROR_NOTFILE]:= ReadString(LangStr, 'BASS_ERROR_NOTFILE', 'Le fichier ne contient pas de flux');
      BassErrArr[BASS_ERROR_NOHW]:= ReadString(LangStr, 'BASS_ERROR_NOHW', 'Matériel non trouvé');
      BassErrArr[BASS_ERROR_EMPTY]:= ReadString(LangStr, 'BASS_ERROR_EMPTY', 'Le fichier ne contient pas de données d''échantillonnage');
      BassErrArr[BASS_ERROR_NONET]:= ReadString(LangStr, 'BASS_ERROR_NONET', 'Pas de connexion Internet');
      BassErrArr[BASS_ERROR_CREATE]:= ReadString(LangStr, 'BASS_ERROR_CREATE', 'Le fichier audio n''a pas pu être créé'); //The PCM file could not be created.
      BassErrArr[BASS_ERROR_NOFX]:= ReadString(LangStr, 'BASS_ERROR_NOFX', 'L''effet demandé est indisponible'); //The specified DX8 effect is unavailable
      BassErrArr[BASS_ERROR_NOTAVAIL]:= ReadString(LangStr, 'BASS_ERROR_NOTAVAIL', 'Elélent non disponible');
      BassErrArr[BASS_ERROR_DECODE]:= ReadString(LangStr, 'BASS_ERROR_DECODE', 'Le canal de décodage ne peut pas être lu'); //The channel is not playable; it is a "decoding channel".
      BassErrArr[BASS_ERROR_DX]:= ReadString(LangStr, 'BASS_ERROR_DX', 'Mauvaise version de DX)');
      BassErrArr[BASS_ERROR_TIMEOUT]:= ReadString(LangStr, 'BASS_ERROR_TIMEOUT', 'Délai maximum dépassé');
      BassErrArr[BASS_ERROR_FILEFORM]:= ReadString(LangStr, 'BASS_ERROR_FILEFORM', 'Format de fichier invalide');
      BassErrArr[BASS_ERROR_SPEAKER]:= ReadString(LangStr, 'BASS_ERROR_SPEAKER', 'Les paramètres de haut parleur sont invalides');  //  The specified SPEAKER flags are invalid
      BassErrArr[BASS_ERROR_VERSION]:= ReadString(LangStr, 'BASS_ERROR_VERSION', 'Le plugin nécessite une version différente de BASS'); //The plugin requires a different BASS version
      BassErrArr[BASS_ERROR_CODEC]:= ReadString(LangStr, 'BASS_ERROR_CODEC', 'Codec non disponible');
      BassErrArr[BASS_ERROR_ENDED]:= ReadString(LangStr, 'BASS_ERROR_ENDED', 'Le canal a atteint la fin');//   The channel has reached the end
      BassErrArr[BASS_ERROR_BUSY]:= ReadString(LangStr, 'BASS_ERROR_BUSY', 'L''élément est utilisé'); // The deviece is busy
      BassErrArr[BASS_ERROR_UNSTREAMABLE]:= ReadString(LangStr, 'BASS_ERROR_UNSTREAMABLE', 'Impossible de créer un flux');
      BassErrArr[length(BassErrArr)-1]:= ReadString(LangStr, 'BASS_ERROR_UNKNOWN', 'Erreur inconnue');

      ErrNoBassPLAY:=  ReadString(LangStr, 'ErrNoBassPLAY','La bibliothèque Bass%s n''est pas installée.%sLecture des flux %s impossible.');
      ErrNoBassEnc:=  ReadString(LangStr, 'ErrNoBassEnc','La bibliothèque BassEnc%s n''est pas installée.%sEnregistrement impossible au format %s.');
      ErrBassEncVer:= ReadString(LangStr, 'ErrNoBassEncVer','La version de la bibliothèque BassEnc est erronnée.%sEnregistrement impossible.');
      ErrUnsupportedEnc:= ReadString(LangStr, 'ErrUnsupportedEnc', 'Impossible d''enregistrer, encodage %s non supporté');
      ErrEncoding:= ReadString(LangStr, 'ErrEncoding', 'Erreur d''encodage %d');
      ErrConnection:= ReadString(LangStr, 'ErrConnection', 'Erreur de connexion');

    // HTTP Error messages
    HttpErrMsgNames[0] := ReadString(LangStr,'SErrInvalidProtocol','Protocole "%s" invalide');
    HttpErrMsgNames[1] := ReadString(LangStr,'SErrReadingSocket','Erreur de lecture des données à partir du socket');
    HttpErrMsgNames[2] := ReadString(LangStr,'SErrInvalidProtocolVersion','Version de protocole invalide en réponse: %s');
    HttpErrMsgNames[3] := ReadString(LangStr,'SErrInvalidStatusCode','Code de statut de réponse invalide: %s');
    HttpErrMsgNames[4] := ReadString(LangStr,'SErrUnexpectedResponse','Code de statut de réponse non prévu: %s');
    HttpErrMsgNames[5] := ReadString(LangStr,'SErrChunkTooBig','Bloc trop grand');
    HttpErrMsgNames[6] := ReadString(LangStr,'SErrChunkLineEndMissing','Fin de ligne du bloc manquante');
    HttpErrMsgNames[7] := ReadString(LangStr,'SErrMaxRedirectsReached','Nombre maximum de redirections atteint: %s');
    // Socket error messages
    HttpErrMsgNames[8] := ReadString(LangStr,'strHostNotFound','Résolution du nom d''hôte pour "%s" impossible.');
    HttpErrMsgNames[9] := ReadString(LangStr,'strSocketCreationFailed','Echec de la création du socket: %s');
    HttpErrMsgNames[10] := ReadString(LangStr,'strSocketBindFailed','Echec de liaison du socket: %s');
    HttpErrMsgNames[11] := ReadString(LangStr,'strSocketListenFailed','Echec de l''écoute sur le port n° %s, erreur %s');
    HttpErrMsgNames[12]:=ReadString(LangStr,'strSocketConnectFailed','Echec de la connexion à %s');
    HttpErrMsgNames[13]:=ReadString(LangStr,'strSocketAcceptFailed','Connexion refusée d''un client sur le socket: %s, erreur %s');
    HttpErrMsgNames[14]:=ReadString(LangStr,'strSocketAcceptWouldBlock','La connexion pourrait bloquer le socket: %s');
    HttpErrMsgNames[15]:=ReadString(LangStr,'strSocketIOTimeOut','Impossible de fixer le timeout E/S à %s');
    HttpErrMsgNames[16]:=ReadString(LangStr,'strErrNoStream','Flux du socket non assigné');
    end;
end;

initialization
{$I webradio.lrs}

end.

