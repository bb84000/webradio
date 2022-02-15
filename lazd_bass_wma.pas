{
  lazd_bass_wma 2.4 Lazarus unit
  Copyright (c) 2003-2020 Un4seen Developments Ltd.

  See the BASSWMA.CHM file for more detailed documentation
  
  Call Load_BASSWMADLL to load BASSENC before using any functions, and
  Unload_BASSWMADLL to unload 
  Important !!! Always use () after a function or procedure without parameter

  Lazarus port - bb - sdtp - february 2022
}

unit lazd_bass_wma;

interface

{$mode objfpc}{$H+}

uses lazd_bass, Windows;

const
  // Additional error codes returned by BASS_ErrorGetCode
  BASS_ERROR_WMA_LICENSE     = 1000; // the file is protected
  BASS_ERROR_WMA             = 1001; // Windows Media (9 or above) is not installed
  BASS_ERROR_WMA_WM9         = BASS_ERROR_WMA;
  BASS_ERROR_WMA_DENIED      = 1002; // access denied (user/pass is invalid)
  BASS_ERROR_WMA_INDIVIDUAL  = 1004; // individualization is needed
  BASS_ERROR_WMA_PUBINIT     = 1005; // publishing point initialization problem

  // Additional BASS_SetConfig options
  BASS_CONFIG_WMA_PRECHECK   = $10100;
  BASS_CONFIG_WMA_BASSFILE   = $10103;
  BASS_CONFIG_WMA_NETSEEK    = $10104;
  BASS_CONFIG_WMA_VIDEO      = $10105;
  BASS_CONFIG_WMA_BUFTIME    = $10106;

  // additional WMA sync types
  BASS_SYNC_WMA_CHANGE       = $10100;
  BASS_SYNC_WMA_META         = $10101;

  // additional BASS_StreamGetFilePosition WMA mode
  BASS_FILEPOS_WMA_BUFFER    = 1000; // internet buffering progress (0-100%)

  // Additional flags for use with BASS_WMA_EncodeOpen/File/Network/Publish
  BASS_WMA_ENCODE_STANDARD   = $2000;  // standard WMA
  BASS_WMA_ENCODE_PRO        = $4000;  // WMA Pro
  BASS_WMA_ENCODE_24BIT      = $8000;  // 24-bit
  BASS_WMA_ENCODE_PCM        = $10000; // uncompressed PCM
  BASS_WMA_ENCODE_SCRIPT     = $20000; // set script (mid-stream tags) in the WMA encoding
  BASS_WMA_ENCODE_QUEUE      = $40000; // queue data to feed encoder asynchronously
  BASS_WMA_ENCODE_SOURCE     = $80000; // use a BASS channel as source
  BASS_WMA_ENCODE_VOICE      = $100000; // WMA Voice
  BASS_WMA_ENCODE_VOICE_MIXED = $300000; // WMA Voice mixed mode

  // Additional flag for use with BASS_WMA_EncodeGetRates
  BASS_WMA_ENCODE_RATES_VBR  = $10000; // get available VBR quality settings

  // WMENCODEPROC "type" values
  BASS_WMA_ENCODE_HEAD       = 0;
  BASS_WMA_ENCODE_DATA       = 1;
  BASS_WMA_ENCODE_DONE       = 2;

  // BASS_WMA_EncodeSetTag "form" values
  BASS_WMA_TAG_ANSI          = 0;
  BASS_WMA_TAG_UNICODE       = 1;
  BASS_WMA_TAG_UTF8          = 2;
  BASS_WMA_TAG_BINARY        = $100; // FLAG: binary tag (HIWORD=length)

  // BASS_CHANNELINFO type
  BASS_CTYPE_STREAM_WMA      = $10300;
  BASS_CTYPE_STREAM_WMA_MP3  = $10301;

  // Additional BASS_ChannelGetTags type
  BASS_TAG_WMA               = 8; // WMA header tags : series of null-terminated UTF-8 strings
  BASS_TAG_WMA_META          = 11; // WMA mid-stream tag : UTF-8 string
  BASS_TAG_WMA_CODEC         = 12; // WMA codec


type
  HWMENCODE = DWORD;		// WMA encoding handle

  CLIENTCONNECTPROC = procedure(handle:HWMENCODE; connect:BOOL; ip:PAnsiChar; user:Pointer); stdcall;
  {
    Client connection notification callback function.
    handle : The encoder
    connect: TRUE=client is connecting, FALSE=disconnecting
    ip     : The client's IP (xxx.xxx.xxx.xxx:port)
    user   : The 'user' parameter value given when calling BASS_WMA_EncodeSetNotify
  }

  WMENCODEPROC = procedure(handle:HWMENCODE; dtype:DWORD; buffer:Pointer; length:DWORD; user:Pointer); stdcall;
  {
    Encoder callback function.
    handle : The encoder handle
    dtype  : The type of data, one of BASS_WMA_ENCODE_xxx values
    buffer : The encoded data
    length : Length of the data
    user   : The 'user' parameter value given when calling BASS_WMA_EncodeOpen
  }


const
  basswmadll = 'basswma.dll';

var
  BASS_WMA_Handle:Thandle = 0;

  BASS_WMA_StreamCreateFile:function(mem:BOOL; fl:pointer; offset,length:QWORD; flags:DWORD): HSTREAM; stdcall; 
  BASS_WMA_StreamCreateFileAuth:function(mem:BOOL; fl:pointer; offset,length:QWORD; flags:DWORD; user,pass:PChar): HSTREAM; stdcall; 
  BASS_WMA_StreamCreateFileUser:function(system,flags:DWORD; var procs:BASS_FILEPROCS; user:Pointer): HSTREAM; stdcall; 

  BASS_WMA_GetTags:function(fname:PChar; flags:DWORD): PAnsiChar; stdcall; 

  BASS_WMA_EncodeGetRates:function(freq,chans,flags:DWORD): PDWORD; stdcall; 
  BASS_WMA_EncodeOpen:function(freq,chans,flags,bitrate:DWORD; proc:WMENCODEPROC; user:Pointer): HWMENCODE; stdcall; 
  BASS_WMA_EncodeOpenFile:function(freq,chans,flags,bitrate:DWORD; fname:PChar): HWMENCODE; stdcall; 
  BASS_WMA_EncodeOpenNetwork:function(freq,chans,flags,bitrate,port,clients:DWORD): HWMENCODE; stdcall; 
  BASS_WMA_EncodeOpenNetworkMulti:function(freq,chans,flags:DWORD; bitrates:PDWORD; port,clients:DWORD): HWMENCODE; stdcall; 
  BASS_WMA_EncodeOpenPublish:function(freq,chans,flags,bitrate:DWORD; url,user,pass:PChar): HWMENCODE; stdcall; 
  BASS_WMA_EncodeOpenPublishMulti:function(freq,chans,flags:DWORD; bitrates:PDWORD; url,user,pass:PChar): HWMENCODE; stdcall; 
  BASS_WMA_EncodeGetPort:function(handle:HWMENCODE): DWORD; stdcall;
  BASS_WMA_EncodeSetNotify:function(handle:HWMENCODE; proc:CLIENTCONNECTPROC; user:Pointer): BOOL; stdcall; 
  BASS_WMA_EncodeGetClients:function(handle:HWMENCODE): DWORD; stdcall;
  BASS_WMA_EncodeSetTag:function(handle:HWMENCODE; tag,text:PChar; form:DWORD): BOOL; stdcall; 
  BASS_WMA_EncodeWrite:function(handle:HWMENCODE; buffer:Pointer; length:DWORD): BOOL; stdcall; 
  BASS_WMA_EncodeClose:function(handle:HWMENCODE): BOOL; stdcall; 

function Load_BASSWMADLL(const dllfilename : String) :boolean;
procedure Unload_BASSWMADLL;


implementation

Function Load_BASSWMADLL (const dllfilename : String) :boolean;
begin
  result:= false;
  if BASS_WMA_Handle <> 0 then Result:= true else
  begin
    {$IFDEF UNICODE}
      BASS_WMA_Handle:= LoadLibraryW(PWideChar(dllfilename));
    {$ELSE}
      BASS_WMA_Handle:= LoadLibrary(PChar(dllfilename));
    {$ENDIF}
    if BASS_WMA_Handle <> 0 then
    begin
      pointer(BASS_WMA_StreamCreateFile):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_StreamCreateFile'));
      pointer(BASS_WMA_StreamCreateFileAuth):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_StreamCreateFileAuth'));
      pointer(BASS_WMA_StreamCreateFileUser):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_StreamCreateFileUser'));
	  
	  pointer(BASS_WMA_GetTags):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_GetTags'));
	  
	  pointer(BASS_WMA_EncodeGetRates):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeGetRates'));
	  pointer(BASS_WMA_EncodeOpen):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeOpen'));
	  pointer(BASS_WMA_EncodeOpenFile):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeOpenFile'));
	  pointer(BASS_WMA_EncodeOpenNetwork):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeOpenNetwork'));
	  pointer(BASS_WMA_EncodeOpenNetworkMulti):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeOpenNetworkMulti'));
	  pointer(BASS_WMA_EncodeOpenPublish):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeOpenPublish'));
	  pointer(BASS_WMA_EncodeOpenPublishMulti):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeOpenPublishMulti'));
	  pointer(BASS_WMA_EncodeGetPort):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeGetPort'));
	  pointer(BASS_WMA_EncodeSetNotify):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeSetNotify'));
	  pointer(BASS_WMA_EncodeGetClients):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeGetClients'));
	  pointer(BASS_WMA_EncodeSetTag):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeSetTag'));
	  pointer(BASS_WMA_EncodeWrite):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeWrite'));
	  pointer(BASS_WMA_EncodeClose):=GetProcAddress(BASS_WMA_Handle, PAnsiChar('BASS_WMA_EncodeClose'));
    end;
    // check if all functions are OK
    if (pointer(BASS_WMA_StreamCreateFile)=nil) or (pointer(BASS_WMA_StreamCreateFileAuth)=nil) or
       (pointer(BASS_WMA_StreamCreateFileUser)=nil) or (pointer(BASS_WMA_GetTags)=nil) or
       (pointer(BASS_WMA_EncodeGetRates)=nil) or (pointer(BASS_WMA_EncodeOpen)=nil) or
       (pointer(BASS_WMA_EncodeOpenFile)=nil) or (pointer(BASS_WMA_EncodeOpenNetwork)= nil) or
       (pointer(BASS_WMA_EncodeOpenNetworkMulti)=nil) or (pointer(BASS_WMA_EncodeOpenPublish)= nil) or
       (pointer(BASS_WMA_EncodeOpenPublishMulti)=nil) or (pointer(BASS_WMA_EncodeGetPort)= nil) or
       (pointer(BASS_WMA_EncodeSetNotify)=nil) or (pointer(BASS_WMA_EncodeGetClients)= nil) or
       (pointer(BASS_WMA_EncodeSetTag)=nil) or (pointer(BASS_WMA_EncodeWrite)= nil) or
       (pointer(BASS_WMA_EncodeClose)= nil) then
    begin
      FreeLibrary(BASS_WMA_Handle);
    Result := false
    end else Result := (BASS_WMA_Handle) <> 0;
  end;
end;

Procedure Unload_BASSWMADLL;
begin
 if BASS_WMA_Handle <> 0 then
  begin
   FreeLibrary(BASS_WMA_Handle);
  end;
 BASS_WMA_Handle:=0;
end;

end.
