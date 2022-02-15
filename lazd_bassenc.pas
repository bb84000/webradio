{
  BASSenc 2.4 Delphi unit
  Copyright (c) 2003-2020 Un4seen Developments Ltd.

  See the BASSENC.CHM file for more detailed documentation
  
  Call Load_BASSENCDLL to load BASSENC before using any functions, and
  Unload_BASSENCDLL to unload 
  Important !!! Always use () after a function or procedure without parameter
  
  Lazarus port - bb - sdtp - february 2022
}

Unit lazd_bassenc;

interface

{$mode objfpc}{$H+}

uses lazd_bass, dynlibs
{$IFDEF WINDOWS}, Windows{$ENDIF} ;

const
  // Additional error codes returned by BASS_ErrorGetCode
  BASS_ERROR_ACM_CANCEL         = 2000; // ACM codec selection cancelled
  BASS_ERROR_CAST_DENIED        = 2100; // access denied (invalid password)
  BASS_ERROR_SERVER_CERT        = 2101; // missing/invalid certificate

  // Additional BASS_SetConfig options
  BASS_CONFIG_ENCODE_PRIORITY   = $10300;
  BASS_CONFIG_ENCODE_QUEUE      = $10301;
  BASS_CONFIG_ENCODE_CAST_TIMEOUT = $10310;

  // Additional BASS_SetConfigPtr options
  BASS_CONFIG_ENCODE_ACM_LOAD   = $10302;
  BASS_CONFIG_ENCODE_CAST_PROXY = $10311;
  BASS_CONFIG_ENCODE_CAST_BIND  = $10312;
  BASS_CONFIG_ENCODE_SERVER_CERT = $10320;
  BASS_CONFIG_ENCODE_SERVER_KEY = $10321;

  // BASS_Encode_Start flags
  BASS_ENCODE_NOHEAD            = 1;	// don't send a WAV header to the encoder
  BASS_ENCODE_FP_8BIT           = 2;	// convert floating-point sample data to 8-bit integer
  BASS_ENCODE_FP_16BIT          = 4;	// convert floating-point sample data to 16-bit integer
  BASS_ENCODE_FP_24BIT          = 6;	// convert floating-point sample data to 24-bit integer
  BASS_ENCODE_FP_32BIT          = 8;	// convert floating-point sample data to 32-bit integer
  BASS_ENCODE_FP_AUTO           = 14;	// convert floating-point sample data back to channel's format
  BASS_ENCODE_BIGEND            = 16;	// big-endian sample data
  BASS_ENCODE_PAUSE             = 32;	// start encording paused
  BASS_ENCODE_PCM               = 64;	// write PCM sample data (no encoder)
  BASS_ENCODE_RF64              = 128;	// send an RF64 header
  BASS_ENCODE_MONO              = $100; // convert to mono (if not already)
  BASS_ENCODE_QUEUE             = $200; // queue data to feed encoder asynchronously
  BASS_ENCODE_WFEXT             = $400; // WAVEFORMATEXTENSIBLE "fmt" chunk
  BASS_ENCODE_CAST_NOLIMIT      = $1000; // don't limit casting data rate
  BASS_ENCODE_LIMIT             = $2000; // limit data rate to real-time
  BASS_ENCODE_AIFF              = $4000; // send an AIFF header rather than WAV
  BASS_ENCODE_DITHER            = $8000; // apply dither when converting floating-point sample data to integer
  BASS_ENCODE_AUTOFREE          = $40000; // free the encoder when the channel is freed

  // BASS_Encode_GetACMFormat flags
  BASS_ACM_DEFAULT              = 1; // use the format as default selection
  BASS_ACM_RATE                 = 2; // only list formats with same sample rate as the source channel
  BASS_ACM_CHANS                = 4; // only list formats with same number of channels (eg. mono/stereo)
  BASS_ACM_SUGGEST              = 8; // suggest a format (HIWORD=format tag)

  // BASS_Encode_GetCount counts
  BASS_ENCODE_COUNT_IN          = 0; // sent to encoder
  BASS_ENCODE_COUNT_OUT         = 1; // received from encoder
  BASS_ENCODE_COUNT_CAST        = 2; // sent to cast server
  BASS_ENCODE_COUNT_QUEUE       = 3; // queued
  BASS_ENCODE_COUNT_QUEUE_LIMIT = 4; // queue limit
  BASS_ENCODE_COUNT_QUEUE_FAIL  = 5; // failed to queue
  BASS_ENCODE_COUNT_IN_FP       = 6; // sent to encoder before floating-point conversion

  // BASS_Encode_CastInit content MIME types
  BASS_ENCODE_TYPE_MP3          = 'audio/mpeg';
  BASS_ENCODE_TYPE_OGG          = 'application/ogg';
  BASS_ENCODE_TYPE_AAC          = 'audio/aacp';

  // BASS_Encode_CastGetStats types
  BASS_ENCODE_STATS_SHOUT       = 0; // Shoutcast stats
  BASS_ENCODE_STATS_ICE         = 1; // Icecast mount-point stats
  BASS_ENCODE_STATS_ICESERV     = 2; // Icecast server stats

  // Encoder notifications
  BASS_ENCODE_NOTIFY_ENCODER    = 1; // encoder died
  BASS_ENCODE_NOTIFY_CAST       = 2; // cast server connection died
  BASS_ENCODE_NOTIFY_SERVER     = 3; // server died
  BASS_ENCODE_NOTIFY_CAST_TIMEOUT = $10000; // cast timeout
  BASS_ENCODE_NOTIFY_QUEUE_FULL = $10001; // queue is out of space
  BASS_ENCODE_NOTIFY_FREE       = $10002; // encoder has been freed

  // BASS_Encode_ServerInit flags
  BASS_ENCODE_SERVER_NOHTTP     = 1; // no HTTP headers
  BASS_ENCODE_SERVER_META       = 2; // Shoutcast metadata
  BASS_ENCODE_SERVER_SSL        = 4;

type
  HENCODE = DWORD;   // encoder handle

  ENCODEPROC = procedure(handle:HENCODE; channel:DWORD; buffer:Pointer; length:DWORD; user:Pointer); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Encoding callback function.
    handle : The encoder
    channel: The channel handle
    buffer : Buffer containing the encoded data
    length : Number of bytes
    user   : The 'user' parameter value given when calling BASS_EncodeStart
  }

  ENCODEPROCEX = procedure(handle:HENCODE; channel:DWORD; buffer:Pointer; length:DWORD; offset:QWORD; user:Pointer); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Encoding callback function with offset info.
    handle : The encoder
    channel: The channel handle
    buffer : Buffer containing the encoded data
    length : Number of bytes
    offset : File offset of the data
    user   : The 'user' parameter value given when calling BASS_Encode_StartCA
  }

  ENCODERPROC = function(handle:HENCODE; channel:DWORD; buffer:Pointer; length:DWORD; maxout:DWORD; user:Pointer): DWORD; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Encoder callback function.
    handle : The encoder
    channel: The channel handle
    buffer : Buffer containing the PCM data (input) and receiving the encoded data (output)
    length : Number of bytes in (-1=closing)
    maxout : Maximum number of bytes out
    user   : The 'user' parameter value given when calling BASS_Encode_StartUser
    RETURN : The amount of encoded data (-1=stop)
  }

  ENCODECLIENTPROC = function(handle:HENCODE; connect:BOOL; client:PAnsiChar; headers:PAnsiChar; user:Pointer): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Client connection notification callback function.
    handle : The encoder
    connect: TRUE/FALSE=client is connecting/disconnecting
    client : The client's address (xxx.xxx.xxx.xxx:port)
    headers: Request headers (optionally response headers on return)
    user   : The 'user' parameter value given when calling BASS_Encode_ServerInit
    RETURN : TRUE/FALSE=accept/reject connection (ignored if connect=FALSE)
  }

  ENCODENOTIFYPROC = procedure(handle:HENCODE; status:DWORD; user:Pointer); {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  {
    Encoder death notification callback function.
    handle : The encoder
    status : Notification (BASS_ENCODE_NOTIFY_xxx)
    user   : The 'user' parameter value given when calling BASS_Encode_SetNotify
  }


const
{$IFDEF WINDOWS}
  bassencdll = 'bassenc.dll';
{$ENDIF}
{$IFDEF LINUX}
  bassencdll = 'libbassenc.so';
{$ENDIF}
{$IFDEF ANDROID}
  bassencdll = 'libbassenc.so';
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
    bassencdll = 'libbassenc.a';
  {$ELSE}
    bassencdll = 'libbassenc.dylib';
  {$ENDIF}
{$ENDIF}
var
  BASSENC_Handle:Thandle = 0;
  
  BASS_Encode_GetVersion:function: DWORD; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 

  BASS_Encode_Start:function(handle:DWORD; cmdline:PChar; flags:DWORD; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_StartLimit:function(handle:DWORD; cmdline:PChar; flags:DWORD; proc:ENCODEPROC; user:Pointer; limit:DWORD): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_StartUser:function(handle:DWORD; filename:PChar; flags:DWORD; proc:ENCODERPROC; user:Pointer): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_AddChunk:function(handle:HENCODE; id:PAnsiChar; buffer:Pointer; length:DWORD): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_IsActive:function(handle:DWORD): DWORD; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_Stop:function(handle:DWORD): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_SetPaused:function(handle:DWORD; paused:BOOL): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_Write:function(handle:DWORD; buffer:Pointer; length:DWORD): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_SetNotify:function(handle:DWORD; proc:ENCODENOTIFYPROC; user:Pointer): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_GetCount:function(handle:HENCODE; count:DWORD): QWORD; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_SetChannel:function(handle:DWORD; channel:DWORD): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_GetChannel:function(handle:HENCODE): DWORD; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  {$IFDEF WINDOWS}
    BASS_Encode_GetACMFormat:function(handle:DWORD; form:Pointer; formlen:DWORD; title:PChar; flags:DWORD): DWORD; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
    BASS_Encode_StartACM:function(handle:DWORD; form:Pointer; flags:DWORD; proc:ENCODEPROC; user:Pointer): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
    BASS_Encode_StartACMFile:function(handle:DWORD; form:Pointer; flags:DWORD; filename:PChar): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  {$ENDIF}

 {$IFDEF MACOS}
    BASS_Encode_StartCA:function(handle,ftype,atype,flags,bitrate:DWORD; proc:ENCODEPROCEX; user:Pointer): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
    BASS_Encode_StartCAFile:function(handle,ftype,atype,flags,bitrate:DWORD; filename:PChar): HENCODE; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
 {$ENDIF}

  BASS_Encode_CastInit:function(handle:HENCODE; server,pass,content,name,url,genre,desc,headers:PAnsiChar; bitrate:DWORD; pub:BOOL): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_CastSetTitle:function(handle:HENCODE; title,url:PAnsiChar): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_CastSendMeta:function(handle:HENCODE; mtype:DWORD; data:Pointer; length:DWORD): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_CastGetStats:function(handle:HENCODE; stype:DWORD; pass:PAnsiChar): PAnsiChar; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 

  BASS_Encode_ServerInit:function(handle:HENCODE; port:PAnsiChar; buffer,burst,flags:DWORD; proc:ENCODECLIENTPROC; user:Pointer): DWORD; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 
  BASS_Encode_ServerKick:function(handle:HENCODE; client:PAnsiChar): BOOL; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF}; 

function Load_BASSENCDLL(const dllfilename : string) : boolean;
procedure Unload_BASSENCDLL;

implementation

function Load_BASSENCDLL(const dllfilename : String) :boolean;
begin
  result:= false;
  if BASSENC_Handle <> 0 then Result:= true else
  begin
    {$IFDEF UNICODE}
      BASSENC_Handle:= LoadLibraryW(PWideChar(dllfilename));
    {$ELSE}
      BASSENC_Handle:= LoadLibrary(PChar(dllfilename));
    {$ENDIF}
    if BASSENC_Handle <> 0 then
    begin
      Pointer(BASS_Encode_GetVersion):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_GetVersion'));

      Pointer(BASS_Encode_Start):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_Start'));
      Pointer(BASS_Encode_StartLimit):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_StartLimit'));
      Pointer(BASS_Encode_StartUser):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_StartUser'));
      Pointer(BASS_Encode_AddChunk):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_AddChunk'));
      Pointer(BASS_Encode_IsActive):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_IsActive'));
      Pointer(BASS_Encode_Stop):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_Stop'));
      Pointer(BASS_Encode_SetPaused):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_SetPaused'));
      Pointer(BASS_Encode_Write):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_Write'));
      Pointer(BASS_Encode_SetNotify):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_SetNotify'));
      Pointer(BASS_Encode_GetCount):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_GetCount'));
      Pointer(BASS_Encode_SetChannel):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_SetChannel'));
      Pointer(BASS_Encode_GetChannel):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_GetChannel'));

      {$IFDEF WINDOWS}
        Pointer(BASS_Encode_GetACMFormat):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_GetACMFormat'));
        Pointer(BASS_Encode_StartACM):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_StartACM'));
        Pointer(BASS_Encode_StartACMFile):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_StartACMFile'));
      {$ENDIF}

      {$IFDEF MACOS}
        Pointer(BASS_Encode_StartCA):= GetProcAddress(BASSENC_Handle, PChar(BASS_Encode_StartCA''));
        Pointer(BASS_Encode_StartCAFile):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_StartCAFile'));
      {$ENDIF}

      Pointer(BASS_Encode_CastInit):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_CastInit'));
      Pointer(BASS_Encode_CastSetTitle):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_CastSetTitle'));
      Pointer(BASS_Encode_CastSendMeta):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_CastSendMeta'));
      Pointer(BASS_Encode_CastGetStats):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_CastGetStats'));

      Pointer(BASS_Encode_ServerInit):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_ServerInit'));
      Pointer(BASS_Encode_ServerKick):= GetProcAddress(BASSENC_Handle, PChar('BASS_Encode_ServerKick'));
    end;
    // check if all functions are OK
    if (Pointer(BASS_Encode_GetVersion)=nil) or (Pointer(BASS_Encode_Start)=nil) or
       (Pointer(BASS_Encode_StartLimit)=nil) or (Pointer(BASS_Encode_StartUser)=nil) or
       (Pointer(BASS_Encode_AddChunk)=nil) or (Pointer(BASS_Encode_IsActive)=nil) or
       (Pointer(BASS_Encode_Stop)=nil) or (Pointer(BASS_Encode_SetPaused)=nil) or
       (Pointer(BASS_Encode_Write)=nil) or (Pointer(BASS_Encode_SetNotify)=nil) or
       (Pointer(BASS_Encode_GetCount)=nil) or (Pointer(BASS_Encode_SetChannel)=nil) or
       (Pointer(BASS_Encode_GetChannel)=nil) or
       {$IFDEF WINDOWS}
         (Pointer(BASS_Encode_GetACMFormat)=nil) or (Pointer(BASS_Encode_StartACM)=nil) or
         (Pointer(BASS_Encode_StartACMFile)=nil) or
       {$ENDIF}

       {$IFDEF MACOS}
         (Pointer(BASS_Encode_StartCA)=nil) or (Pointer(BASS_Encode_StartCAFile)=nil) or
       {$ENDIF}
       (Pointer(BASS_Encode_CastInit)=nil) or (Pointer(BASS_Encode_CastSetTitle)=nil) or
       (Pointer(BASS_Encode_CastSendMeta)=nil) or (Pointer(BASS_Encode_CastGetStats)=nil) or
       (Pointer(BASS_Encode_ServerInit)=nil) or (Pointer(BASS_Encode_ServerKick)=nil) then
    begin
      FreeLibrary(BASSENC_Handle );
      Result := false
    end else Result := (BASSENC_Handle) <> 0;
  end;
end;

procedure Unload_BASSENCDLL;
begin
  if BASSENC_Handle <> 0 then FreeLibrary(BASSENC_Handle);
  BASSENC_Handle:= 0;
end; 
end.
