;Installation script for WebRadio
; bb - sdtp - February 2022
;--------------------------------
  Unicode true

  !include "MUI2.nsh"
  !include x64.nsh
  !include FileFunc.nsh

;--------------------------------
;Configuration

  ;General
  Name "Web radio tuner"
  OutFile "InstallWebradio.exe"
  !define lazarus_dir "C:\Users\Bernard\Documents\Lazarus"
  !define source_dir "${lazarus_dir}\webradio"


  RequestExecutionLevel admin
  
  ;Windows vista.. 10 manifest
  ManifestSupportedOS all

  ;!define MUI_LANGDLL_ALWAYSSHOW                     ; To display language selection dialog
  !define MUI_ICON "${source_dir}\webradio.ico"
  !define MUI_UNICON "${source_dir}\webradio.ico"

  ; The default installation directory
  InstallDir "$PROGRAMFILES\NewWebRadio"

  ; Variables to properly manage X64 or X32
  var exe_to_inst       ; "32.exe" or "64.exe"
  var exe_to_del
  var dll_to_inst       ; "32.dll" or "64.dll"
  var dll_to_del
  var instnewfolder     ; if old Delphi 32 bits version, instrall 32 bit new version in another folder
  var instfolder
;--------------------------------
; Interface Settings

!define MUI_ABORTWARNING

;--------------------------------
;Language Selection Dialog Settings

  ;Remember the installer language
  !define MUI_LANGDLL_REGISTRY_ROOT "HKCU"
  !define MUI_LANGDLL_REGISTRY_KEY "Software\SDTP\NewWebRadio"
  !define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"
  !define MUI_FINISHPAGE_SHOWREADME
  !define MUI_FINISHPAGE_SHOWREADME_TEXT "$(Check_box)"
  !define MUI_FINISHPAGE_SHOWREADME_FUNCTION inst_shortcut
  !define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
; Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE $(licence)
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;Languages
  !insertmacro MUI_LANGUAGE "English"
  !insertmacro MUI_LANGUAGE "French"
  !insertmacro MUI_RESERVEFILE_LANGDLL

  ;Licence langage file
  LicenseLangString Licence ${LANG_ENGLISH} "${source_dir}\license.txt"
  LicenseLangString Licence ${LANG_FRENCH}  "${source_dir}\licensf.txt"

  ;Language strings for uninstall string
  LangString RemoveStr ${LANG_ENGLISH}  "Web radio tuner"
  LangString RemoveStr ${LANG_FRENCH} "Tuner Web radio"

  ;Language string for links
  LangString ProgramLnkStr ${LANG_ENGLISH} "Web radio tuner.lnk"
  LangString ProgramLnkStr ${LANG_FRENCH} "Tuner Web radio.lnk"
  LangString UninstLnkStr ${LANG_ENGLISH} "Web radio tuner uninstall.lnk"
  LangString UninstLnkStr ${LANG_FRENCH} "Désinstallation de Tuner Web radio.lnk"

  LangString ProgramDescStr ${LANG_ENGLISH} "Web radio tuner"
  LangString ProgramDescStr ${LANG_FRENCH} "Tuner Web radio"

  ;Language strings for language selection dialog
  LangString LangDialog_Title ${LANG_ENGLISH} "Installer Language|$(^CancelBtn)"
  LangString LangDialog_Title ${LANG_FRENCH} "Langue d'installation|$(^CancelBtn)"

  LangString LangDialog_Text ${LANG_ENGLISH} "Please select the installer language."
  LangString LangDialog_Text ${LANG_FRENCH} "Choisissez la langue du programme d'installation."

  ;language strings for checkbox
  LangString Check_box ${LANG_ENGLISH} "Install a shortcut on the desktop"
  LangString Check_box ${LANG_FRENCH} "Installer un raccourci sur le bureau"

  ;Cannot install
  LangString No_Install ${LANG_ENGLISH} "The application cannot be installed on a 32bit system"
  LangString No_Install ${LANG_FRENCH} "Cette application ne peut pas être installée sur un système 32bits"
  
  ; Language string for remove old install
  LangString Remove_Old ${LANG_ENGLISH} "Install will remove a previous installation."
  LangString Remove_Old ${LANG_FRENCH} "Install va supprimer une ancienne installation."
  
  ;Wrong OS: 32 bit
   LangString Error_32b ${LANG_ENGLISH} "The program cannot install on 32bit OS."
   LangString Error_32b ${LANG_FRENCH} "Ce programme en peut pas être installé sur un OS 32 bits."
  

  !define MUI_LANGDLL_WINDOWTITLE "$(LangDialog_Title)"
  !define MUI_LANGDLL_INFO "$(LangDialog_Text)"
;--------------------------------

  !getdllversion  "${source_dir}\webradiowin64.exe" expv_
  !define FileVersion "${expv_1}.${expv_2}.${expv_3}.${expv_4}"

  VIProductVersion "${FileVersion}"
  VIAddVersionKey "FileVersion" "${FileVersion}"
  VIAddVersionKey "ProductName" "InstallWebRadio.exe"
  VIAddVersionKey "FileDescription" "WebRadio Installer"
  VIAddVersionKey "LegalCopyright" "sdtp - bb"
  VIAddVersionKey "ProductVersion" "${FileVersion}"
   
  ; Change nsis brand line
  BrandingText "$(ProgramDescStr) version ${FileVersion} - bb - sdtp"

; The stuff to install
Section "" ;No components page, name is not important
  SetShellVarContext all
  SetOutPath "$INSTDIR"

 ${If} ${RunningX64}
    SetRegView 64    ; change registry entries and install dir for 64 bit
  ${EndIf}

  ;Copy all files, files whhich have the same name in 32 and 64 bit are copied
  ; with 64 or 32 in their name, the renamed
  File  "${source_dir}\webradiowin64.exe"
  File  "${source_dir}\webradiowin32.exe"
  File "/oname=bass64.dll" "${lazarus_dir}\Bass\x64\bass.dll"
  File "/oname=bassenc64.dll" "${lazarus_dir}\Bass\x64\bassenc.dll"
  File "/oname=bass32.dll" "${lazarus_dir}\Bass\bass.dll"
  File "/oname=bassenc32.dll" "${lazarus_dir}\Bass\bass.dll"
  ; Install plugins
  CreateDirectory "$INSTDIR\plugins"
  SetOutPath "$INSTDIR\plugins"
  File "/oname=bass_aac64.dll" "${lazarus_dir}\Bass\x64\bass_aac.dll"
  File "/oname=basswma64.dll" "${lazarus_dir}\Bass\x64\basswma.dll"
  File "/oname=bassflac64.dll" "${lazarus_dir}\Bass\x64\bassflac.dll"
  File "/oname=bassenc_mp364.dll" "${lazarus_dir}\Bass\x64\bassenc_mp3.dll"
  File "/oname=bassenc_aac64.dll" "${lazarus_dir}\Bass\x64\bassenc_aac.dll"
  File "/oname=bassenc_ogg64.dll" "${lazarus_dir}\Bass\x64\bassenc_ogg.dll"
  File "/oname=bass_aac32.dll" "${lazarus_dir}\Bass\bass_aac.dll"
  File "/oname=basswma32.dll" "${lazarus_dir}\Bass\basswma.dll"
  File "/oname=bassflac32.dll" "${lazarus_dir}\Bass\bassflac.dll"
  File "/oname=bassenc_mp332.dll" "${lazarus_dir}\Bass\bassenc_mp3.dll"
  File "/oname=bassenc_aac32.dll" "${lazarus_dir}\Bass\bassenc_aac.dll"
  File "/oname=bassenc_ogg32.dll" "${lazarus_dir}\Bass\bassenc_ogg.dll"


  ${If} ${RunningX64}  ; change registry entries and install dir for 64 bit
     StrCpy $exe_to_inst "64.exe"
     StrCpy $dll_to_inst "64.dll"
     StrCpy $exe_to_del "32.exe"
     StrCpy $dll_to_del "32.dll"
  ${Else}
     StrCpy $exe_to_inst "32.exe"
     StrCpy $dll_to_inst "32.dll"
     StrCpy $exe_to_del "64.exe"
     StrCpy $dll_to_del "64.dll"
  ${EndIf}
  SetOutPath "$INSTDIR"
  ; Delete old files if they exist as we can not rename if the file exists
  Delete /REBOOTOK "$INSTDIR\webradio.exe"
  Delete /REBOOTOK "$INSTDIR\bass.dll"
  Delete /REBOOTOK "$INSTDIR\bassenc.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bass_aac.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\basswma.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bassflac.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bassenc_mp3.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bassenc_aac.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bassenc_ogg.dll"
  ; Rename 32 or 64 files
  Rename /REBOOTOK "$INSTDIR\webradiowin$exe_to_inst" "$INSTDIR\webradio.exe"
  Rename /REBOOTOK "$INSTDIR\bass$dll_to_inst" "$INSTDIR\bass.dll"
  Rename /REBOOTOK "$INSTDIR\bassenc$dll_to_inst" "$INSTDIR\bassenc.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bass_aac$dll_to_inst" "$INSTDIR\plugins\bass_aac.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\basswma$dll_to_inst" "$INSTDIR\plugins\basswma.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bassflac$dll_to_inst" "$INSTDIR\plugins\bassflac.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bassenc_mp3$dll_to_inst" "$INSTDIR\plugins\bassenc_mp3.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bassenc_aac$dll_to_inst" "$INSTDIR\plugins\bassenc_aac.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bassenc_ogg$dll_to_inst" "$INSTDIR\plugins\bassenc_ogg.dll"
  ; delete non used files
  Delete "$INSTDIR\webradiowin$exe_to_del"
  Delete "$INSTDIR\bass$dll_to_del"
  Delete "$INSTDIR\bassenc$dll_to_del"
  Delete "$INSTDIR\plugins\bass_aac$dll_to_del"
  Delete "$INSTDIR\plugins\basswma$dll_to_del"
  Delete "$INSTDIR\plugins\bassflac$dll_to_del"
  Delete "$INSTDIR\plugins\bassenc_mp3$dll_to_del"
  Delete "$INSTDIR\plugins\bassenc_aac$dll_to_del"
  Delete "$INSTDIR\plugins\bassenc_ogg$dll_to_del"
  ; Install other files
  File "${source_dir}\licensf.txt"
  File "${source_dir}\license.txt"
  File "${source_dir}\history.txt"
  File "${source_dir}\webradio.txt"
  File "${source_dir}\webradio.lng"
  File "${source_dir}\webradio.ini"
  File /r "${source_dir}\help"
  ; write out uninstaller
  WriteUninstaller "$INSTDIR\uninst.exe"
  ; Get install folder size
  ${GetSize} "$INSTDIR" "/S=0K" $0 $1 $2

  ;Write uninstall in register
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradio" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradio" "DisplayIcon" "$INSTDIR\uninst.exe"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradio" "DisplayName" "$(RemoveStr)"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradio" "DisplayVersion" "${FileVersion}"
  WriteRegDWORD HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradio" "EstimatedSize" "$0"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradio" "Publisher" "SDTP"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradio" "URLInfoAbout" "www.sdtp.com"
  WriteRegStr HKEY_LOCAL_MACHINE "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradio" "HelpLink" "www.sdtp.com"
  ;Store install folder
  WriteRegStr HKCU "Software\SDTP\newwebradio" "InstallDir" $INSTDIR

SectionEnd ; end the section

; Install shortcuts, language dependant

Section "Start Menu Shortcuts"
  SetShellVarContext all
  CreateDirectory "$SMPROGRAMS\NewWebradio"
  CreateShortCut  "$SMPROGRAMS\NewWebradio\$(ProgramLnkStr)" "$INSTDIR\webradio.exe" "" "$INSTDIR\webradio.exe" 0 SW_SHOWNORMAL "" "$(ProgramDescStr)"
  CreateShortCut  "$SMPROGRAMS\NewWebradio\$(UninstLnkStr)" "$INSTDIR\uninst.exe" "" "$INSTDIR\uninst.exe" 0

SectionEnd

;Uninstaller Section

Section Uninstall
SetShellVarContext all
${If} ${RunningX64}
  SetRegView 64    ; change registry entries and install dir for 64 bit
${EndIf}
; add delete commands to delete whatever files/registry keys/etc you installed here.
Delete /REBOOTOK "$INSTDIR\webradio.exe"
Delete "$INSTDIR\history.txt"
Delete "$INSTDIR\webradio.txt"
Delete "$INSTDIR\webradio.lng"
Delete "$INSTDIR\webradio.ini"
Delete /REBOOTOK "$INSTDIR\bass.dll"
Delete /REBOOTOK "$INSTDIR\bassenc.dll"
;Delete "$INSTDIR\libeay32.dll"
;Delete "$INSTDIR\ssleay32.dll"
Delete "$INSTDIR\licensf.txt"
Delete "$INSTDIR\license.txt"
;Delete "$INSTDIR\OpenSSL License.txt"
Delete "$INSTDIR\uninst.exe"
RMDir /r "$INSTDIR\help"
RMDir /r "$INSTDIR\plugins"
; remove shortcuts, if any.
  Delete  "$SMPROGRAMS\NewWebradio\$(ProgramLnkStr)"
  Delete  "$SMPROGRAMS\NewWebradio\$(UninstLnkStr)"
  Delete  "$DESKTOP\$(ProgramLnkStr)"


; remove directories used.
  RMDir "$SMPROGRAMS\newwebradio"
  RMDir "$INSTDIR"

; Remove installed keys
DeleteRegKey HKCU "Software\SDTP\newwebradio"
DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\newwebradiox"
; remove also autostart settings if any
DeleteRegValue HKCU "Software\Microsoft\Windows\CurrentVersion\Run\" "webradio"
DeleteRegValue HKCU "Software\Microsoft\Windows\CurrentVersion\RunOnce\" "webradio"

SectionEnd ; end of uninstall section

Function inst_shortcut
  CreateShortCut "$DESKTOP\$(ProgramLnkStr)" "$INSTDIR\webradio.exe"
FunctionEnd

Function .onInit

  ; !insertmacro MUI_LANGDLL_DISPLAY
  ${If} ${RunningX64}
    SetRegView 64    ; change registry entries and install dir for 64 bit
    ; no conflict as the old program is in x86 folder
    StrCpy "$instnewfolder" "$PROGRAMFILES64\webradio";
    StrCpy "$instfolder" "$PROGRAMFILES64\webradio";
  ${Else}
    ; new folder is different, don't break old program if it is installed
    StrCpy "$instnewfolder" "$PROGRAMFILES\newwebradio";
    StrCpy "$instfolder" ,"$PROGRAMFILES\webradio";
  ${EndIf}
  
  SetShellVarContext all
  ; Close all apps instance
  FindProcDLL::FindProc "$INSTDIR\webradio.exe"
  ${While} $R0 > 0
    FindProcDLL::KillProc "$INSTDIR\webradio.exe"
    FindProcDLL::WaitProcEnd "$INSTDIR\webradio.exe" -1
    FindProcDLL::FindProc "$INSTDIR\webradio.exe"
  ${EndWhile}
  StrCpy "$INSTDIR" "$instfolder"
  ; See if there is old program
  ReadRegStr $R0 HKLM "SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall\webradio" "UninstallString"
  ${If} ${RunningX64}
    SetRegView 64    ; change registry entries and install dir for 64 bit
  ${EndIf}
   ${If} $R0 == ""
        StrCpy $INSTDIR "$instfolder"
        Goto Done
   ${EndIf}
   MessageBox MB_YESNO "$(Remove_Old)" IDYES true IDNO false
   true:
      StrCpy $INSTDIR "$instfolder"
      ExecWait $R0
  false:
     StrCpy $INSTDIR "$instnewfolder"
  Done:
FunctionEnd