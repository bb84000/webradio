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
  Var /GLOBAL prg_to_inst
  Var /GLOBAL prg_to_del

  ${If} ${RunningX64}  ; change registry entries and install dir for 64 bit
     StrCpy "$prg_to_inst" "$INSTDIR\webradiowin64.exe"
     StrCpy "$prg_to_del" "$INSTDIR\webradiowin32.exe"
     File  "${lazarus_dir}\Bass\x64\bass.dll"
     File  "${lazarus_dir}\Bass\x64\bassenc.dll"
     CreateDirectory "$INSTDIR\plugins"
     SetOutPath "$INSTDIR\plugins"
     File  "${lazarus_dir}\Bass\x64\bass_aac.dll"
     File  "${lazarus_dir}\Bass\x64\bassflac.dll"
     File  "${lazarus_dir}\Bass\x64\basswma.dll"
     File  "${lazarus_dir}\Bass\x64\bassenc_aac.dll"
     ;File  "${source_dir}\plugins\x64\bassenc_flac.dll"
     File  "${lazarus_dir}\Bass\x64\bassenc_mp3.dll"
     File  "${lazarus_dir}\Bass\x64\bassenc_ogg.dll"

     ;IfFileExists "$WINDIR\sysnative\libeay32.dll" ssl_lib64_found ssl_lib64_not_found
     ;ssl_lib64_not_found:
     ;  File "${lazarus_dir}\openssl\win64\libeay32.dll"
     ;  File "${lazarus_dir}\openssl\win64\ssleay32.dll"
     ;  File "${lazarus_dir}\openssl\OpenSSL License.txt"
     ;ssl_lib64_found:
  ${Else}
     StrCpy "$prg_to_inst" "$INSTDIR\webradiowin32.exe"
     StrCpy "$prg_to_del" "$INSTDIR\webradiowin64.exe"
     CreateDirectory "$INSTDIR\plugins"
     SetOutPath "$INSTDIR\plugins"
     File  "${lazarus_dir}\Bass\bass_aac.dll"
     File  "${lazarus_dir}\Bass\bassflac.dll"
     File  "${lazarus_dir}\Bass\basswma.dll"
     File  "${lazarus_dir}\Bass\bassenc_aac.dll"
     ;File  "${source_dir}\plugins\x64\bassenc_flac.dll"
     File  "${lazarus_dir}\Bass\bassenc_mp3.dll"
     File  "${lazarus_dir}\Bass\bassenc_ogg.dll"

     ;IfFileExists "$WINDIR\system32\libeay32.dll" ssl_lib32_found ssl_lib32_not_found
     ;ssl_lib32_not_found:
     ;  File "${lazarus_dir}\openssl\win32\libeay32.dll"
     ;  File "${lazarus_dir}\openssl\win32\ssleay32.dll"
     ;  File "${lazarus_dir}\openssl\OpenSSL License.txt"
     ;ssl_lib32_found:
   ${EndIf}
   SetOutPath "$INSTDIR"  ; Dans le cas ou on n'aurait pas pu fermer l'application
   Delete /REBOOTOK "$INSTDIR\webradio.exe"
   File "${source_dir}\webradiowin64.exe"
   File "${source_dir}\licensf.txt"
   File "${source_dir}\license.txt"
   File "${source_dir}\history.txt"
   File "${source_dir}\webradio.txt"
   File "${source_dir}\webradio.lng"
   File "${source_dir}\webradio.ini"
    ;File /r "${source_dir}\help"
  Rename /REBOOTOK "$prg_to_inst" "$INSTDIR\webradio.exe"
  Delete /REBOOTOK "$prg_to_del"

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
Delete "$INSTDIR\bass.dll"
Delete "$INSTDIR\bassenc.dll"
;Delete "$INSTDIR\libeay32.dll"
;Delete "$INSTDIR\ssleay32.dll"
Delete "$INSTDIR\licensf.txt"
Delete "$INSTDIR\license.txt"
;Delete "$INSTDIR\OpenSSL License.txt"
Delete "$INSTDIR\uninst.exe"
;RMDir /r "$INSTDIR\help"
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
    StrCpy "$INSTDIR" "$PROGRAMFILES64\newwebradio"
  ${Else}

  ${EndIf}
  SetShellVarContext all
  ; Close all apps instance
  FindProcDLL::FindProc "$INSTDIR\webradio.exe"
  ${While} $R0 > 0
    FindProcDLL::KillProc "$INSTDIR\webradio.exe"
    FindProcDLL::WaitProcEnd "$INSTDIR\webradio.exe" -1
    FindProcDLL::FindProc "$INSTDIR\webradio.exe"
  ${EndWhile}
  ; See if there is old program
  ReadRegStr $R0 HKLM "SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall\webradio" "UninstallString"
   ${If} $R0 == ""
        Goto Done
   ${EndIf}
   MessageBox MB_YESNO "$(Remove_Old)" IDYES true IDNO false
   true:
      ExecWait $R0
  false:
  Done:
FunctionEnd