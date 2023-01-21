;------------------------------------------------------------------------------------------
; NSIS Installation script for 32/64 bit WebRadio
; bb - sdtp - October 2022
;
; 22/10/2022 Replaced onInit with a custom page to check running app and previous versions
;------------------------------------------------------------------------------------------

!define FileVersion "1.0.0.2"

 Unicode true

!include "MUI2.nsh"
!include x64.nsh
!include FileFunc.nsh
!include WordFunc.nsh
!include StrFunc.nsh
!include nsDialogs.nsh
  
${Using:StrFunc} StrRep   ; Replace substring function, uses StrFunc.nsh needed in custom page

;------------------------------------------------------------------------------------------
; Installer customization
!define lazarus_dir "C:\Users\Bernard\Documents\Lazarus"
!define source_dir "${lazarus_dir}\webradio"
; generic program name suffix, then use ${prog_name} constant
!define prog_name "Webradio"
; Key name for uninstall
!define uninst_key "Software\Microsoft\Windows\CurrentVersion\Uninstall\${prog_name}"
Name "Web radio tuner"
;------------------------------------------------------------------------------------------
; Common data for all installers
OutFile "Install${prog_name}.exe"
; The default installation directory
InstallDir "$PROGRAMFILES\${prog_name}"

;Windows vista.. 11 manifest
ManifestSupportedOS all
; Install with admin privileges
RequestExecutionLevel admin

; Variables to properly manage X64 or X32
var exe_to_inst       ; "32.exe" or "64.exe"
var exe_to_del
var dll_to_inst       ; "32.dll" or "64.dll"
var dll_to_del
var sysfolder         ; system 32 folder
; Variables for version management
var prev_inst_folder  ; Install folder of previous version
var prev_inst_version ; Number of previous version
var Custom_cbuninst   ; uninstall check box in custopm page
var Custom_cbstate
var old_uninstpath
var estimated_size
var install_date
  
; Interface Settings
; installer and uninstall icons
!define MUI_ABORTWARNING
!define MUI_ICON "${source_dir}\${prog_name}.ico"
!define MUI_UNICON "${source_dir}\${prog_name}.ico"
; Welcome and finsh pages vertical images 164x314 pixels max unstretched
!define MUI_WELCOMEFINISHPAGE_BITMAP "${source_dir}\images\${prog_name}_welcome.bmp"
!define MUI_WELCOMEFINISHPAGE_BITMAP_NOSTRETCH
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "${source_dir}\images\${prog_name}_welcome.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP_NOSTRETCH
;Remember the installer language
!define MUI_LANGDLL_REGISTRY_ROOT "HKCU"
!define MUI_LANGDLL_REGISTRY_KEY "Software\SDTP\${prog_name}"
!define MUI_LANGDLL_REGISTRY_VALUENAME "Installer Language"
;!define MUI_LANGDLL_ALWAYSSHOW                     ; To force language selection dialog
;checkboxes on finsh page
!define MUI_FINISHPAGE_SHOWREADME
!define MUI_FINISHPAGE_SHOWREADME_TEXT "$(Check_box)"
!define MUI_FINISHPAGE_SHOWREADME_FUNCTION inst_shortcut
!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
!define MUI_FINISHPAGE_RUN
!define MUI_FINISHPAGE_RUN_TEXT "$(App_run)"
!define MUI_FINISHPAGE_RUN_FUNCTION run_appli
!define MUI_FINISHPAGE_RUN_NOTCHECKED
  
; Pages
;!define MUI_WELCOMEPAGE_TITLE_3LINES
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE $(licence)
Page custom ChkVerPage ChkVerLeave                 ; Custom page for version check
;!insertmacro MUI_PAGE_DIRECTORY                   ; Let select the install directory
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

;------------------------------------------------------------------------------------------
; CustomizedLanguage strings for uninstall string
LangString RemoveStr ${LANG_ENGLISH}  "Web radio tuner"
LangString RemoveStr ${LANG_FRENCH} "Tuner Web radio"

;Language string for links must be customized
LangString ProgramLnkStr ${LANG_ENGLISH} "Web radio tuner.lnk"
LangString ProgramLnkStr ${LANG_FRENCH} "Tuner Web radio.lnk"
LangString UninstLnkStr ${LANG_ENGLISH} "Web radio tuner uninstall.lnk"
LangString UninstLnkStr ${LANG_FRENCH} "Désinstallation de Tuner Web radio.lnk"
LangString ProgramDescStr ${LANG_ENGLISH} "Web radio tuner"
LangString ProgramDescStr ${LANG_FRENCH} "Tuner Web radio"
LangString ShortcutDescStr ${LANG_ENGLISH} "Application to listen radios broadcasted on Internet"
LangString ShortcutDescStr ${LANG_FRENCH} "Application pour écouter les radios diffusées sur Internet"
;-------------------------------------------------------------------------------------------

; Common language strings
; Language selection dialog
LangString LangDialog_Title ${LANG_ENGLISH} "Installer Language|$(^CancelBtn)"
LangString LangDialog_Title ${LANG_FRENCH} "Langue d'installation|$(^CancelBtn)"
LangString LangDialog_Text ${LANG_ENGLISH} "Please select the installer language."
LangString LangDialog_Text ${LANG_FRENCH} "Choisissez la langue du programme d'installation."

; Finish checkboxes
LangString Check_box ${LANG_ENGLISH} "Install a shortcut on the desktop"
LangString Check_box ${LANG_FRENCH} "Installer un raccourci sur le bureau"
LangString App_run ${LANG_ENGLISH} "Launch $(RemoveStr)"
LangString App_run ${LANG_FRENCH} "Lancer $(RemoveStr)"

; custom page strings
LangString Custom_title ${LANG_ENGLISH} "Installation initialization"
LangString Custom_title ${LANG_FRENCH} "Initialisation de l'installation"
LangString Custom_subtitle ${LANG_ENGLISH} "Close program's running instance(s) if there are and search a previous installation"
LangString Custom_subtitle ${LANG_FRENCH} "Ferme le programme s'il est en cours d'exécution et recherche une installation précédente"
LangString Custom_labelclose ${LANG_ENGLISH} "Closing eventual running program instance..."
LangString Custom_labelclose ${LANG_FRENCH} "Fermeture du programme s'il est en cours d'exécution..."
LangString Custom_labelsearchprev ${LANG_ENGLISH} "Searching previous program installation..."
LangString Custom_labelsearchprev ${LANG_FRENCH} "Recheche d'une précédente installation du programme..."
LangString Custom_newinstall ${LANG_ENGLISH} "No previous installation found. Click Install to continue or Cancel to abort installation"
LangString Custom_newinstall ${LANG_FRENCH} "Pas d'installation précxédente trouvée. Cliquer sur Installer pour continuer ou Annuler pour abandonner l'installation"
LangString Custom_delphifound ${LANG_ENGLISH} "Previous conflicting installation to be uninstalled. Click Install to continue or Cancel to abort installation"
LangString Custom_delphifound ${LANG_FRENCH} "Une précédente installation va être désinstallée. Cliquer sur Installer pour continuer ou Annuler pour abandonner l'installation"
LangString Custom_delphichecked ${LANG_ENGLISH} "If you uncheck this box, previous install can conflict with new install"
LangString Custom_delphichecked ${LANG_FRENCH} "Si vous décochez cette case, une précente installation peut perturber la nouvelle"
   
;Install checks
LangString Already_inst ${LANG_ENGLISH} "The program is already installed. Click Install to reinstall or Cancel to abort installation"
LangString Already_inst ${LANG_FRENCH} "Ce programme est déjà installé. Cliquer sur Installer pour le réinstaller ou Annuler pour abandonner l'installation"
LangString Upgrade_inst ${LANG_ENGLISH} "A previous version %ver1% of the program is installed. Do you want upgrade this previous version with %ver2% version ?"
LangString Upgrade_inst ${LANG_FRENCH} "Une version précédente %ver1% de ce programme est installée. Voulez vous mettre à jour cette version avec la version %ver2% ?"
LangString Downgrade_inst ${LANG_ENGLISH} "A most recent version %ver1% of the program is installed. Do you want really return to an older version %ver2% ?"
LangString Downgrade_inst ${LANG_FRENCH} "Une version plus récente %ver1% de ce programme est installée. Voulez vous vraiment revenir à une version plus ancienne %ver2% ?"

!define MUI_LANGDLL_WINDOWTITLE "$(LangDialog_Title)"
!define MUI_LANGDLL_INFO "$(LangDialog_Text)"

; Get new version of program to install
!getdllversion  "${source_dir}\${prog_name}win64.exe" expv_
!define ProductVersion "${expv_1}.${expv_2}.${expv_3}.${expv_4}"
!define /date DATE "%d/%m/%Y %H:%M:%S"
  
; Generate version information of installer and uninstaller
VIProductVersion ${ProductVersion}
VIFileVersion ${FileVersion}
VIAddVersionKey "FileVersion" "${FileVersion}"
VIAddVersionKey "ProductName" "Install${prog_name}.exe"
VIAddVersionKey "FileDescription" "${prog_name} Installer"
VIAddVersionKey "LegalCopyright" "sdtp - bb"
VIAddVersionKey "ProductVersion" "${ProductVersion}"
VIAddVersionKey "LegalTrademarks" "sdtp - bb - ${DATE}"

; Change nsis brand line
BrandingText "$(ProgramDescStr) version ${ProductVersion} - bb - sdtp"

; The stuff to install
Section "" ;No components page, name is not important
  SetShellVarContext all
  ; change registry entries for 64 bit
  ${If} ${RunningX64}           
    SetRegView 64
  ${EndIf}
  SetOutPath "$INSTDIR"
  ; need to uninstall previous version if  transmitted from custom page
  ${If} $Custom_cbstate <> ${BST_UNCHECKED}
  ${andif} $old_uninstpath != ""
     Execwait $old_uninstpath
  ${EndIf}
  ;Copy all files, files whhich have the same name in 32 and 64 bit are copied
  ; with 64 or 32 in their name, the renamed
  File  "${source_dir}\${prog_name}win64.exe"
  File  "${source_dir}\${prog_name}win32.exe"
  File "/oname=bass64.dll" "${lazarus_dir}\Bass\x64\bass.dll"
  File "/oname=bassenc64.dll" "${lazarus_dir}\Bass\x64\bassenc.dll"
  File "/oname=bass32.dll" "${lazarus_dir}\Bass\bass.dll"
  File "/oname=bassenc32.dll" "${lazarus_dir}\Bass\bass.dll"
  File "/oname=libeay3264.dll" "${lazarus_dir}\openssl\win64\libeay32.dll"
  File "/oname=ssleay3264.dll" "${lazarus_dir}\openssl\win64\ssleay32.dll"
  File "/oname=libeay3232.dll" "${lazarus_dir}\openssl\win32\libeay32.dll"
  File "/oname=ssleay3232.dll" "${lazarus_dir}\openssl\win32\ssleay32.dll"
  File "${lazarus_dir}\openssl\OpenSSL License.txt"
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

  ; Set variables according 64 ou 32 bit windows version
  ${If} ${RunningX64} 
     StrCpy $exe_to_inst "64.exe"
     StrCpy $dll_to_inst "64.dll"
     StrCpy $exe_to_del "32.exe"
     StrCpy $dll_to_del "32.dll"
     StrCpy $sysfolder "$WINDIR\sysnative"
  ${Else}
     StrCpy $exe_to_inst "32.exe"
     StrCpy $dll_to_inst "32.dll"
     StrCpy $exe_to_del "64.exe"
     StrCpy $dll_to_del "64.dll"
     StrCpy $sysfolder "$WINDIR\system32"
  ${EndIf}
  SetOutPath "$INSTDIR"
  ; Delete old files if they exist as we can not rename if the file exists
  Delete /REBOOTOK "$INSTDIR\${prog_name}.exe"
  Delete /REBOOTOK "$INSTDIR\bass.dll"
  Delete /REBOOTOK "$INSTDIR\bassenc.dll"
  Delete /REBOOTOK "$INSTDIR\libeay32.dll"
  Delete /REBOOTOK "$INSTDIR\\ssleay32.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bass_aac.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\basswma.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bassflac.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bassenc_mp3.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bassenc_aac.dll"
  Delete /REBOOTOK "$INSTDIR\plugins\bassenc_ogg.dll"
  ; Rename 32 or 64 files to their proper name
  Rename /REBOOTOK "$INSTDIR\${prog_name}win$exe_to_inst" "$INSTDIR\${prog_name}.exe"
  Rename /REBOOTOK "$INSTDIR\bass$dll_to_inst" "$INSTDIR\bass.dll"
  Rename /REBOOTOK "$INSTDIR\bassenc$dll_to_inst" "$INSTDIR\bassenc.dll"
  ; Install ssl libraries if not already in system folder
  IfFileExists "$sysfolder\libeay32.dll" ssl_lib_found ssl_lib_not_found
  ssl_lib_not_found:
    File "${lazarus_dir}\openssl\OpenSSL License.txt"
    Rename /REBOOTOK "$INSTDIR\libeay32$dll_to_inst" "$INSTDIR\libeay32.dll"
    Rename /REBOOTOK "$INSTDIR\ssleay32$dll_to_inst" "$INSTDIR\\ssleay32.dll"
    Goto ssl_lib_set
  ssl_lib_found:
    Delete "$INSTDIR\libeay32$dll_to_inst"
    Delete "$INSTDIR\ssleay32$dll_to_inst"
  ssl_lib_set:
  Rename /REBOOTOK "$INSTDIR\plugins\bass_aac$dll_to_inst" "$INSTDIR\plugins\bass_aac.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\basswma$dll_to_inst" "$INSTDIR\plugins\basswma.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bassflac$dll_to_inst" "$INSTDIR\plugins\bassflac.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bassenc_mp3$dll_to_inst" "$INSTDIR\plugins\bassenc_mp3.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bassenc_aac$dll_to_inst" "$INSTDIR\plugins\bassenc_aac.dll"
  Rename /REBOOTOK "$INSTDIR\plugins\bassenc_ogg$dll_to_inst" "$INSTDIR\plugins\bassenc_ogg.dll"
  ; delete non used files
  Delete "$INSTDIR\${prog_name}win$exe_to_del"
  Delete "$INSTDIR\bass$dll_to_del"
  Delete "$INSTDIR\bassenc$dll_to_del"
  Delete "$INSTDIR\libeay32$dll_to_del"
  Delete "$INSTDIR\ssleay32$dll_to_del"
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
  File "${source_dir}\${prog_name}.txt"
 ; File "${source_dir}\${prog_name}.lng"
  File "${source_dir}\${prog_name}.ini"
  File /r "${source_dir}\help"
  ; delete old lng file
  IfFileExists "$INSTDIR\${prog_name}.lng" 0 +2
  Delete "$INSTDIR\${prog_name}.lng"
  ; Install language files
  CreateDirectory "$INSTDIR\lang"
  SetOutPath "$INSTDIR\lang"
  File "${source_dir}\lang\en.lng"
  File "${source_dir}\lang\fr.lng"
  ; restore install directory variable
  SetOutPath "$INSTDIR"
  ; write out uninstaller
  WriteUninstaller "$INSTDIR\uninst.exe"
  
  ; Get install folder size in var $estimated_size
  ${GetSize} "$INSTDIR" "/S=0K" $estimated_size $1 $2
  ; Get install date in var $install_date
  ${GetTime} "" "L" $0 $1 $2 $3 $4 $5 $6 ; $0 day ; $1 month ; $2 year $3 day of week name ; $4 hour ; $5 minutes ; $6 seconds
  StrCpy $install_date "$0/$1/$2 $4:$5:$6"
  
  ;Write uninstall infos in registry
  WriteRegStr HKLM ${uninst_key} "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr HKLM ${uninst_key} "DisplayIcon" "$INSTDIR\uninst.exe"
  WriteRegStr HKLM ${uninst_key} "DisplayName" "$(RemoveStr)"
  WriteRegStr HKLM ${uninst_key} "DisplayVersion" "${ProductVersion}"
  WriteRegDWORD HKLM ${uninst_key} "EstimatedSize" "$estimated_size"
  WriteRegStr HKLM ${uninst_key} "Publisher" "SDTP"
  WriteRegStr HKLM ${uninst_key} "URLInfoAbout" "www.sdtp.com"
  WriteRegStr HKLM ${uninst_key} "HelpLink" "www.sdtp.com"
  ;Store install folder and product version for later use
  WriteRegStr HKCU "Software\SDTP\${prog_name}" "InstallDir" $INSTDIR
  WriteRegStr HKCU "Software\SDTP\${prog_name}" "ProductVersion" ${ProductVersion}
  WriteRegStr HKCU "Software\SDTP\${prog_name}" "InstallDate" $install_date
SectionEnd ; end the section

; Install shortcuts, language dependant

Section "Start Menu Shortcuts"
  SetShellVarContext all
  ;delete non localized shortcuts if exist
  Delete  "$SMPROGRAMS\${prog_name}\$(ProgramLnkStr)"
  Delete  "$SMPROGRAMS\${prog_name}\$(UninstLnkStr)"
  RMDir "$SMPROGRAMS\${prog_name}"
  ; create localized shortcuts
  CreateDirectory "$SMPROGRAMS\$(RemoveStr)"
  CreateShortCut  "$SMPROGRAMS\$(RemoveStr)\$(ProgramLnkStr)" "$INSTDIR\${prog_name}.exe" "" "$INSTDIR\${prog_name}.exe" 0 SW_SHOWNORMAL "" "$(ProgramDescStr)"
  CreateShortCut  "$SMPROGRAMS\$(RemoveStr)\$(UninstLnkStr)" "$INSTDIR\uninst.exe" "" "$INSTDIR\uninst.exe" 0
SectionEnd

;Uninstaller Section

Section Uninstall
  SetShellVarContext all
  ${If} ${RunningX64}
    SetRegView 64    ; change registry entries and install dir for 64 bit
  ${EndIf}
  ; add delete commands to delete whatever files/registry keys/etc you installed here.
  Delete /REBOOTOK "$INSTDIR\${prog_name}.exe"
  Delete "$INSTDIR\history.txt"
  Delete "$INSTDIR\${prog_name}.txt"
  Delete "$INSTDIR\${prog_name}.lng"
  Delete "$INSTDIR\${prog_name}.ini"
  Delete /REBOOTOK "$INSTDIR\bass.dll"
  Delete /REBOOTOK "$INSTDIR\bassenc.dll"
  Delete "$INSTDIR\libeay32.dll"
  Delete "$INSTDIR\ssleay32.dll"
  Delete "$INSTDIR\licensf.txt"
  Delete "$INSTDIR\license.txt"
  Delete "$INSTDIR\OpenSSL License.txt"
  Delete "$INSTDIR\uninst.exe"
  RMDir /r "$INSTDIR\lang"
  RMDir /r "$INSTDIR\help"
  RMDir /r "$INSTDIR\plugins"
  ; remove shortcuts, if any.
  Delete  "$SMPROGRAMS\$(RemoveStr)\$(ProgramLnkStr)"
  Delete  "$SMPROGRAMS\$(RemoveStr)\$(UninstLnkStr)"
  Delete  "$DESKTOP\$(ProgramLnkStr)"
  ; remove directories used.
  RMDir "$SMPROGRAMS\$(RemoveStr)"
  RMDir "$INSTDIR"
  ; Remove installed keys
  DeleteRegKey HKCU "Software\SDTP\${prog_name}"
  DeleteRegKey HKLM ${uninst_key}
  ; remove also autostart settings if any
  DeleteRegValue HKCU "Software\Microsoft\Windows\CurrentVersion\Run\"${prog_name}
  DeleteRegValue HKCU "Software\Microsoft\Windows\CurrentVersion\RunOnce\"${prog_name}
SectionEnd ; end of uninstall section

Function inst_shortcut
  CreateShortCut "$DESKTOP\$(ProgramLnkStr)" "$INSTDIR\${prog_name}.exe" "" "" "" SW_SHOWNORMAL "" $(ShortcutDescStr)
FunctionEnd

Function run_appli
  Exec "$INSTDIR\${prog_name}.exe"
FunctionEnd

; custom page to check running program, old veresions, etc...
Function ChkVerPage
  !insertmacro MUI_HEADER_TEXT $(Custom_title) $(Custom_subtitle)
  StrCpy $Custom_cbstate ${BST_UNCHECKED}    ; Set initial/default state
  nsDialogs::Create 1018
  Pop $0
  ${If} $0 == error
    Abort
  ${EndIf}
  ; Close all apps instance
  ${NSD_CreateLabel} 0 0 100% 12u $(Custom_labelclose)
  Pop $0
  FindProcDLL::FindProc "${prog_name}.exe"
  ${While} $R0 > 0
    FindProcDLL::KillProc "${prog_name}.exe"
    FindProcDLL::WaitProcEnd "${prog_name}.exe" -1
    FindProcDLL::FindProc "${prog_name}.exe"
  ${EndWhile}
  ${NSD_CreateLabel} 0 20 100% 12u $(Custom_labelsearchprev)
  Pop $0
  ; Retrieve previous install and current program version
  ReadRegStr $prev_inst_folder HKCU "Software\SDTP\${prog_name}" "InstallDir"
  ${If} $prev_inst_folder == ""     ; Install dir not found program is not already installed
    ${NSD_CreateLabel} 0 40 100% 20u $(Custom_newinstall)
    Pop $0
  ${else}
    ; See if there is old program, cannot use reg key as it is the same as new program.
    ; basswma.dll is in the progrzam folder in old versionj, in the plugin folder in new version
    IfFileExists "$prev_inst_folder\basswma.dll" delphi_prg_found delphi_prg_not_found
    delphi_prg_found:
      ${NSD_CreateLabel} 0 40 100% 20u $(Custom_delphifound)
      Pop $0
      ; Retreive uninstall path in registry
      ;StrCpy $old_uninstpath "$prev_inst_folder\uninst.exe"
      ${If} ${RunningX64}
        ReadRegStr $old_uninstpath HKLM "SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall\${prog_name}" "UninstallString"
      ${Else}
        ReadRegStr $old_uninstpath HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\${prog_name}" "UninstallString"
      ${EndIf}
      ${NSD_CreateCheckbox} 0 -50 100% 16u $(Custom_delphichecked)
      Pop $Custom_cbuninst
      ; cjheck the uninstall box
      ${NSD_Check} $Custom_cbuninst
      goto instnew      ; no lazarus version likely installed
    delphi_prg_not_found:
    ; check previous versions and set proper programfiles folder
    StrCpy "$INSTDIR" $prev_inst_folder
    ReadRegStr $prev_inst_version HKCU "Software\SDTP\${prog_name}" "ProductVersion"
    ${if} $prev_inst_version == ""                             ; version not set in registry, get file version
      ${GetFileVersion} "$prev_inst_folder\${prog_name}.exe" $prev_inst_version
    ${endif}
    ; compare instelled version and new poroduct version
    ${VersionConvert} ${ProductVersion} "" $R0
    ${VersionConvert} $prev_inst_version "" $R1
    ${VersionCompare} $R0 $R1 $R2
    ${If} $R2 == 0       ; same version, install only to repair
      StrCpy $R3 $(Already_inst)
    ${elseif} $R2 == 1   ; version can be upgraded
      StrCpy $R3 $(Upgrade_inst)
    ${elseif} $R2 == 2   ; Downgrade version ?
      StrCpy $R3 $(Downgrade_inst)
    ${endif}
    ; Replace with versions values in language strings
    ${StrRep} $R1 $R3 "%ver1%" $prev_inst_version
    ${StrRep} $R3 $R1 "%ver2%" ${ProductVersion}
    ${NSD_CreateLabel} 0 40 100% 20u $R3
    Pop $0
    instnew:
  ${endif}
  nsDialogs::Show
FunctionEnd

Function ChkVerLeave
  ; store checkbox sate in var Custom_cbstate to use in main saction
  ${NSD_GetState} $Custom_cbuninst $Custom_cbstate
FunctionEnd

Function .onInit
  SetShellVarContext all
  ${If} ${RunningX64}
    SetRegView 64
    StrCpy "$INSTDIR" "$PROGRAMFILES64\${prog_name}"
  ${else}
    StrCpy "$INSTDIR" "$PROGRAMFILES\${prog_name}"
  ${endif}
  StrCpy $old_uninstpath ""
FunctionEnd