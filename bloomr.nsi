
;The BloomR "green" Installer
;----------------------------

;; Run with:
;; makensis /V2 /dSRCDIR=brCore bloomr.nsi
;; Replace 'brCore' as needed

;Include Modern UI
!include "MUI2.nsh"




;General
;--------------------------------

  ;Product name
  Name "BloomR"

  Caption "BloomR File Extraction"
  OutFile "bloomr-setup.exe"

  ;Default installation folder
  InstallDir $DESKTOP\BloomR

  ; Icon path
  !define MUI_ICON "ahk\Compiler\bloomr.ico"

  ;Set application privileges
  RequestExecutionLevel user


;Interface Settings
;--------------------------------

  
  !define MUI_ABORTWARNING
  
  ; Licence page text
  !define lic  "I am green to your system. No system files will be touched!"

  ; Folder selection text
  !define dirh1  "Choose Destination Folder"
  !define dirh2  "Choose the folder in which to extract BloomR"
  !define dir1  "By default a $\"BloomR$\" directory will be created on your Desktop "
  !define dir2  "to extract BloomR's files. $\r$\n$\r$\nYou might want to choose "
  !define dir3  "a different folder on your USB drive. Click 'Browse...' to select it."
  InstallButtonText  "Extract files"

  ; File extraction text
  !define extr1  "Extracting BloomR files to:"
  !define extr2  "$INSTDIR"

  ; Extraction end text
  !define finish1  "Extraction is completed in BloomR folder:"
  !define finish2  "$INSTDIR"
  CompletedText "Completed. To run BloomR click on the green icon in BloomR folder" 

 
;Pages
;--------------------------------

  ; Licence page 
  !define MUI_PAGE_HEADER_TEXT "${lic}"
  !insertmacro MUI_PAGE_LICENSE "${SRCDIR}\License.txt"


  ; Folder selection page
  !define MUI_PAGE_HEADER_TEXT    "${dirh1}"
  !define MUI_PAGE_HEADER_SUBTEXT "${dirh2}"
  !define MUI_DIRECTORYPAGE_TEXT_TOP  "${dir1}${dir2}${dir3}"
  !define MUI_DIRECTORYPAGE_TEXT_DESTINATION "Folder to extract BloomR files."
  !insertmacro MUI_PAGE_DIRECTORY

  ; INSTFILES page: text before 
  !define MUI_PAGE_HEADER_TEXT    "${extr1}"
  !define MUI_PAGE_HEADER_SUBTEXT "${extr2}"

  ; INSTFILES page: text after, set dynamically in section 
  var finishhead
  var finishsub
  !define MUI_INSTFILESPAGE_FINISHHEADER_TEXT "$finishhead"
  !define MUI_INSTFILESPAGE_FINISHHEADER_SUBTEXT "$finishsub"
  !insertmacro MUI_PAGE_INSTFILES


;Languages
;--------------------------------
 
  !insertmacro MUI_LANGUAGE "English"



;Installer Sections
;--------------------------------
Section "Folder selection" SecMain

  ; User inst dir
  SetOutPath "$INSTDIR"  	    

  ; Extract files
  File /r "${SRCDIR}\*.*"
    
  ; Set completion text 
  StrCpy $finishhead  "${finish1}"
  StrCpy $finishsub   "${finish2}"



SectionEnd
