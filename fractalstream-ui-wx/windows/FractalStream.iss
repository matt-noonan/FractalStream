; Inno Setup script for the FractalStream installer. windows-build.yml runs
;
;   iscc /DAppVersion=<version> /DSourceDir=<dir> /DOutputBase=<name> /O<outdir> FractalStream.iss
;
; where <dir> holds FractalStream.exe and its DLLs, and a cli\ folder with
; fsrun.exe and the (few) DLLs it needs. The installer can put cli\ on the
; PATH; it holds only what fsrun needs, so that the app's other DLLs aren't
; found by other programs searching the PATH.

#ifndef AppVersion
  #define AppVersion "0.0.0"
#endif
#ifndef SourceDir
  #define SourceDir "FractalStream"
#endif
#ifndef OutputBase
  #define OutputBase "FractalStream-setup"
#endif

[Setup]
; AppId identifies FractalStream to Windows across versions: never change it.
AppId={{BD0D20BE-20A5-4D96-B072-C49AF6731580}
AppName=FractalStream
AppVersion={#AppVersion}
AppPublisher=Matt Noonan
AppPublisherURL=https://github.com/matt-noonan/FractalStream
DefaultDirName={autopf}\FractalStream
DisableProgramGroupPage=yes
; Install for the current user by default, without needing administrator
; rights; the user can choose an all-users install instead.
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=dialog
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
LicenseFile=..\LICENSE
SetupIconFile=FS.ico
UninstallDisplayIcon={app}\FractalStream.exe
OutputBaseFilename={#OutputBase}
Compression=lzma2
SolidCompression=yes
WizardStyle=modern
ChangesEnvironment=yes

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "addtopath"; Description: "Add the fsrun command-line tool to the PATH"; GroupDescription: "Command-line tool:"

[Files]
Source: "{#SourceDir}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs

[Icons]
Name: "{autoprograms}\FractalStream"; Filename: "{app}\FractalStream.exe"
Name: "{autodesktop}\FractalStream"; Filename: "{app}\FractalStream.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\FractalStream.exe"; Description: "{cm:LaunchProgram,FractalStream}"; Flags: nowait postinstall skipifsilent

[Code]
// The PATH lives in a different registry key for per-user and all-users
// installs.
function PathRoot: Integer;
begin
  if IsAdminInstallMode then
    Result := HKEY_LOCAL_MACHINE
  else
    Result := HKEY_CURRENT_USER;
end;

function PathKey: String;
begin
  if IsAdminInstallMode then
    Result := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
  else
    Result := 'Environment';
end;

procedure AddToPath(Dir: String);
var
  Path: String;
begin
  if not RegQueryStringValue(PathRoot, PathKey, 'Path', Path) then
    Path := '';
  if Pos(';' + Uppercase(Dir) + ';', ';' + Uppercase(Path) + ';') > 0 then
    exit;
  if (Path <> '') and (Path[Length(Path)] <> ';') then
    Path := Path + ';';
  RegWriteExpandStringValue(PathRoot, PathKey, 'Path', Path + Dir);
end;

procedure RemoveFromPath(Dir: String);
var
  Path: String;
  P: Integer;
begin
  if not RegQueryStringValue(PathRoot, PathKey, 'Path', Path) then
    exit;
  Path := ';' + Path + ';';
  P := Pos(';' + Uppercase(Dir) + ';', Uppercase(Path));
  if P = 0 then
    exit;
  Delete(Path, P, Length(Dir) + 1);
  RegWriteExpandStringValue(PathRoot, PathKey, 'Path', Copy(Path, 2, Length(Path) - 2));
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if (CurStep = ssPostInstall) and WizardIsTaskSelected('addtopath') then
    AddToPath(ExpandConstant('{app}\cli'));
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usPostUninstall then
    RemoveFromPath(ExpandConstant('{app}\cli'));
end;
