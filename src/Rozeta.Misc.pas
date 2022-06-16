unit Rozeta.Misc;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  2022.06
}

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}


interface

uses
  Windows, ShellApi,
  Classes, SysUtils, Graphics,
  GdiPlus,
  VirtualTrees,
  JPL.Strings, JPL.Conversion, JPL.Colors, JPP.Common.Procs;



const
  // Rozeta 2.0 PL: ID = 2 (September 2003)
  // Rozeta 2.0 EN: ID = 6 (July 2004)
  APP_ID = 2;

  APP_NAME = 'Rozeta';
  APP_VER_STR = '3.0';
  APP_DATE_STR = '2022.06.16';

  COL_NO = 0;
  COL_BIN = 1;
  COL_COUNT = 2;
  COL_PERCENT = 3;

  INI_SECTION_MAIN = 'MAIN';
  INI_SECTION_GRID_BINS = 'GRID_BINS';

  LANG_INI_SECTION_MAIN = 'MAIN';
  LANG_INI_SECTION_RANDOM_DIAGRAM = 'Random_diagram';
  LANG_INI_SECTION_EDIT_MEASUREMENTS = 'Edit_measurements';
  LANG_INI_SECTION_FONT_DIALOG = 'Font_dialog';
  LANG_INI_SECTION_MEASUREMENT_CONVERTER = 'Measurement_converter';
  LANG_INI_SECTION_ABOUT = 'ABOUT';

  COLLAPSED_POSTFIX = '...';

  // WNoZ UŚ, GPO-1, Analiza mezostrukturalna, praca nr 2 (2001 rok)
  ArrTestData: array[0..42] of Single =
  (
    305, 135, 320, 310, 130,  10, 355,  30, 210, 350,
    255, 230, 305, 330, 165, 145, 250, 130, 295, 155,
    330, 145, 245, 335, 320, 300, 165, 325,   0, 180,
     10, 355, 250,  50, 105,  65, 155,  50, 320, 350,
    285, 350, 145
  );


type

  TAppDebugInfo = record
    StartTimeMS: Int64;
    DrawCount: integer;
    LastDrawTime: Int64;
    ReportGenerationTime: Int64;
  end;

  TAppParams = record
    AppBits: Byte;
    AppBitsStr: string;

    AppName: string;
    AppVerStr: string;
    AppFullName: string;
    AppDateStr: string;
    AppLicense: string;
    Author: string;

    AppDir: string;
    IniFile: string;
    PresetDir: string;
    LangDir: string;
    LanguageIni: string;
    MonospaceFontName: string;

    UrlApplication: string;
    UrlGithub: string;
    UrlDonation: string;

    PresetFile_LastSession: string;
    PresetFile_DefaultMain: string;
    PresetFile_DefaultFrame: string;
    PresetFile_DefaultBackground: string;
    PresetFile_DefaultCircles: string;
    PresetFile_DefaultRadii: string;
    PresetFile_DefaultAxes: string;
    PresetFile_DefaultPies: string;
    PresetFile_DefaultSelectedBins: string;
    PresetFile_DefaultTitle: string;

    procedure Init;
  end;

  PVstBinsData = ^TVstBinsData;
  TVstBinsData = record
    No: integer;
    BinIndex: integer;
    BinStart: Word;
    BinEnd: Word;
    Count: integer;
    Percent: Single;
    Selected: Boolean;
    function RangeStr: string;
  end;

  TVirtualNodeHelper = record helper for TVirtualNode
  public
    procedure Hide;
    procedure Show;
    function Visible: Boolean;
    function Selected: Boolean;
  end;




function IsSupportedFile(const FileName: string): Boolean;
function Darker(const cl: TColor; const xPercent: integer): TColor;
procedure GoToUrl(const Url: string);


var
  AppParams: TAppParams;
  AppDebugInfo: TAppDebugInfo;



implementation


{ TAppParams }

procedure TAppParams.Init;
begin
  {$IFDEF CPUX64}
  AppBits := 64;
  {$ELSE}
  AppBits := 32;
  {$ENDIF}
  AppBitsStr := itos(AppBits);
  AppName := APP_NAME;
  AppVerStr := APP_VER_STR;
  AppDateStr := APP_DATE_STR;
  AppFullName := AppName + ' ' + AppVerStr + '  [' + AppBitsStr + '-bit]';
  AppLicense := 'Freeware (public domain), Open source';
  Author := 'Jacek Pazera';

  UrlApplication := 'https://www.pazera-software.com/products/rozeta/';
  UrlGithub := 'https://github.com/jackdp/Rozeta';
  UrlDonation := 'https://www.pazera-software.com/donation/';

  AppDir := rbs(ExtractFileDir(ParamStr(0)));
  IniFile := ChangeFileExt(ParamStr(0), '.ini');
  MonospaceFontName := GetFontName(['Fira Mono', 'Roboto Mono', 'Consolas', 'Courier New', 'Tahoma']);
  LangDir := AppDir + PathDelim + 'lang';
  LanguageIni := LangDir + PathDelim + 'English.ini';

  PresetDir := AppDir + PathDelim + 'presets';
  PresetFile_LastSession := PresetDir + '\_LastSession.json';
  PresetFile_DefaultMain := PresetDir + '\DefaultParams_Main.json';
  PresetFile_DefaultFrame := PresetDir + '\DefaultParams_Frame.json';
  PresetFile_DefaultBackground := PresetDir + '\DefaultParams_Background.json';
  PresetFile_DefaultCircles := PresetDir + '\DefaultParams_Circles.json';
  PresetFile_DefaultRadii := PresetDir + '\DefaultParams_Radii.json';
  PresetFile_DefaultAxes := PresetDir + '\DefaultParams_Axes.json';
  PresetFile_DefaultPies := PresetDir + '\DefaultParams_Pies.json';
  PresetFile_DefaultSelectedBins := PresetDir + '\DefaultParams_SelectedBins.json';
  PresetFile_DefaultTitle := PresetDir + '\DefaultParams_Title.json';
end;


function IsSupportedFile(const FileName: string): Boolean;
var
  Ext: string;
begin
  // Extensions:
  //   roz - data file (JSON)
  //   rozx - data file (zipped JSON)
  //   rozt - template file (no measurements, only diagram settings)
  Ext := LowerCase(GetFileExt(FileName, True));
  Result := (Ext = 'roz') or (Ext = 'rozx') or (Ext = 'rozt');
end;

procedure GoToUrl(const Url: string);
begin
  ShellExecute(0, 'open', PChar(Url), '', '', SW_SHOW);
end;

function Darker(const cl: TColor; const xPercent: integer): TColor;
begin
  Result := ColorSetPercentPale(cl, -xPercent);
end;



{ TVirtualNodeHelper }

procedure TVirtualNodeHelper.Hide;
begin
  if vsVisible in States then States := States - [vsVisible];
end;

procedure TVirtualNodeHelper.Show;
begin
  if not (vsVisible in States) then States := States + [vsVisible];
end;

function TVirtualNodeHelper.Visible: Boolean;
begin
  Result := vsVisible in States;
end;

function TVirtualNodeHelper.Selected: Boolean;
begin
  Result := vsSelected in States;
end;


{ TVstBinsData }

function TVstBinsData.RangeStr: string;
begin
  Result := itos(BinStart) + ' - ' + itos(BinEnd);
end;





end.

