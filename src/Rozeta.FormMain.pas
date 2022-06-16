unit Rozeta.FormMain;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  Rozeta

  The main unit.

  License: public domain.

  2022.06
}

{$mode delphi}{$H+}

interface

uses

  // RTL
  Windows, Classes, SysUtils, DateUtils,

  // LCL
  Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList, StdCtrls, Clipbrd, Menus, ComCtrls, ExtDlgs,

  // GDI+
  IGDIPlus,
  GdiPlus, GdiPlusHelpers,

  // JVCL
  JvPageList, JvHint,

  // JPLib
  JPL.Strings, JPL.TStr, JPL.Colors, JPL.Conversion, JPL.MemIniFile, JPL.Win.Dialogs, JPL.Dialogs, JPL.Files, JPL.TimeLogger, JPL.DateTime,
  JPL.LangMgr, JPL.PixelConv,

  // JPPack
  JPP.Graphics, JPP.SimplePanel, JPP.GPHatchStyleComboBox, JPP.Timer, JPP.ColorComboBox, JPP.FlipPanel, JPP.BasicSpeedButton,
  JPP.PenStyleComboBox, JPP.ComboBox, JPP.Labels, JPP.CheckBox, JPP.ColorSwatch, JPP.DoubleLineLabel, JPP.Memo,

  // JPL - Rose Diagram
  JPL.RoseDiag.Diagram, JPL.RoseDiag.BinDataList, JPL.RoseDiag.DataFile,

  // Application
  Rozeta.Misc, Rozeta.FormEditData, Rozeta.FormGPFontDialog, Rozeta.FormRandomDiagram, Rozeta.FormMeasurementConverter,
  Rozeta.AppStrings, Rozeta.FormAbout,

  // Other
  attabs, VirtualTrees, HtmlView, HtmlGlobals, JPLazSpinEdit
  ;


type

  {$region ' INT - TFormMain '}
  TFormMain = class(TForm)
    actAbout: TAction;
    actGoTo_HomePage: TAction;
    actGoTo_Github: TAction;
    actGoTo_Donation: TAction;
    actReloadCurrentLanguageFile: TAction;
    actSaveTemplate: TAction;
    actOpenTemplate: TAction;
    actShowDebugInfo: TAction;
    actShowFormMeasurementConverter: TAction;
    actSavePng: TAction;
    actEsc: TAction;
    actCopyDiagramToClipboard: TAction;
    actCollapsePanels: TAction;
    actExpandPanels: TAction;
    actExpandPanel_Main: TAction;
    actExpandPanel_Background: TAction;
    actExpandPanel_Frame: TAction;
    actExpandPanel_Circles: TAction;
    actExpandPanel_Radii: TAction;
    actExpandPanel_Axes: TAction;
    actExpandPanel_Pies: TAction;
    actInvertSelectedBins: TAction;
    actExpandPanel_SelectedBins: TAction;
    actEditMeasurements: TAction;
    actExpandPanel_Description: TAction;
    actDefaultParams_Frame: TAction;
    actDefaultParams_Background: TAction;
    actDefaultParams_Main: TAction;
    actDefaultParams_Circles: TAction;
    actDefaultParams_Radii: TAction;
    actDefaultParams_Axes: TAction;
    actDefaultParams_Pies: TAction;
    actDefaultParams_SelectedBins: TAction;
    actDefaultParams_Title: TAction;
    actDefaultParams_ALL: TAction;
    actCloseFile: TAction;
    actExit: TAction;
    actSaveReport: TAction;
    actReport_CopySelection: TAction;
    actSwitchTab_Report: TAction;
    actSwitchTab_Metadata: TAction;
    actSwitchTab_Diagram: TAction;
    actRandomDiagram: TAction;
    actSaveAs: TAction;
    actSave: TAction;
    actOpen: TAction;
    actRepaintDiagram: TAction;
    actShowHideRightPanel: TAction;
    actShowHideLeftPanel: TAction;
    actUnselectAllBins: TAction;
    actSelectAllBins: TAction;
    actSelectMinBins: TAction;
    actSelectMaxBins: TAction;
    Actions: TActionList;
    btnAxes_PercentageMarkers_ChangeFont: TButton;
    btnTitle_ChangeFont: TButton;
    btnDesc_ChangeFont: TButton;
    btnShowDebugInfo: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    bvBins: TBevel;
    BtnChangeColor: TJppComboButton;
    BtnCopyColor: TJppComboButton;
    BtnPasteColor: TJppComboButton;
    btnAxes_ChangeFont: TButton;
    ccbAxes_PercentageMarkers_Color: TJppColorComboBox;
    ccbCircles_Color: TJppColorComboBox;
    ccbSelectedBin_BackgroundColor: TJppColorComboBox;
    ccbSelectedBin_HatchColor: TJppColorComboBox;
    ccbSelectedBin_LineColor: TJppColorComboBox;
    ccbRadii_Color: TJppColorComboBox;
    ccbPies_HatchColor: TJppColorComboBox;
    ccbFrame_Color: TJppColorComboBox;
    ccbAxes_Color: TJppColorComboBox;
    chAxes_PercentageMarkers: TJppCheckBox;
    chDesc_Visible: TJppCheckBox;
    chCircles_Visible: TJppCheckBox;
    chSelectedBin_SolidFill: TJppCheckBox;
    chRadii_Visible: TJppCheckBox;
    chAxes_Visible: TJppCheckBox;
    csAxes_PercentageMarkers_FontColor: TJppColorSwatch;
    csTitle_FontColor: TJppColorSwatch;
    csDesc_FontColor: TJppColorSwatch;
    dlblBinMax: TJppDoubleLineLabel;
    dlblBinMin: TJppDoubleLineLabel;
    dlblBinCount: TJppDoubleLineLabel;
    dlblRadius: TJppDoubleLineLabel;
    dlblStdDev: TJppDoubleLineLabel;
    dlblSelectedBinCount: TJppDoubleLineLabel;
    dlblSelection: TJppDoubleLineLabel;
    dlblMean: TJppDoubleLineLabel;
    dlblSumOfMeasurements: TJppDoubleLineLabel;
    dlblMeasurements: TJppDoubleLineLabel;
    dlblMeasurementsPerBin: TJppDoubleLineLabel;
    fpDesc: TJppFlipPanel;
    fpSelectedBins: TJppFlipPanel;
    fpRadii: TJppFlipPanel;
    fpBackground: TJppFlipPanel;
    fpCircles: TJppFlipPanel;
    fpPies: TJppFlipPanel;
    fpAxes: TJppFlipPanel;
    hscbSelectedBin_HatchStyle: TJppGPHatchStyleComboBox;
    htvReport: THtmlViewer;
    Images: TImageList;
    img: TImage;
    hscbPies_HatchStyle: TJppGPHatchStyleComboBox;
    ccbBackground_Color: TJppColorComboBox;
    fpMain: TJppFlipPanel;
    cbClassSize: TJppComboBox;
    ccbPies_BackgroundColor: TJppColorComboBox;
    chPies_SolidFill: TJppCheckBox;
    fpFrame: TJppFlipPanel;
    chFrame_Visible: TJppCheckBox;
    chAxes_Description: TJppCheckBox;
    csAxes_FontColor: TJppColorSwatch;
    cbAxes_CaptionType: TJppComboBox;
    AppHint: TJvHint;
    fpShortStats: TJppFlipPanel;
    cbDiagramType: TJppComboBox;
    cbMeasurementType: TJppComboBox;
    chCentralSymmetry: TJppCheckBox;
    chDrawInternalPolygonLines: TJppCheckBox;
    cbLinearMeasurementMode: TJppComboBox;
    chTitle_Visible: TJppCheckBox;
    lblAxes_PercentageMarkers_Alpha: TLabel;
    spedAxes_PercentageMarkers_Width: TJPLazSpinEdit;
    lblAxes_PercentageMarkers_Font: TJppLabel;
    lblAxes_PercentageMarkers_FontStyle: TJppLabel;
    lblAxes_PercentageMarkers_FontTitle: TJppLabel;
    lblAxes_PercentageMarkers: TJppShadowLabel;
    mnuDefaultParams_Main: TMenuItem;
    mnuGoTo_HomePage: TMenuItem;
    mnuGoTo_Github: TMenuItem;
    mnuGoTo_Donation: TMenuItem;
    dlgSaveTemplate: TSaveDialog;
    dlgOpenTemplate: TOpenDialog;
    mnuOpenTemplate: TMenuItem;
    mnuSaveTemplate: TMenuItem;
    pnAxes_PercentageMarkers_Font: TJppSimplePanel;
    mnuPresetsSep1: TMenuItem;
    mnuHelpSep2: TMenuItem;
    mnuHelpSep1: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHelp: TMenuItem;
    popSelectMaxBins: TMenuItem;
    popSelectMinBins: TMenuItem;
    popSelectAllBins: TMenuItem;
    popInvertSelectedBins: TMenuItem;
    popUnselectAllBins: TMenuItem;
    popBinsSep1: TMenuItem;
    mnuLanguage: TMenuItem;
    mnuShowHideRightPanel: TMenuItem;
    mnuShowHideLeftPanel: TMenuItem;
    mnuPanelsSep1: TMenuItem;
    mnuPresets: TMenuItem;
    mnuDefaultParams_Frame: TMenuItem;
    mnuDefaultParams_Background: TMenuItem;
    mnuDefaultParams_Circles: TMenuItem;
    mnuDefaultParams_Radii: TMenuItem;
    mnuDefaultParams_Axes: TMenuItem;
    mnuDefaultParams_Pies: TMenuItem;
    mnuDefaultParams_SelectedBins: TMenuItem;
    mnuDefaultParams_Title: TMenuItem;
    mnuDefaultParams_ALL: TMenuItem;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuSaveAs: TMenuItem;
    mnuExit: TMenuItem;
    mnuCloseFile: TMenuItem;
    mnuEditMeasurements: TMenuItem;
    mnuTabs: TMenuItem;
    mnuSwitchTab_Diagram: TMenuItem;
    mnuSwitchTab_Metadata: TMenuItem;
    mnuSwitchTab_Report: TMenuItem;
    popReport_CopySelection: TMenuItem;
    mnuSaveReport: TMenuItem;
    mnuSavePng: TMenuItem;
    dlgSavePng: TSavePictureDialog;
    popSavePng: TMenuItem;
    popRandomDiagram: TMenuItem;
    popShowFormMeasurementConverter: TMenuItem;
    mnuTools: TMenuItem;
    mnuRandomDiagram: TMenuItem;
    mnuShowFormMeasurementConverter: TMenuItem;
    popTools: TPopupMenu;
    mnuSep2: TMenuItem;
    popReport: TPopupMenu;
    dlgSaveReport: TSaveDialog;
    popVstBins: TPopupMenu;
    sepFile1: TMenuItem;
    mnuSep4: TMenuItem;
    mnuSep3: TMenuItem;
    mnuPresetsSep2: TMenuItem;
    sbtnDefaultParams_Pies: TJppBasicSpeedButton;
    sbtnDefaultParams_SelectedBins: TJppBasicSpeedButton;
    sbtnDefaultParams_Radii: TJppBasicSpeedButton;
    sbtnDefaultParams_Frame: TJppBasicSpeedButton;
    sbtnDefaultParams_Background: TJppBasicSpeedButton;
    sbtnDefaultParams_Circles: TJppBasicSpeedButton;
    sbtnDefaultParams_Main: TJppBasicSpeedButton;
    sbtnDefaultParams_Axes: TJppBasicSpeedButton;
    sbtnDefaultParams_Title: TJppBasicSpeedButton;
    sbtnDefT: TJppBasicSpeedButton;
    lblDesc: TJppShadowLabel;
    lblDesc_Font: TJppLabel;
    lblDesc_FontStyle: TJppLabel;
    lblDesc_FontTitle: TJppLabel;
    meDesc_Text: TJppMemo;
    pnT: TPanel;
    pnDesc_Font: TJppSimplePanel;
    spedAxes_PercentageMarkers_Transp: TJPLazSpinEdit;
    spedTitle_PosX: TJPLazFloatSpinEdit;
    meTitle_Text: TJppMemo;
    lblTitle_Font: TJppLabel;
    lblAxes_FontStyle: TJppLabel;
    lblTitle_FontStyle: TJppLabel;
    lblTitle_FontTitle: TJppLabel;
    lblTitle: TJppShadowLabel;
    mnuExpandPanel_Description: TMenuItem;
    popExpandPanel_Description: TMenuItem;
    pnTitle_Font: TJppSimplePanel;
    pnDiagramBg: TJppSimplePanel;
    spReport: TJvStandardPage;
    lblSelectedBin_Fill: TJppShadowLabel;
    lblSelectedBin_FillAlpha: TLabel;
    lblSelectedBin_Lines: TJppShadowLabel;
    lblSelectedBin_LinesAlpha: TLabel;
    meMeta_Author: TJppMemo;
    meMeta_Description: TJppMemo;
    popExpandPanel_SelectedBins: TMenuItem;
    mnuExpandPanel_SelectedBins: TMenuItem;
    popRepaintDiagram: TMenuItem;
    meMeta_Subject: TJppMemo;
    lblBins: TJppShadowLabel;
    MainMenu: TMainMenu;
    mnuPanels: TMenuItem;
    mnuExpandPanel_Main: TMenuItem;
    mnuExpandPanel_Background: TMenuItem;
    mnuExpandPanel_Frame: TMenuItem;
    mnuExpandPanel_Circles: TMenuItem;
    mnuExpandPanel_Radii: TMenuItem;
    mnuExpandPanel_Axes: TMenuItem;
    mnuExpandPanel_Pies: TMenuItem;
    mnuExpandPanels: TMenuItem;
    mnuCollapsePanels: TMenuItem;
    popShowHideLeftPanel: TMenuItem;
    popShowHideRightPanel: TMenuItem;
    pscbSelectedBin_LineStyle: TJppPenStyleComboBox;
    popImgSep1: TMenuItem;
    mnuPanelsSep2: TMenuItem;
    PageList: TJvPageList;
    spDiagram: TJvStandardPage;
    spedSelectedBin_FillTransp: TJPLazSpinEdit;
    spedSelectedBin_LineTransp: TJPLazSpinEdit;
    spedSelectedBin_LineWidth: TJPLazFloatSpinEdit;
    spedDesc_PosX: TJPLazFloatSpinEdit;
    spedTitle_PosY: TJPLazFloatSpinEdit;
    spedDesc_PosY: TJPLazFloatSpinEdit;
    splLeft: TSplitter;
    spMetadata: TJvStandardPage;
    Tabs: TATTabs;
    tmStart: TJppTimer;
    lblAxes_FontTitle: TJppLabel;
    pnAxes_Font: TJppSimplePanel;
    lblAxes_Font: TJppLabel;
    lblAxes_Alpha: TLabel;
    lblAxes_Lines: TJppShadowLabel;
    lblAxes_Captions: TJppShadowLabel;
    popExpandPanel_Pies: TMenuItem;
    popExpandPanels: TMenuItem;
    popCollapsePanels: TMenuItem;
    popExpandPanel_Main: TMenuItem;
    popExpandPanel_Background: TMenuItem;
    popExpandPanel_Frame: TMenuItem;
    popExpandPanel_Circles: TMenuItem;
    popExpandPanel_Radii: TMenuItem;
    popExpandPanel_Axes: TMenuItem;
    popPanelsSep1: TMenuItem;
    popPanels: TPopupMenu;
    pscbAxes_Style: TJppPenStyleComboBox;
    spedCircles_Count: TJPLazSpinEdit;
    lblCircles_Alpha: TLabel;
    lblRadii_Alpha: TLabel;
    pscbCircles_Style: TJppPenStyleComboBox;
    pscbRadii_Style: TJppPenStyleComboBox;
    spedRadii_Transp: TJPLazSpinEdit;
    spedAxes_Transp: TJPLazSpinEdit;
    spedRadii_Width: TJPLazFloatSpinEdit;
    spedFrame_Transp: TJPLazSpinEdit;
    lblFrame_Alpha: TLabel;
    lblPies_LinesAlpha: TLabel;
    pscbFrame_Style: TJppPenStyleComboBox;
    spedCircles_Transp: TJPLazSpinEdit;
    spedCircles_Width: TJPLazFloatSpinEdit;
    spedPies_LineTransp: TJPLazSpinEdit;
    spedPies_LineWidth: TJPLazFloatSpinEdit;
    ccbPies_LineColor: TJppColorComboBox;
    pscbPies_LineStyle: TJppPenStyleComboBox;
    lblPies_Fill: TJppShadowLabel;
    lblPies_FillAlpha: TLabel;
    lblPies_Lines: TJppShadowLabel;
    popCopyDiagramToClipboard: TMenuItem;
    popImg: TPopupMenu;
    spedPies_FillTransp: TJPLazSpinEdit;
    lblMarginPix: TLabel;
    spedBackground_Transp: TJPLazSpinEdit;
    lblBackground_Alpha: TLabel;
    lblRadiusPix: TLabel;
    spedFrame_Width: TJPLazFloatSpinEdit;
    spedAxes_Width: TJPLazFloatSpinEdit;
    spedRadiusMM: TJPLazSpinEdit;
    sboxDiagParams: TScrollBox;
    spedMarginMM: TJPLazSpinEdit;
    tmUpdateDiag: TJppTimer;
    pnLeft: TJppSimplePanel;
    pnRight: TJppSimplePanel;
    sboxImg: TScrollBox;
    tbMain: TToolBar;
    tbBins: TToolBar;
    tbtnShowHideLeftPanel: TToolButton;
    tbtnShowHideRightPanel: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    tbtnSave: TToolButton;
    tbtnEditMeasurements: TToolButton;
    tbtnSep2: TToolButton;
    tbtnSep1: TToolButton;
    tbtnOpen: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    vstBins: TVirtualStringTree;
    procedure actAboutExecute(Sender: TObject);
    procedure actCloseFileExecute(Sender: TObject);
    procedure actCollapsePanelsExecute(Sender: TObject);
    procedure actCopyDiagramToClipboardExecute(Sender: TObject);
    procedure actDefaultParams_ALLExecute(Sender: TObject);
    procedure actDefaultParams_AxesExecute(Sender: TObject);
    procedure actDefaultParams_BackgroundExecute(Sender: TObject);
    procedure actDefaultParams_CirclesExecute(Sender: TObject);
    procedure actDefaultParams_FrameExecute(Sender: TObject);
    procedure actDefaultParams_MainExecute(Sender: TObject);
    procedure actDefaultParams_PiesExecute(Sender: TObject);
    procedure actDefaultParams_RadiiExecute(Sender: TObject);
    procedure actDefaultParams_SelectedBinsExecute(Sender: TObject);
    procedure actDefaultParams_TitleExecute(Sender: TObject);
    procedure actEditMeasurementsExecute(Sender: TObject);
    procedure actEscExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExpandPanel_DescriptionExecute(Sender: TObject);
    procedure actExpandPanelsExecute(Sender: TObject);
    procedure actExpandPanel_AxesExecute(Sender: TObject);
    procedure actExpandPanel_BackgroundExecute(Sender: TObject);
    procedure actExpandPanel_CirclesExecute(Sender: TObject);
    procedure actExpandPanel_FrameExecute(Sender: TObject);
    procedure actExpandPanel_PiesExecute(Sender: TObject);
    procedure actExpandPanel_RadiiExecute(Sender: TObject);
    procedure actExpandPanel_SelectedBinsExecute(Sender: TObject);
    procedure actExpandPanel_MainExecute(Sender: TObject);
    procedure actGoTo_DonationExecute(Sender: TObject);
    procedure actGoTo_GithubExecute(Sender: TObject);
    procedure actGoTo_HomePageExecute(Sender: TObject);
    procedure actInvertSelectedBinsExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actOpenTemplateExecute(Sender: TObject);
    procedure actRandomDiagramExecute(Sender: TObject);
    procedure actReloadCurrentLanguageFileExecute(Sender: TObject);
    procedure actRepaintDiagramExecute(Sender: TObject);
    procedure actReport_CopySelectionExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSavePngExecute(Sender: TObject);
    procedure actSaveReportExecute(Sender: TObject);
    procedure actSaveTemplateExecute(Sender: TObject);
    procedure actSelectAllBinsExecute(Sender: TObject);
    procedure actSelectMaxBinsExecute(Sender: TObject);
    procedure actSelectMinBinsExecute(Sender: TObject);
    procedure actShowDebugInfoExecute(Sender: TObject);
    procedure actShowFormMeasurementConverterExecute(Sender: TObject);
    procedure actShowHideLeftPanelExecute(Sender: TObject);
    procedure actShowHideRightPanelExecute(Sender: TObject);
    procedure actSwitchTab_DiagramExecute(Sender: TObject);
    procedure actSwitchTab_MetadataExecute(Sender: TObject);
    procedure actSwitchTab_ReportExecute(Sender: TObject);
    procedure actUnselectAllBinsExecute(Sender: TObject);
    procedure btnAxes_PercentageMarkers_ChangeFontClick(Sender: TObject);
    procedure btnDesc_ChangeFontClick(Sender: TObject);
    procedure btnAxes_ChangeFontClick(Sender: TObject);
    procedure btnTitle_ChangeFontClick(Sender: TObject);
    procedure cbClassSizeChange(Sender: TObject);
    procedure cbDiagramTypeChange(Sender: TObject);
    procedure cbLinearMeasurementModeChange(Sender: TObject);
    procedure cbMeasurementTypeChange(Sender: TObject);
    procedure chCentralSymmetryChange(Sender: TObject);
    procedure chDesc_VisibleChange(Sender: TObject);
    procedure chTitle_VisibleChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure popReportPopup(Sender: TObject);
    procedure PrepareControls;
    procedure InitControls;
    procedure SaveSettingsToIni;
    procedure LoadSettingsFromIni;
    procedure DrawDiagram;
    procedure RepaintDiagram(Sender: TObject);
    procedure RepaintDiagramAndModify(Sender: TObject);
    procedure sboxImgPaint(Sender: TObject);
    procedure spedMarginMMChange(Sender: TObject);
    procedure TabsTabChanged(Sender: TObject);
    procedure tmStartTimer(Sender: TObject);
    procedure tmUpdateDiagTimer(Sender: TObject);
    procedure PrepareDataList;
    procedure LoadDataFile(const FileName: string; const LoadMeasurements, LoadMetadata, UpdateBins: Boolean);
    function LoadPresetFile(const FileName: string; const UpdateBins: Boolean): Boolean;
    procedure UpdateControlsFromRoseDiagramObject;
    procedure SetComboClassSizeValue(const ClassSize: Byte);
    procedure ExpandOrCollapseFlipPanels(const bExpand: Boolean);
    procedure ExpandOnePanel(fp: TJppFlipPanel);
    procedure FillFontParams(var gpfp: TGPFontParams; RoseText: TRoseDiagramText); overload;
    procedure FillFontParams(var gpfp: TGPFontParams; RoseFont: TRoseDiagramFont); overload;
    procedure ApplyFontParams(const gpfp: TGPFontParams; RoseText: TRoseDiagramText); overload;
    procedure ApplyFontParams(const gpfp: TGPFontParams; RoseFont: TRoseDiagramFont); overload;
    procedure UpdateStatsControls;
    procedure PerformSaveDataFile(const FileName: string; SaveMetadata, SaveMeasurements: Boolean);
    function PerformLoadDataFile(const FileName: string; LoadMetadata, LoadMeasurements, IsPreset: Boolean): Boolean;
    procedure UpdateAppTitleAndCaption;
    procedure UpdateReport;
    function GetHtmlReportStr(BodyBgColor: string = '#F8F8F8'; PngFileName: string = ''): string;
    procedure UpdateInitialDirs(Dir: string = '');
    procedure Modify;
    procedure Unmodify;
    function QuerySaveDlg: TModalResult;
    function TrySaveDiagramWithQuery: Boolean;
    procedure SaveDiagramAsPngFile(const PngFileName: string);

    procedure InitLang;
    procedure SetLang;
    procedure UpdateLangMenu;
    procedure ChangeLanguage(Sender: TObject);

    procedure vstBins_Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstBins_BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstBins_PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vstBins_GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstBins_GetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    function vstBins_GetNodeData(Node: PVirtualNode; var DataPtr: PVstBinsData): Boolean;
    procedure vstBins_FillGrid;
    procedure vstBins_UpdateSelectedRowsFromBinDataList;
    procedure vstBins_UpdateSelectionStats;
    procedure vstBins_CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstBins_HeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
  private
    FUpdatingControls: Boolean;
    FRoseDiagram: TRoseDiagram;
    FCurrentFile: string;
    FModified: Boolean;
  public
    property RoseDiagram: TRoseDiagram read FRoseDiagram;
  end;
  {$endregion INT - TFormMain}



var
  FormMain: TFormMain;



implementation


{$R *.lfm}


{$region '   Create & Close   '}
procedure TFormMain.FormCreate(Sender: TObject);
var
  Param1: string;
begin
  {$IFNDEF DEBUG}
  btnShowDebugInfo.Hide;
  {$ENDIF}

  AppParams.Init;
  if not DirectoryExists(AppParams.PresetDir) then CreateDir(AppParams.PresetDir);

  Param1 := ParamStr(1);
  FCurrentFile := '';
  dlgOpenTemplate.InitialDir := AppParams.PresetDir;
  dlgSaveTemplate.InitialDir := dlgOpenTemplate.InitialDir;

  UpdateAppTitleAndCaption;


  Constraints.MinWidth := 720;
  Constraints.MinHeight := 400;

  RegisterHtHints; // Activate HTML hints (TJvHint)

  FRoseDiagram := TRoseDiagram.Create;

  PrepareControls;
  LoadSettingsFromIni;

  InitLang;
  InitLangMgr;
  PrepareModuleStrings_Main;
  UpdateLangMenu;


  if FileExists(AppParams.PresetFile_LastSession) then
    LoadPresetFile(AppParams.PresetFile_LastSession, False);

  if (Param1 <> '') and (FileExists(Param1)) then PerformLoadDataFile(Param1, True, True, False)
  else
  begin
    {$IFDEF DEBUG}
    PerformLoadDataFile(AppParams.PresetFile_LastSession, False, False, True);
    FRoseDiagram.BinDataList.ProcessMeasurementArray(ArrTestData);
    {$ENDIF}
  end;

  PrepareDataList;
  UpdateControlsFromRoseDiagramObject;
  tmUpdateDiag.Start;
  tmStart.Start;
  FModified := False;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  bCanContinue: Boolean;
begin
  if FModified then bCanContinue := TrySaveDiagramWithQuery
  else bCanContinue := True;

  if not bCanContinue then
  begin
    CloseAction := caNone;
    Exit;
  end;

  tmUpdateDiag.Stop;

  try
    if DirectoryExists(AppParams.PresetDir) then
      PerformSaveDataFile(AppParams.PresetFile_LastSession, False, False);
  except
    // Bez exceptions przy kończeniu programu.
  end;

  FRoseDiagram.Free;
  SaveSettingsToIni;
  if Assigned(LangMgr) then LangMgr.Free;
end;
{$endregion Create & Close}

procedure TFormMain.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.popReportPopup(Sender: TObject);
begin
  // To czasami nie działa! W Delphi tak samo.
  //actReport_CopySelection.Enabled := htvReport.SelLength > 0;
end;



{$region '   PrepareControls   '}
procedure TFormMain.PrepareControls;
var
  sbtn: TJppBasicSpeedButton;
  ccb: TJppColorComboBox;
  btn: TJppComboButton;
  gpfp: TGPFontParams;

  procedure SetColorComboParams(ccb: TJppColorComboBox);
  var
    ccbSrc: TJppColorComboBox;
    cl: TColor;
  begin
    cl := ccb.SelectedColor;
    ccbSrc := ccbBackground_Color;

    ccb.BeginUpdate;
    ccb.Items.Assign(ccbSrc.Items);
    ccb.EndUpdate;
    ccb.ButtonsAlignment := ccbSrc.ButtonsAlignment;
    ccb.ItemHeight := ccbSrc.ItemHeight;
    ccb.Appearance.Assign(ccbSrc.Appearance);

    ccb.ButtonChangeColor.Appearance.Assign(ccbSrc.ButtonChangeColor.Appearance);
    ccb.ButtonChangeColor.Caption := ccbSrc.ButtonChangeColor.Caption;
    ccb.ButtonChangeColor.Hint := ccbSrc.ButtonChangeColor.Hint;
    ccb.ButtonChangeColor.ShowHint := ccbSrc.ButtonChangeColor.ShowHint;
    if Assigned(ccbSrc.ButtonChangeColor.PngImage) then ccb.ButtonChangeColor.PngImage.Assign(ccbSrc.ButtonChangeColor.PngImage);

    ccb.ButtonCopyColor.Appearance.Assign(ccbSrc.ButtonChangeColor.Appearance);
    ccb.ButtonCopyColor.Caption := ccbSrc.ButtonCopyColor.Caption;
    ccb.ButtonCopyColor.Hint := ccbSrc.ButtonCopyColor.Hint;
    ccb.ButtonCopyColor.ShowHint := ccbSrc.ButtonCopyColor.ShowHint;
    if Assigned(ccbSrc.ButtonCopyColor.PngImage) then ccb.ButtonCopyColor.PngImage.Assign(ccbSrc.ButtonCopyColor.PngImage);

    ccb.ButtonPasteColor.Appearance.Assign(ccbSrc.ButtonChangeColor.Appearance);
    ccb.ButtonPasteColor.Caption := ccbSrc.ButtonPasteColor.Caption;
    ccb.ButtonPasteColor.Hint := ccbSrc.ButtonPasteColor.Hint;
    ccb.ButtonPasteColor.ShowHint := ccbSrc.ButtonPasteColor.ShowHint;
    if Assigned(ccbSrc.ButtonPasteColor.PngImage) then ccb.ButtonPasteColor.PngImage.Assign(ccbSrc.ButtonPasteColor.PngImage);

    ccb.ButtonsSpacing := ccbSrc.ButtonsSpacing;
    ccb.DropDownCount := ccbSrc.DropDownCount;

    ccb.ItemIndex := -1;
    ccb.SelectedColor := cl;
  end;

  procedure SetSbtDefaultParams(sbtn: TJppBasicSpeedButton);
  begin
    sbtn.Height := sbtnDefT.Height;
    sbtn.Font.Name := sbtnDefT.Font.Name;
    sbtn.Font.Size := sbtnDefT.Font.Size;
    sbtn.Appearance.Assign(sbtnDefT.Appearance);
  end;

begin
  FUpdatingControls := True;
  try
    pnDiagramBg.Align := alClient;

    PageList.Align := alClient;
    PageList.ActivePageIndex := 0;

    sboxImg.BorderStyle := bsNone;
    sboxImg.Align := alClient;

    sboxDiagParams.BorderStyle := bsNone;
    sboxDiagParams.Align := alClient;

    htvReport.Align := alClient;

    bvBins.Height := 2;

    dlblBinCount.RightCaptionFont.Name := AppParams.MonospaceFontName;
    dlblMeasurements.RightCaptionFont.Name := AppParams.MonospaceFontName;
    dlblMeasurementsPerBin.RightCaptionFont.Name := AppParams.MonospaceFontName;
    dlblSelectedBinCount.RightCaptionFont.Name := AppParams.MonospaceFontName;
    dlblSelection.RightCaptionFont.Name := AppParams.MonospaceFontName;
    dlblBinMax.RightCaptionFont.Name := AppParams.MonospaceFontName;
    dlblBinMin.RightCaptionFont.Name := AppParams.MonospaceFontName;
    dlblSumOfMeasurements.RightCaptionFont.Name := AppParams.MonospaceFontName;
    dlblRadius.RightCaptionFont.Name := AppParams.MonospaceFontName;

    vstBins.Align := alClient;


    spedRadiusMM.MinValue := RD_MIN_DIAGRAM_WIDTH_MM;
    spedRadiusMM.MaxValue := RD_MAX_DIAGRAM_WIDTH_MM;
    spedMarginMM.MinValue := RD_MIN_DIAGRAM_MARGIN_MM;
    spedMarginMM.MaxValue := RD_MAX_DIAGRAM_MARGIN_MM;


    fpShortStats.Appearance.Assign(fpMain.Appearance);
    fpShortStats.Header.Assign(fpMain.Header, False, False);
    fpShortStats.Header.Images := Images;
    fpShortStats.Header.ImageIndexCollapsed := 13;
    fpShortStats.Header.ImageIndexExpanded := 13;


    SetSbtDefaultParams(sbtnDefaultParams_Main);


    // Diagram background
    fpBackground.Appearance.Assign(fpMain.Appearance);
    fpBackground.Header.Assign(fpMain.Header, False, False);
    SetSbtDefaultParams(sbtnDefaultParams_Background);

    ccb := ccbBackground_Color;
    sbtn := ccb.ButtonChangeColor;
    sbtn.Caption := '...';

    ccb.ButtonCopyColor.Appearance.Assign(sbtn.Appearance);
    ccb.ButtonCopyColor.Caption := '';
    ccb.ButtonPasteColor.Appearance.Assign(sbtn.Appearance);
    ccb.ButtonPasteColor.Caption := '';


    // Frame
    fpFrame.Appearance.Assign(fpMain.Appearance);
    fpFrame.Header.Assign(fpMain.Header, False, False);
    SetSbtDefaultParams(sbtnDefaultParams_Frame);
    SetColorComboParams(ccbFrame_Color);
    spedFrame_Width.MaxValue := RD_MAX_LINE_WIDTH;


    // Circles
    fpCircles.Appearance.Assign(fpMain.Appearance);
    fpCircles.Header.Assign(fpMain.Header, False, False);
    SetSbtDefaultParams(sbtnDefaultParams_Circles);
    SetColorComboParams(ccbCircles_Color);
    spedCircles_Width.MaxValue := RD_MAX_LINE_WIDTH;
    spedCircles_Count.MaxValue := RD_MAX_CIRCLES_COUNT;


    // Radii
    fpRadii.Appearance.Assign(fpMain.Appearance);
    fpRadii.Header.Assign(fpMain.Header, False, False);
    SetSbtDefaultParams(sbtnDefaultParams_Radii);
    SetColorComboParams(ccbRadii_Color);
    spedRadii_Width.MaxValue := RD_MAX_LINE_WIDTH;

    // Axes
    fpAxes.Appearance.Assign(fpMain.Appearance);
    fpAxes.Header.Assign(fpMain.Header, False, False);
    SetSbtDefaultParams(sbtnDefaultParams_Axes);
    SetColorComboParams(ccbAxes_Color);
    spedAxes_Width.MaxValue := RD_MAX_LINE_WIDTH;

    FillFontParams(gpfp{%H-}, FRoseDiagram.Axes.Text.Font);
    lblAxes_Font.Caption := gpfp.InfoStr(False);
    lblAxes_FontStyle.Caption := gpfp.FontStyleStr;
    lblAxes_Font.AnchoredControls.UpdateAllControlsPos;
    csAxes_FontColor.SelectedColor := gpfp.Color;

    FillFontParams(gpfp, FRoseDiagram.Axes.PercentageMarkers.Font);
    lblAxes_PercentageMarkers_Font.Caption := gpfp.InfoStr(False);
    lblAxes_PercentageMarkers_FontStyle.Caption := gpfp.FontStyleStr;
    lblAxes_PercentageMarkers_Font.AnchoredControls.UpdateAllControlsPos;
    csAxes_PercentageMarkers_FontColor.SelectedColor := gpfp.Color;

    SetColorComboParams(ccbAxes_PercentageMarkers_Color);
    spedAxes_PercentageMarkers_Width.MinValue := RD_MIN_MARKER_WIDTH;
    spedAxes_PercentageMarkers_Width.MaxValue := RD_MAX_MARKER_WIDTH;

    // Pies
    fpPies.Appearance.Assign(fpMain.Appearance);
    fpPies.Header.Assign(fpMain.Header, False, False);
    SetSbtDefaultParams(sbtnDefaultParams_Pies);
    SetColorComboParams(ccbPies_BackgroundColor);
    SetColorComboParams(ccbPies_HatchColor);
    SetColorComboParams(ccbPies_LineColor);
    spedPies_LineWidth.MaxValue := RD_MAX_LINE_WIDTH;

    // Selected bin
    fpSelectedBins.Appearance.Assign(fpMain.Appearance);
    fpSelectedBins.Header.Assign(fpMain.Header, False, False);
    SetSbtDefaultParams(sbtnDefaultParams_SelectedBins);
    SetColorComboParams(ccbSelectedBin_BackgroundColor);
    SetColorComboParams(ccbSelectedBin_HatchColor);
    SetColorComboParams(ccbSelectedBin_LineColor);
    spedSelectedBin_LineWidth.MaxValue := RD_MAX_LINE_WIDTH;


    // Title & Description
    fpDesc.Appearance.Assign(fpMain.Appearance);
    fpDesc.Header.Assign(fpMain.Header, False, False);
    SetSbtDefaultParams(sbtnDefaultParams_Title);

    FillFontParams(gpfp{%H-}, FRoseDiagram.Title);
    lblTitle_Font.Caption := gpfp.InfoStr(False);
    lblTitle_FontStyle.Caption := gpfp.FontStyleStr;
    lblTitle_Font.AnchoredControls.UpdateAllControlsPos;
    csTitle_FontColor.SelectedColor := gpfp.Color;

    meTitle_Text.Text := FRoseDiagram.Title.Text;
    meTitle_Text.Font.Name := AppParams.MonospaceFontName;
    spedTitle_PosX.Value := FRoseDiagram.Title.PosX;
    spedTitle_PosY.Value := FRoseDiagram.Title.PosY;
    spedTitle_PosX.MaxValue := RD_MAX_POS;
    spedTitle_PosY.MaxValue := RD_MAX_POS;

    FillFontParams(gpfp{%H-}, FRoseDiagram.Description);
    lblDesc_Font.Caption := gpfp.InfoStr(False);
    lblDesc_FontStyle.Caption := gpfp.FontStyleStr;
    lblDesc_Font.AnchoredControls.UpdateAllControlsPos;
    csDesc_FontColor.SelectedColor := gpfp.Color;

    meDesc_Text.Text := FRoseDiagram.Description.Text;
    meDesc_Text.Font.Name := AppParams.MonospaceFontName;
    spedDesc_PosX.Value := FRoseDiagram.Description.PosX;
    spedDesc_PosY.Value := FRoseDiagram.Description.PosY;
    spedDesc_PosX.MaxValue := RD_MAX_POS;
    spedDesc_PosY.MaxValue := RD_MAX_POS;



    // Metadata
    meMeta_Subject.Clear;
    meMeta_Subject.Font.Name := AppParams.MonospaceFontName;
    meMeta_Author.Clear;
    meMeta_Author.Font.Name := AppParams.MonospaceFontName;
    meMeta_Description.Clear;
    meMeta_Description.Font.Name := AppParams.MonospaceFontName;


  finally
    FUpdatingControls := False;
  end;
end;
{$endregion PrepareControls}

{$region ' InitControls '}
procedure TFormMain.InitControls;
var
  b: Boolean;
begin
  b := cbDiagramType.ItemIndex = 1; // polygon
  chDrawInternalPolygonLines.Enabled := b;

  b := cbMeasurementType.ItemIndex = 1; // 180
  chCentralSymmetry.Enabled := b;
  cbLinearMeasurementMode.Enabled := b;

  // Frame
  b := chFrame_Visible.Checked;
  ccbFrame_Color.Enabled := b;
  spedFrame_Width.Enabled := b;
  pscbFrame_Style.Enabled := b;
  spedFrame_Transp.Enabled := b;
  lblFrame_Alpha.Enabled := b;

  // Circles
  b := chCircles_Visible.Checked;
  spedCircles_Count.Enabled := b;
  ccbCircles_Color.Enabled := b;
  spedCircles_Width.Enabled := b;
  pscbCircles_Style.Enabled := b;
  spedCircles_Transp.Enabled := b;
  lblCircles_Alpha.Enabled := b;

  // Radii
  b := chRadii_Visible.Checked;
  ccbRadii_Color.Enabled := b;
  spedRadii_Width.Enabled := b;
  pscbRadii_Style.Enabled := b;
  spedRadii_Transp.Enabled := b;
  lblRadii_Alpha.Enabled := b;

  // Axes
  b := chAxes_Visible.Checked;
  ccbAxes_Color.Enabled := b;
  spedAxes_Width.Enabled := b;
  pscbAxes_Style.Enabled := b;
  spedAxes_Transp.Enabled := b;
  lblAxes_Alpha.Enabled := b;
  b := chAxes_Description.Checked;
  pnAxes_Font.Enabled := b;
  cbAxes_CaptionType.Enabled := b;
  b := chAxes_PercentageMarkers.Checked;
  ccbAxes_PercentageMarkers_Color.Enabled := b;
  pnAxes_PercentageMarkers_Font.Enabled := b;
  spedAxes_PercentageMarkers_Width.Enabled := b;
  spedAxes_PercentageMarkers_Transp.Enabled := b;
  lblAxes_PercentageMarkers_Alpha.Enabled := b;

  // Pies
  b := chPies_SolidFill.Checked;
  hscbPies_HatchStyle.Enabled := not b;
  ccbPies_HatchColor.Enabled := not b;

  // Selected bins
  b := chSelectedBin_SolidFill.Checked;
  hscbSelectedBin_HatchStyle.Enabled := not b;
  ccbSelectedBin_HatchColor.Enabled := not b;

  // Title
  b := chTitle_Visible.Checked;
  pnTitle_Font.Enabled := b;
  meTitle_Text.Enabled := b;
  spedTitle_PosX.Enabled := b;
  spedTitle_PosY.Enabled := b;

  // Description
  b := chDesc_Visible.Checked;
  pnDesc_Font.Enabled := b;
  meDesc_Text.Enabled := b;
  spedDesc_PosX.Enabled := b;
  spedDesc_PosY.Enabled := b;
end;
{$endregion InitControls}

procedure TFormMain.PrepareDataList;
var
  x: Byte;
  i: integer;
  vd: PVstBinsData;
  Node: PVirtualNode;
  FixMode: TLinearMeasurementFixMode;
  s: string;
begin
  FRoseDiagram.BinDataList.AutoUpdateBins := False;

  if cbLinearMeasurementMode.ItemIndex = 0 then FixMode := lmfmNormalize else FixMode := lmfmRemove;
  FRoseDiagram.BinDataList.LinearMeasurementFixMode := FixMode;

  s := Trim(cbClassSize.Text);
  s := TrimFromEnd(s, TSpecialChars.Degree);
  if not TryStrToByte(s, x) then x := 10;
  FRoseDiagram.BinDataList.ClassSize := x;

  FRoseDiagram.BinDataList.Azimuths := cbMeasurementType.ItemIndex = 0;
  FRoseDiagram.CentralSymmetry := chCentralSymmetry.Checked;

  FRoseDiagram.BinDataList.UpdateBins;
  FRoseDiagram.BinDataList.AutoUpdateBins := True;

  vstBins_FillGrid;

  tmUpdateDiag.Restart;
end;

function TFormMain.QuerySaveDlg: TModalResult;
var
  s: string;
begin
  s := lsMain.GetString('SaveQuery=The current diagram has been modified.\nDo you want to save the changes?');
  Result := MessageDlg(AppParams.AppName, s, mtConfirmation, [mbYes, mbNo, mbCancel], '');
end;

function TFormMain.TrySaveDiagramWithQuery: Boolean;
begin
  if (FCurrentFile <> '') and (not FModified) then Exit(True);

  if FModified then
    case QuerySaveDlg of
      mrNo: Exit(True);
      mrCancel: Exit(False);
    end;

  if FCurrentFile = '' then
  begin
    if not dlgSave.Execute then Exit(False);
    FCurrentFile := dlgSave.FileName;
  end;

  PerformSaveDataFile(FCurrentFile, True, True);
  UpdateAppTitleAndCaption;
  Unmodify;
  Result := True;
end;


procedure TFormMain.actOpenExecute(Sender: TObject);
var
  bCanContinue: Boolean;
begin
  if FModified then bCanContinue := TrySaveDiagramWithQuery
  else bCanContinue := True;
  if not bCanContinue then Exit;

  if not dlgOpen.Execute then Exit;

  Unmodify;
  PerformLoadDataFile(dlgOpen.FileName, True, True, False);
end;

procedure TFormMain.actCloseFileExecute(Sender: TObject);
var
  bCanContinue: Boolean;
begin
  if FModified then bCanContinue := TrySaveDiagramWithQuery
  else bCanContinue := True;
  if not bCanContinue then Exit;

  FCurrentFile := '';
  meMeta_Author.Clear;
  meMeta_Subject.Clear;
  meMeta_Description.Clear;
  Unmodify;
  UpdateAppTitleAndCaption;
  FRoseDiagram.BinDataList.ClearParams(True);
  PrepareDataList;
  DrawDiagram;
end;

procedure TFormMain.actOpenTemplateExecute(Sender: TObject);
begin
  if not dlgOpenTemplate.Execute then Exit;
  Modify;
  PerformLoadDataFile(dlgOpenTemplate.FileName, True, False, True);
end;

procedure TFormMain.actSaveTemplateExecute(Sender: TObject);
begin
  if not dlgSaveTemplate.Execute then Exit;
  PerformSaveDataFile(dlgSaveTemplate.FileName, True, False);
end;

procedure TFormMain.actSaveExecute(Sender: TObject);
begin
  if FCurrentFile <> '' then
  begin
    PerformSaveDataFile(FCurrentFile, True, True);
    Unmodify;
  end
  else
    actSaveAs.Execute;
end;

procedure TFormMain.actSaveAsExecute(Sender: TObject);
begin
  if not dlgSave.Execute then Exit;
  PerformSaveDataFile(dlgSave.FileName, True, True);
  FCurrentFile := dlgSave.FileName;
  UpdateAppTitleAndCaption;
  Unmodify;
end;

procedure TFormMain.SaveDiagramAsPngFile(const PngFileName: string);
var
  Png: TPortableNetworkGraphic;
begin
  Png := TPortableNetworkGraphic.Create;
  try
    Png.Assign(img.Picture.Bitmap);
    Png.SaveToFile(PngFileName);
  finally
    Png.Free;
  end;
end;

procedure TFormMain.actSavePngExecute(Sender: TObject);
begin
  if not dlgSavePng.Execute then Exit;
  SaveDiagramAsPngFile(dlgSavePng.FileName);
end;

procedure TFormMain.actSaveReportExecute(Sender: TObject);
var
  s: string;
  PngFileName: string;
begin
  if not dlgSaveReport.Execute then Exit;

  PngFileName := ChangeFileExt(dlgSaveReport.FileName, '.png');
  PngFileName := GetIncFileName(PngFileName, '_', 3);
  PngFileName := ExtractFileName(PngFileName);

  s := GetHtmlReportStr('#FFFFFF', PngFileName);
  SaveStringToFile(dlgSaveReport.FileName, s, TEncoding.UTF8);
  SaveDiagramAsPngFile(PngFileName);
end;

procedure TFormMain.actAboutExecute(Sender: TObject);
begin
  FormAbout.Show;
end;

{$region '   DrawDiagram   '}
procedure TFormMain.DrawDiagram;
var
  Png: TPortableNetworkGraphic;
  DiagWidthPix, DiagHeightPix : integer;
  b180, bSymm: Boolean;
  sAlpha: string;
  PixelConv: IPixelConv;

  function MmToPix(xMillimeters: Single): Single;
  begin
    Result := PixelConv.MmToPixelsX(xMillimeters);
  end;

begin
  TTimeLogger.StartLog;

  Png := TPortableNetworkGraphic.Create;
  try
    PixelConv := TPixelConv.Create(Png.Canvas);

    bSymm := chCentralSymmetry.Checked;
    FRoseDiagram.CentralSymmetry := bSymm;


    b180 := cbMeasurementType.ItemIndex = 1;
    if b180 then FRoseDiagram.MeasurementType := mt180 else FRoseDiagram.MeasurementType := mt360;


    // Main
    FRoseDiagram.MarginMM := spedMarginMM.Value;
    FRoseDiagram.RadiusMM := spedRadiusMM.Value;

    DiagWidthPix := Round(MmToPix((FRoseDiagram.RadiusMM * 2))) + Round(MmToPix((FRoseDiagram.MarginMM * 2)));
    DiagHeightPix := DiagWidthPix;
    if b180 and (not bSymm) then DiagWidthPix := DiagWidthPix - Round(MmToPix(FRoseDiagram.RadiusMM));

    FRoseDiagram.DrawInternalPolygonLines := chDrawInternalPolygonLines.Checked;


    Png.SetSize(DiagWidthPix, DiagHeightPix);
    SetTransparentPng(Png);



    if cbDiagramType.ItemIndex = 1 then FRoseDiagram.DiagramType := rdtPolygon
    else FRoseDiagram.DiagramType := rdtRose;


    // Background
    FRoseDiagram.Background.Color := ccbBackground_Color.SelectedColor;
    FRoseDiagram.Background.Transparency := spedBackground_Transp.Value;
    FRoseDiagram.Background.SolidFill := True;

    // Frame
    FRoseDiagram.Frame.Visible := chFrame_Visible.Checked;
    FRoseDiagram.Frame.Color := ccbFrame_Color.SelectedColor;
    FRoseDiagram.Frame.Width := spedFrame_Width.Value;
    FRoseDiagram.Frame.Style := pscbFrame_Style.Selected;
    FRoseDiagram.Frame.Transparency := spedFrame_Transp.Value;

    // Circles
    FRoseDiagram.Circles.Visible := chCircles_Visible.Checked;
    FRoseDiagram.CirclesCount := spedCircles_Count.Value;
    FRoseDiagram.Circles.Color := ccbCircles_Color.SelectedColor;
    FRoseDiagram.Circles.Width := spedCircles_Width.Value;
    FRoseDiagram.Circles.Style := pscbCircles_Style.Selected;
    FRoseDiagram.Circles.Transparency := spedCircles_Transp.Value;

    // Radii
    FRoseDiagram.Radii.Visible := chRadii_Visible.Checked;
    FRoseDiagram.Radii.Color := ccbRadii_Color.SelectedColor;
    FRoseDiagram.Radii.Width := spedRadii_Width.Value;
    FRoseDiagram.Radii.Style := pscbRadii_Style.Selected;
    FRoseDiagram.Radii.Transparency := spedRadii_Transp.Value;

    // Axes
    FRoseDiagram.Axes.Line.Visible := chAxes_Visible.Checked;
    FRoseDiagram.Axes.Line.Color := ccbAxes_Color.SelectedColor;
    FRoseDiagram.Axes.Line.Width := spedAxes_Width.Value;
    FRoseDiagram.Axes.Line.Style := pscbAxes_Style.Selected;
    FRoseDiagram.Axes.Line.Transparency := spedAxes_Transp.Value;
    FRoseDiagram.Axes.Text.Visible := chAxes_Description.Checked;
    if cbAxes_CaptionType.ItemIndex = 0 then FRoseDiagram.Axes.CaptionType := actDegrees
    else FRoseDiagram.Axes.CaptionType := actSymbols;
    FRoseDiagram.Axes.PercentageMarkers.Visible := chAxes_PercentageMarkers.Checked;
    FRoseDiagram.Axes.PercentageMarkers.MarkerColor := ccbAxes_PercentageMarkers_Color.SelectedColor;
    FRoseDiagram.Axes.PercentageMarkers.MarkerWidth := spedAxes_PercentageMarkers_Width.Value;
    FRoseDiagram.Axes.PercentageMarkers.MarkerTransparency := spedAxes_PercentageMarkers_Transp.Value;

    // Pies
    FRoseDiagram.PieFill.Color := ccbPies_BackgroundColor.SelectedColor;
    FRoseDiagram.PieFill.Transparency := spedPies_FillTransp.Value;
    FRoseDiagram.PieFill.SolidFill := chPies_SolidFill.Checked;
    FRoseDiagram.PieFill.HatchStyle := TGPHatchStyle(hscbPies_HatchStyle.ItemIndex);
    FRoseDiagram.PieFill.HatchColor := ccbPies_HatchColor.SelectedColor;

    FRoseDiagram.PieLine.Color := ccbPies_LineColor.SelectedColor;
    FRoseDiagram.PieLine.Width := spedPies_LineWidth.Value;
    FRoseDiagram.PieLine.Style := pscbPies_LineStyle.Selected;
    FRoseDiagram.PieLine.Transparency := spedPies_LineTransp.Value;

    // Selected bin
    FRoseDiagram.SelectedBinFill.Color := ccbSelectedBin_BackgroundColor.SelectedColor;
    FRoseDiagram.SelectedBinFill.Transparency := spedSelectedBin_FillTransp.Value;
    FRoseDiagram.SelectedBinFill.SolidFill := chSelectedBin_SolidFill.Checked;
    FRoseDiagram.SelectedBinFill.HatchStyle := TGPHatchStyle(hscbSelectedBin_HatchStyle.ItemIndex);
    FRoseDiagram.SelectedBinFill.HatchColor := ccbSelectedBin_HatchColor.SelectedColor;

    FRoseDiagram.SelectedBinLine.Color := ccbSelectedBin_LineColor.SelectedColor;
    FRoseDiagram.SelectedBinLine.Width := spedSelectedBin_LineWidth.Value;
    FRoseDiagram.SelectedBinLine.Style := pscbSelectedBin_LineStyle.Selected;
    FRoseDiagram.SelectedBinLine.Transparency := spedSelectedBin_LineTransp.Value;

    // Title & description
    FRoseDiagram.Title.Visible := chTitle_Visible.Checked;
    FRoseDiagram.Title.Text := meTitle_Text.Lines.Text;
    FRoseDiagram.Title.PosX := spedTitle_PosX.Value;
    FRoseDiagram.Title.PosY := spedTitle_PosY.Value;

    FRoseDiagram.Description.Visible := chDesc_Visible.Checked;
    FRoseDiagram.Description.Text := meDesc_Text.Lines.Text;
    FRoseDiagram.Description.PosX := spedDesc_PosX.Value;
    FRoseDiagram.Description.PosY := spedDesc_PosY.Value;



    /////////////////////////////////////////////////
    FRoseDiagram.PaintOnCanvas(Png.Canvas);
    /////////////////////////////////////////////////



    img.Width := Png.Width;
    img.Height := Png.Height;
    img.Picture.Assign(Png);


  finally
    Png.Free;
  end;

  sAlpha := lsMain.GetString('AlphaChannel=Alpha:') + ' ';
  lblBackground_Alpha.Caption := sAlpha + itos(TranspToAlpha(spedBackground_Transp.Value));
  lblFrame_Alpha.Caption := sAlpha + itos(TranspToAlpha(spedFrame_Transp.Value));
  lblCircles_Alpha.Caption := sAlpha + itos(TranspToAlpha(spedCircles_Transp.Value));
  lblRadii_Alpha.Caption := sAlpha + itos(TranspToAlpha(spedRadii_Transp.Value));
  lblAxes_Alpha.Caption := sAlpha + itos(TranspToAlpha(spedAxes_Transp.Value));
  lblAxes_PercentageMarkers_Alpha.Caption := sAlpha + itos(TranspToAlpha(spedAxes_PercentageMarkers_Transp.Value));
  lblPies_FillAlpha.Caption := sAlpha + itos(TranspToAlpha(spedPies_FillTransp.Value));
  lblPies_LinesAlpha.Caption := sAlpha + itos(TranspToAlpha(spedPies_LineTransp.Value));
  lblSelectedBin_FillAlpha.Caption := sAlpha + itos(TranspToAlpha(spedSelectedBin_FillTransp.Value));
  lblSelectedBin_LinesAlpha.Caption := sAlpha + itos(TranspToAlpha(spedSelectedBin_LineTransp.Value));

  lblRadiusPix.Caption := FormatFloat('0.0 pix', spedRadiusMM.Value * PixelConv.PixelsPerMillimeterX);
  lblMarginPix.Caption := FormatFloat('0.0 pix', spedMarginMM.Value * PixelConv.PixelsPerMillimeterX);

  UpdateStatsControls;


  TTimeLogger.EndLog;
  AppDebugInfo.LastDrawTime := TTimeLogger.ElapsedTime;
  Inc(AppDebugInfo.DrawCount);

end;
{$endregion DrawDiagram}

procedure TFormMain.UpdateStatsControls;
var
  bs: TBinStats;
  sBins: string;
begin
  FRoseDiagram.BinDataList.CalculateStats;
  bs := FRoseDiagram.BinDataList.Stats;

  dlblBinCount.RightCaption := itos(bs.BinCount);

  dlblMeasurements.RightCaption := ' ' + IntToStrEx(bs.MeasurementCount) + ' ';

  sBins := lsMain.GetString('dlblBinMinMax_BinsSuffix=bins');
  dlblBinMax.RightCaption := IntToStrEx(bs.MaxMeasurementsInBin) + ' / ' + itos(bs.BinsWithMaxMeasurements) + ' ' + sBins;

  dlblBinMin.RightCaption := IntToStrEx(bs.MinMeasurementsInBin) + ' / ' + itos(bs.BinsWithMinMeasurements) + ' ' + sBins;

  dlblMeasurementsPerBin.RightCaption := InsertNumSep( ftos(bs.MeasurementsPerBin, 2) );

  dlblSumOfMeasurements.RightCaption := InsertNumSep( ftos(bs.SumOfMeasurements, 2) ) + string(DEG_SIGN);

  dlblRadius.RightCaption :=
    IntToStrEx(FRoseDiagram.RadiusMM) + ' mm (' +
    ftos(bs.MaxMeasurementsInBin_Percentage, 2) + '%)';

  dlblMean.RightCaption := ftos(bs.Mean, 2);
  dlblStdDev.RightCaption := ftos(bs.StdDev, 2);
end;

procedure TFormMain.PerformSaveDataFile(const FileName: string; SaveMetadata, SaveMeasurements: Boolean);
var
  rdf: TRoseDataFile;
  Options: TRoseDataFileOptions;
  bZip: Boolean;
begin
  rdf := TRoseDataFile.Create(FRoseDiagram);
  try
    Options := {%H-}Options.SetAll;

    if SaveMetadata then
    begin
      FRoseDiagram.Metadata.Subject := meMeta_Subject.Text;
      FRoseDiagram.Metadata.Author := meMeta_Author.Text;
      FRoseDiagram.Metadata.Description := meMeta_Description.Text;
    end
    else
      Exclude(Options, rdfoMetadata);

    if not SaveMeasurements then Exclude(Options, rdfoMeasurements);

    bZip := LowerCase(GetFileExt(FileName, True)) = 'rozx';
    rdf.SaveToFile(FileName, Options, AppParams.AppFullName, bZip);

  finally
    rdf.Free;
  end;
end;

function TFormMain.PerformLoadDataFile(const FileName: string; LoadMetadata, LoadMeasurements, IsPreset: Boolean): Boolean;
var
  rdf: TRoseDataFile;
  Options: TRoseDataFileOptions;
begin
  Result := False;
  if not FileExists(FileName) then Exit;

  if not IsPreset then dlgSave.FileName := ExtractFileName(FileName);

  rdf := TRoseDataFile.Create(FRoseDiagram);
  try

    Options := {%H-}Options.SetAll;
    if not LoadMetadata then Exclude(Options, rdfoMetadata);
    if not LoadMeasurements then Exclude(Options, rdfoMeasurements);

    Result := rdf.LoadFromFile(FileName, Options);

    //if LoadMeasurements then GArrM := rdf.Measurements;

  finally
    rdf.Free;
  end;

  if (not IsPreset) and Result then
  begin
    FCurrentFile := ExpandFileName(FileName);
    UpdateAppTitleAndCaption;
    UpdateInitialDirs;
    dlgSaveReport.FileName := ChangeFileExt(FileName, '.html');
  end;

  UpdateControlsFromRoseDiagramObject;

  PrepareDataList;
  vstBins_FillGrid;

  InitControls;
  tmUpdateDiag.Restart;
end;

procedure TFormMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  FileName, Ext: string;
  bTemplate: Boolean;
begin
  if Length(FileNames) = 0 then Exit;
  FileName := FileNames[0];
  if not IsSupportedFile(FileName) then Exit;
  Ext := LowerCase(GetFileExt(FileName, True));
  bTemplate := Ext = 'rozt';

  if not bTemplate then actCloseFile.Execute;
  PerformLoadDataFile(FileName, True, not bTemplate, bTemplate);
end;

procedure TFormMain.UpdateAppTitleAndCaption;
var
  Prefix: string;
begin
  if FModified then Prefix := '*' else Prefix := '';

  if FCurrentFile = '' then
  begin
    Application.Title := AppParams.AppFullName;
    Caption := AppParams.AppFullName;
  end
  else
  begin
    Application.Title := Prefix + BaseFileName(FCurrentFile) + ' - ' + AppParams.AppFullName;
    Caption := Prefix + FCurrentFile + ' - ' + AppParams.AppFullName;
  end;
end;

procedure TFormMain.UpdateReport;
var
  s: string;
begin
  s := GetHtmlReportStr;
  htvReport.LoadFromString(WideString(s));
  //SaveStringToFile('report.html', s, TEncoding.UTF8);
end;

{$region ' GetHtmlReportStr '}
function TFormMain.GetHtmlReportStr(BodyBgColor: string = '#F8F8F8'; PngFileName: string = ''): string;
type
  TLangStr = record
    File_Title, File_Name, File_Size, File_Created, File_Modified: string;
    Metadata_Title, Metadata_Subject, Metadata_Author, Metadata_Description: string;
    Stats_Title, Stats_NumberOfMeasurements, Stats_NumberOfBins, Stats_SumOfMeasurements, Stats_MeasurementsPerBin: string;
    Stats_MaxInBin, Stats_BinsWithMax, Stats_MinInBin, Stats_BinsWithMin, Stats_Radius, Stats_Average, Stats_StdDev: string;
    Bins_Title, Bins_No, Bins_Range, Bins_Count, Bins_Percent, Bins_Average, Bins_StdDev, Bins_Measurements: string;
  end;
const
  VAL_SEP = '; ';
var
  LangStr: TLangStr;
  sl: TStringList;
  i: integer;
  BinData: TBinData;
  Stats: TBinStats;
  fir: TFileInfoRec;
  s, sMetaAuthor, sMetaSubject, sMetaDescription: string;
  ATimeLogger: TClassTimeLogger;

  function FixStr(const s: string): string;
  begin
    Result := TStr.ReplaceAll(s, '<', '&lt;');
    Result := TStr.ReplaceAll(Result, '>', '&gt;');
    Result := TStr.ReplaceAll(Result, ENDL, '<br>');
  end;

begin
  Result := '';
  if not Assigned(lsMain) then Exit;

  ATimeLogger := TClassTimeLogger.Create;
  try
    ATimeLogger.StartLog;

    LangStr.File_Title := lsMain.GetString('Report_File_Title=File info');
    LangStr.File_Name := lsMain.GetString('Report_File_Name=File:');
    LangStr.File_Size := lsMain.GetString('Report_File_Size=Size:');
    LangStr.File_Created := lsMain.GetString('Report_File_Created=Created:');
    LangStr.File_Modified := lsMain.GetString('Report_File_Modified=Modifed:');
    LangStr.Metadata_Title := lsMain.GetString('Report_Metadata_Title=Metadata');
    LangStr.Metadata_Subject := lsMain.GetString('Report_Metadata_Subject=Subject');
    LangStr.Metadata_Author := lsMain.GetString('Report_Metadata_Author=Author');
    LangStr.Metadata_Description := lsMain.GetString('Report_Metadata_Description=Description');
    LangStr.Stats_Title := lsMain.GetString('Report_Stats_Title=Stats');
    LangStr.Stats_NumberOfMeasurements := lsMain.GetString('Report_Stats_NumberOfMeasurements=Number of measurements:');
    LangStr.Stats_NumberOfBins := lsMain.GetString('Report_Stats_NumberOfBins=Number of bins:');
    LangStr.Stats_SumOfMeasurements := lsMain.GetString('Report_Stats_SumOfMeasurements=Sum of measurements:');
    LangStr.Stats_MeasurementsPerBin := lsMain.GetString('Report_Stats_MeasurementsPerBin=Measurements per bin:');
    LangStr.Stats_MaxInBin := lsMain.GetString('Report_Stats_MaxInBin=Maximum number of measurements in the bin(s):');
    LangStr.Stats_BinsWithMax := lsMain.GetString('Report_Stats_BinsWithMax=Number of bins with the maximum number of measurements:');
    LangStr.Stats_MinInBin := lsMain.GetString('Report_Stats_MinInBin=Minimum number of measurements in the bin(s):');
    LangStr.Stats_BinsWithMin := lsMain.GetString('Report_Stats_BinsWithMin=Number of bins with the minimum number of measurements:');
    LangStr.Stats_Radius := lsMain.GetString('Report_Stats_Radius=Radius:');
    LangStr.Stats_Average := lsMain.GetString('Report_Stats_Average=Average measurement:');
    LangStr.Stats_StdDev := lsMain.GetString('Report_Stats_StdDev=Standard deviation:');
    LangStr.Bins_Title := lsMain.GetString('Report_Bins_Title=Bins');
    LangStr.Bins_No := lsMain.GetString('Report_Bins_No=No');
    LangStr.Bins_Range := lsMain.GetString('Report_Bins_Range=Range');
    LangStr.Bins_Count := lsMain.GetString('Report_Bins_Count=Count');
    LangStr.Bins_Percent := lsMain.GetString('Report_Bins_Percent=Percent');
    LangStr.Bins_Average := lsMain.GetString('Report_Bins_Average=Average');
    LangSTr.Bins_StdDev := lsMain.GetString('Report_Bins_StdDev=Std. dev.');
    LangStr.Bins_Measurements := lsMain.GetString('Report_Bins_Measurements=Measurements');



    Stats := FRoseDiagram.BinDataList.Stats;

    sl := TStringList.Create;
    try

      sl.Add('<html lang="' + lsMain.GetString('Report_HtmlLangID=en') + '">');
      sl.Add('<head>');
      sl.Add('<title>' + Self.Caption + '</title>');
      sl.Add('<meta http-equiv="Content-Type" content="text/html; charset=utf-8">');

      sl.Add('<style>');
      sl.Add(
        '  body {font-family: "Open Sans", "Segoe UI", Verdana, Tahoma; font-size: 10pt; background-color: ' + BodyBgColor + '; ' +
        'margin-left: 20px; margin-right: 20px;}'
      );
      sl.Add('  h1 {font-family: Verdana; font-size: 13pt; font-weight: bold; margin: 10px 0px 8px 0px;}');
      sl.Add('  h2 {font-family: Verdana; font-size: 10pt; font-weight: bold; margin: 10px 0px 6px 0px;}');
      sl.Add('  h3 {font-family: Verdana; font-size: 9pt; font-weight: bold; margin: 10px 0px 3px 0px;}');
      sl.Add('  table {border-collapse: collapse; margin-top: 12px;}');
      sl.Add('  table, th, td {border: 1px solid #c0c0c0;}');
      sl.Add('  th {font-family: Verdana; font-size: 9pt; font-weight: bold; padding: 3px; background-color: #e2e2e2;}');
      sl.Add('  td {padding: 2px 4px 2px 4px; background-color: white;}');
      sl.Add('  pre {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; margin: 2px 0px 2px 0px;}');
      sl.Add('  .mono {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 10pt;}');
      sl.Add('  .tdNo {font-size: 8pt; text-align: center;}');
      sl.Add('  .tdRange {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; text-align: center; background-color: #F9E6F0;}');
      sl.Add('  .tdCountMin {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; text-align: center; background-color: #D3EDE4;}');
      sl.Add('  .tdCountMax {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; text-align: center; background-color: #FDF9C4; font-weight: bold;}');
      sl.Add('  .tdCount {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; text-align: center;}');
      sl.Add('  .tdPercent {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; text-align: center;}');
      sl.Add('  .tdAvg {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; text-align: center;}');
      sl.Add('  .tdStdDev {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; text-align: center;}');
      sl.Add('  .tdValues {font-family: "Fira Mono", "Roboto Mono", Consolas; font-size: 9pt; color: #1070BE; text-align: left;}');
      sl.Add('</style>');

      sl.Add('</head>');
      sl.Add('');

      sl.Add('<body>');
      sl.Add('');


      {$region ' Report - File info '}
      if FCurrentFile <> '' then
      begin
        sl.Add('');
        sl.Add('<!-- FILE_INFO -->');
        sl.Add('<h1>' + LangStr.File_Title + '</h1>');

        sl.Add('<p>');
        sl.Add(LangStr.File_Name + ' <b>' + FCurrentFile + '</b>');
        fir.ReadFileInfo(FCurrentFile);
        if fir.Exists then
        begin
          sl.Add('<br><br>');
          if fir.Size < 1024 then s := IntToStrEx(fir.Size) + ' bytes'
          else s := IntToStrEx(fir.Size) + ' bytes (' + GetFileSizeString(fir.Size) + ')';
          sl.Add(LangStr.File_Size + ' <span class="mono">' + s + '</span><br>');
          sl.Add(LangStr.File_Created + ' <span class="mono">' + DateTimeToStr(fir.CreationTime) + '</span><br>');
          sl.Add(LangStr.File_Modified + ' <span class="mono">' + DateTimeToStr(fir.LastWriteTime) + '</span><br>');
        end;
        sl.Add('</p>');

        sl.Add('<hr>');
      end;
      {$endregion Report - File info}


      {$region ' Report - Metadata '}
      sMetaAuthor := Trim(meMeta_Author.Text);
      sMetaSubject := Trim(meMeta_Subject.Text);
      sMetaDescription := Trim(meMeta_Description.Text);
      if (sMetaAuthor <> '') or (sMetaSubject <> '') or (sMetaDescription <> '') then
      begin
        sl.Add('');
        sl.Add('<!-- METADATA -->');
        sl.Add('<h1>' + LangStr.Metadata_Title + '</h1>');

        if sMetaSubject <> '' then
        begin
          sl.Add('<h2>' + LangStr.Metadata_Subject + '</h2>');
          sl.Add(FixStr(sMetaSubject));
        end;

        if sMetaAuthor <> '' then
        begin
          sl.Add('<h2>' + LangStr.Metadata_Author + '</h2>');
          sl.Add(FixStr(sMetaAuthor));
        end;

        if sMetaDescription <> '' then
        begin
          sl.Add('<h2>' + LangStr.Metadata_Description + '</h2>');
          sl.Add(FixStr(sMetaDescription));
        end;


        sl.Add('<hr>');
      end;
      {$endregion Report - Metadata}

      {$region ' Diagram (PNG) '}
      if PngFileName <> '' then
      begin
        sl.Add('');
        sl.Add('<img src="' + PngFileName + '">');
        sl.Add('');
        sl.Add('<hr>');
      end;
      {$endregion PNG}


      {$region ' Report - Stats '}
      sl.Add('');
      sl.Add('<!-- STATS -->');
      sl.Add('<h1>' + LangStr.Stats_Title + '</h1>');

      sl.Add('<p>');
      sl.Add(LangStr.Stats_NumberOfMeasurements + ' <span class="mono"><b>' + IntToStrEx(Stats.MeasurementCount) + '</b></span><br>');
      sl.Add(LangStr.Stats_NumberOfBins + ' <span class="mono">' + IntToStrEx(Stats.BinCount) + '</span><br>');
      sl.Add(LangStr.Stats_SumOfMeasurements + ' <span class="mono">' + InsertNumSep(ftos(Stats.SumOfMeasurements)) + string(TStr.CharDegree) + '</span><br>');
      sl.Add(LangStr.Stats_MeasurementsPerBin + ' <span class="mono">' + ftos(Stats.MeasurementsPerBin) + '</span><br>');
      sl.Add('<br>');
      sl.Add(LangStr.Stats_MaxInBin + ' <span class="mono">' + IntToStrEx(Stats.MaxMeasurementsInBin) + '</span><br>');
      sl.Add(LangStr.Stats_BinsWithMax + ' <span class="mono">' + IntToStrEx(Stats.BinsWithMaxMeasurements) + '</span><br>');
      sl.Add('<br>');
      sl.Add(LangStr.Stats_MinInBin + ' <span class="mono">' + IntToStrEx(Stats.MinMeasurementsInBin) + '</span><br>');
      sl.Add(LangStr.Stats_BinsWithMin + ' <span class="mono">' + IntToStrEx(Stats.BinsWithMinMeasurements) + '</span><br>');
      sl.Add('<br>');
      sl.Add(LangStr.Stats_Radius + ' <span class="mono">' + dlblRadius.RightCaption + '</span><br>');
      sl.Add(LangStr.Stats_Average + ' <span class="mono">' + ftos(Stats.Mean) + string(TStr.CharDegree) + '</span><br>');
      sl.Add(LangStr.Stats_StdDev + ' <span class="mono">' + ftos(Stats.StdDev) + '</span><br>');
      sl.Add('</p>');

      sl.Add('<hr>');
      {$endregion Report - Stats}


      {$region ' Report - Bins '}
      sl.Add('');
      sl.Add('<!-- BINS -->');
      sl.Add('<h1>' + LangStr.Bins_Title + '</h1>');

      sl.Add('<table>');
      sl.Add(
        '<th>' + LangStr.Bins_No + '</th>' +
        '<th>' + LangStr.Bins_Range + '</th>' +
        '<th>' + LangStr.Bins_Count + '</th>' +
        '<th>' + LangStr.Bins_Percent + '</th>' +
        '<th>' + LangStr.Bins_Average + '</th>' +
        '<th>' + LangStr.Bins_StdDev + '</th>' +
        '<th>' + LangStr.Bins_Measurements + '</th>'
      );

      for i := 0 to FRoseDiagram.BinDataList.Count - 1 do
      begin
        BinData := FRoseDiagram.BinDataList[i];

        sl.Add('<tr>');

        // No
        sl.Add('  <td class="tdNo">' + itos(i + 1) + '</td>');

        // Bin (range)
        if i = 0 then s := '&lt;' else s := '(';
        sl.Add('  <td class="tdRange">' + s + itos(BinData.StartValue) + '-' + itos(BinData.EndValue) + '&gt;</td>');

        // Count
        if BinData.MeasurementCount = Stats.MaxMeasurementsInBin then s := 'tdCountMax'
        else if BinData.MeasurementCount = Stats.MinMeasurementsInBin then s := 'tdCountMin'
        else s := 'tdCount';
        sl.Add('  <td class="' + s + '">' + IntToStrEx(BinData.MeasurementCount) + '</td>');

        // Percent
        sl.Add('  <td class="tdPercent">' + ftos(BinData.Percent) + '%</td>');

        // Average
        sl.Add('  <td class="tdAvg">' + ftos(BinData.Mean) + '</td>');

        // StdDev
        sl.Add('  <td class="tdStdDev">' + ftos(BinData.StdDev) + '</td>');

        // Values
        sl.Add('  <td class="tdValues">' + BinData.ValuesAsStr(VAL_SEP) + '</td>');

        sl.Add('</tr>');
      end;

      sl.Add('</table>');

      {$endregion Report - Bins}


      sl.Add('');
      sl.Add('</body>');
      sl.Add('</html>');

      Result := sl.Text;
    finally
      sl.Free;
    end;

  finally
    ATimeLogger.EndLog;
    AppDebugInfo.ReportGenerationTime := ATimeLogger.ElapsedTimeMs;
    ATimeLogger.Free;
  end;
end;
{$endregion GetHtmlReportStr}

procedure TFormMain.UpdateInitialDirs(Dir: string = '');
begin
  if Dir = '' then
    if FCurrentFile <> '' then Dir := ExtractFileDir(FCurrentFile)
    else Dir := ExtractFileDir(ParamStr(0));

  //if FCurrentFile = '' then Exit;
  dlgOpen.InitialDir := Dir;
  dlgSave.InitialDir := Dir;
  dlgSaveReport.InitialDir := Dir;
  dlgSavePng.InitialDir := Dir;
end;



{$region '         Language         '}

{$region ' InitLang '}
procedure TFormMain.InitLang;
var
  fName: string;
begin
  fName := '';
  if not FileExists(AppParams.IniFile) then
  begin

    case SysLocale.PriLangID of
      LANG_POLISH: fName := 'Polish.ini';
      LANG_ALBANIAN: fName := 'Albanian.ini';
      LANG_ARABIC: fName := 'Arabic.ini';
      LANG_BELARUSIAN: fName := 'Belarusian.ini';
      LANG_BULGARIAN: fName := 'Bulgarian.ini';
      LANG_CHINESE: fName := 'Chinese_Simplified.ini';
      LANG_CROATIAN: fName := 'Croatian.ini'; // LANG_SERBIAN = LANG_CROATIAN = $1A
      LANG_CZECH: fName := 'Czech.ini';
      LANG_DANISH: fName := 'Danish.ini';
      LANG_DUTCH: fName := 'Dutch.ini';
      LANG_ESTONIAN: fName := 'Estonian.ini';
      LANG_FINNISH: fName := 'Finnish.ini';
      LANG_FRENCH: fName := 'French.ini';
      LANG_GERMAN: fName := 'German.ini';
      LANG_GREEK: fName := 'Greek.ini';
      LANG_HEBREW: fName := 'Hebrew.ini';
      LANG_HUNGARIAN: fName := 'Hungarian.ini';
      LANG_ICELANDIC: fName := 'Icelandic.ini';
      LANG_ITALIAN: fName := 'Italian.ini';
      LANG_JAPANESE: fName := 'Japanese.ini';
      LANG_KOREAN: fName := 'Korean.ini';
      LANG_LATVIAN: fName := 'Latvian.ini';
      LANG_LITHUANIAN: fName := 'Lithuanian.ini';
      LANG_NORWEGIAN: fName := 'Norwegian.ini';
      LANG_PORTUGUESE: fName := 'Portuguese_BR.ini';
      LANG_ROMANIAN: fName := 'Romanian.ini';
      LANG_RUSSIAN: fName := 'Russian.ini';
      //LANG_SERBIAN: fName := 'Serbian.ini';  LANG_SERBIAN = LANG_CROATIAN
      LANG_SLOVAK: fName := 'Slovak.ini';
      LANG_SLOVENIAN: fName := 'Slovenian.ini';
      LANG_SPANISH: fName := 'Spanish.ini';
      LANG_SWEDISH: fName := 'Swedish.ini';
      LANG_THAI: fName := 'Thai.ini';
      LANG_TURKISH: fName := 'Turkish.ini';
      LANG_UKRAINIAN: fName := 'Ukrainian.ini';
      LANG_VIETNAMESE: fName := 'Vietnamese.ini';
      else fName := 'English.ini';
    end;

    if fName <> '' then fName := AppParams.LangDir + PathDelim + fName;
    if FileExists(fName) then AppParams.LanguageIni := fName;

  end;

end;
{$endregion InitLang}

{$region ' SetLang '}
procedure TFormMain.SetLang;
var
  LangStr_LineVisible, LangStr_LineColor, LangStr_LineWidth, LangStr_LineStyle, LangStr_LineTransparency: string;
  LangStr_FillBackgroundColor, LangStr_FillTransparency, LangStr_FillSolid, LangStr_FillHatchStyle, LangStr_FillHatchColor: string;
  LangStr_TextVisible, LangStr_TextText, LangStr_TextPosX, LangStr_TextPosY: string;
  gpfp: TGPFontParams;

  procedure SetComboItems(cb: TJppComboBox; ItemsArray: array of string);
  var
    x, i: integer;
  begin
    x := cb.ItemIndex;
    cb.Items.Clear;
    for i := 0 to High(ItemsArray) do
      cb.Items.Add(ItemsArray[i]);
    if x < cb.Items.Count then cb.ItemIndex := x;
  end;

  procedure SetColorComboButtonsHints(ccb: TJppColorComboBox);
  begin
    ccb.ButtonChangeColor.Hint := lsMain.GetString('ChangeColor=Change color...');
    ccb.ButtonCopyColor.Hint := lsMain.GetString('CopyColor=Copy color');
    ccb.ButtonPasteColor.Hint := lsMain.GetString('PasteColor=Paste color');
  end;

begin
  if not Assigned(lsMain) then PrepareModuleStrings_Main;

  (Tabs.Tabs.Items[0] as TATTabData).TabCaption := lsMain.GetString('TabDiagram=Diagram');
  (Tabs.Tabs.Items[1] as TATTabData).TabCaption := lsMain.GetString('TabMetadata=Metadata');
  (Tabs.Tabs.Items[2] as TATTabData).TabCaption := lsMain.GetString('TabReport=Report');

  TGPFontParams.BoldStr := lsMain.GetString('FontStyle_Bold=Bold');
  TGPFontParams.ItalicStr := lsMain.GetString('FontStyle_Italic=Italic');
  TGPFontParams.UnderlineStr := lsMain.GetString('FontStyle_Underline=Underline');
  TGPFontParams.StrikeoutStr := lsMain.GetString('FontStyle_Strikeout=Strikeout');

  LangStr_LineVisible := lsMain.GetString('Line_Visible=Visible');
  LangStr_LineColor := lsMain.GetString('Line_Color=Color:');
  LangStr_LineWidth := lsMain.GetString('Line_Width=Width:');
  LangStr_LineStyle := lsMain.GetString('Line_Style=Style:');
  LangStr_LineTransparency := lsMain.GetString('Line_Transparency=Transparency:');

  LangStr_FillBackgroundColor := lsMain.GetString('Fill_BackgroundColor=Background color:');
  LangStr_FillTransparency := lsMain.GetString('Fill_Transparency=Transparency:');
  LangStr_FillSolid := lsMain.GetString('Fill_Solid=Solid fill');
  LangStr_FillHatchStyle := lsMain.GetString('Fill_HatchStyle=Hatch style:');
  LangStr_FillHatchColor := lsMain.GetString('Fill_HatchColor=Hatch color:');

  LangStr_TextVisible := lsMain.GetString('Text_Visible=Visible');
  LangStr_TextText := lsMain.GetString('Text_Text=Text:');
  LangStr_TextPosX := lsMain.GetString('Text_PosX=X:');
  LangStr_TextPosY := lsMain.GetString('Text_PosY=Y:');


  fpShortStats.Header.CaptionCollapsed := fpShortStats.Header.Caption + COLLAPSED_POSTFIX;


  lblBins.Caption := lsMain.GetString('TitleLabel_Bins=Bins');
  vstBins.Header.Columns[0].Text := lsMain.GetString('GridBins_ColNo=No');
  vstBins.Header.Columns[1].Text := lsMain.GetString('GridBins_ColBin=Bin');
  vstBins.Header.Columns[2].Text := lsMain.GetString('GridBins_ColCount=Count');
  popSelectMaxBins.Caption := actSelectMaxBins.Hint;
  popSelectMinBins.Caption := actSelectMinBins.Hint;


  sbtnDefaultParams_Main.Hint := actDefaultParams_Main.Caption;
  sbtnDefaultParams_Background.Hint := actDefaultParams_Background.Caption;
  sbtnDefaultParams_Frame.Hint := actDefaultParams_Frame.Caption;
  sbtnDefaultParams_Circles.Hint := actDefaultParams_Circles.Caption;
  sbtnDefaultParams_Radii.Hint := actDefaultParams_Radii.Caption;
  sbtnDefaultParams_Axes.Hint := actDefaultParams_Axes.Caption;
  sbtnDefaultParams_Pies.Hint := actDefaultParams_Pies.Caption;
  sbtnDefaultParams_SelectedBins.Hint := actDefaultParams_SelectedBins.Caption;
  sbtnDefaultParams_Title.Hint := actDefaultParams_Title.Caption;



  // Flip panel - Main
  fpMain.Header.CaptionCollapsed := fpMain.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_Main.Caption := lsMain.GetString('ButtonDefaultParams=Default');

  SetComboItems(cbDiagramType, [
   lsMain.GetString('DiagramType_Rose=Rose'),
   lsMain.GetString('DiagramType_Polygon=Polygon')
  ]);

  SetComboItems(cbMeasurementType, [
    lsMain.GetString('MeasurementType_Azimuths=Azimuths (0°-360°)'),
    lsMain.GetString('MeasurementType_Strikes=Strikes (0°-180°)')
  ]);

  SetComboItems(cbLinearMeasurementMode, [
    lsMain.GetString('LinearMeasurementMode_Normalize180=Normalize to 180'),
    lsMain.GetString('LinearMeasurementMode_Ignore=Ignore')
  ]);


  // Flip panel - Background
  fpBackground.Header.CaptionCollapsed := fpBackground.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_Background.Caption := lsMain.GetString('ButtonDefaultParams=Default');
  SetColorComboButtonsHints(ccbBackground_Color);
  ccbBackground_Color.BoundLabel.Caption := LangStr_FillBackgroundColor;
  spedBackground_Transp.BoundLabel.Caption := LangStr_FillTransparency;


  // Flip panel - Frame
  fpFrame.Header.CaptionCollapsed := fpFrame.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_Frame.Caption := lsMain.GetString('ButtonDefaultParams=Default');
  chFrame_Visible.Caption := LangStr_LineVisible;
  SetColorComboButtonsHints(ccbFrame_Color);
  ccbFrame_Color.BoundLabel.Caption := LangStr_LineColor;
  spedFrame_Width.BoundLabel.Caption := LangStr_LineWidth;
  pscbFrame_Style.BoundLabel.Caption := LangStr_LineStyle;
  spedFrame_Transp.BoundLabel.Caption := LangStr_LineTransparency;


  // Flip panel - Circles
  fpCircles.Header.CaptionCollapsed := fpCircles.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_Circles.Caption := lsMain.GetString('ButtonDefaultParams=Default');
  chCircles_Visible.Caption := LangStr_LineVisible;
  SetColorComboButtonsHints(ccbCircles_Color);
  ccbCircles_Color.BoundLabel.Caption := LangStr_LineColor;
  spedCircles_Width.BoundLabel.Caption := LangStr_LineWidth;
  pscbCircles_Style.BoundLabel.Caption := LangStr_LineStyle;
  spedCircles_Transp.BoundLabel.Caption := LangStr_LineTransparency;


  // Flip panel - Radii
  fpRadii.Header.CaptionCollapsed := fpRadii.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_Radii.Caption := lsMain.GetString('ButtonDefaultParams=Default');
  chRadii_Visible.Caption := LangStr_LineVisible;
  SetColorComboButtonsHints(ccbRadii_Color);
  ccbRadii_Color.BoundLabel.Caption := LangStr_LineColor;
  spedRadii_Width.BoundLabel.Caption := LangStr_LineWidth;
  pscbRadii_Style.BoundLabel.Caption := LangStr_LineStyle;
  spedRadii_Transp.BoundLabel.Caption := LangStr_LineTransparency;


  // Flip panel - Axes
  fpAxes.Header.CaptionCollapsed := fpAxes.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_Axes.Caption := lsMain.GetString('ButtonDefaultParams=Default');
  lblAxes_Lines.Caption := lsMain.GetString('TitleLabel_Lines=Lines');
  chAxes_Visible.Caption := LangStr_LineVisible;
  SetColorComboButtonsHints(ccbAxes_Color);
  ccbAxes_Color.BoundLabel.Caption := LangStr_LineColor;
  spedAxes_Width.BoundLabel.Caption := LangStr_LineWidth;
  pscbAxes_Style.BoundLabel.Caption := LangStr_LineStyle;
  spedAxes_Transp.BoundLabel.Caption := LangStr_LineTransparency;

  lblAxes_Captions.Caption := lsMain.GetString('TitleLabel_Captions=Captions');
  chAxes_Description.Caption := LangStr_TextVisible;
  lblAxes_FontTitle.Caption := lsMain.GetString('TitleLabel_Font=Font');
  btnAxes_ChangeFont.Hint := lsMain.GetString('ChangeFont=Change font parameters...');
  FillFontParams(gpfp, FRoseDiagram.Axes.Text.Font);
  lblAxes_FontStyle.Caption := gpfp.FontStyleStr;

  lblAxes_PercentageMarkers.Caption := lsMain.GetString('TitleLabel_PercentageMarkers=Percentage markers');
  lblAxes_PercentageMarkers_FontTitle.Caption := lblAxes_FontTitle.Caption;
  chAxes_PercentageMarkers.Caption := LangStr_TextVisible;
  SetColorComboButtonsHints(ccbAxes_PercentageMarkers_Color);
  btnAxes_PercentageMarkers_ChangeFont.Hint := btnAxes_ChangeFont.Hint;
  FillFontParams(gpfp, FRoseDiagram.Axes.PercentageMarkers.Font);
  lblAxes_PercentageMarkers_FontStyle.Caption := gpfp.FontStyleStr;


  // Flip panel - Pies
  fpPies.Header.CaptionCollapsed := fpPies.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_Pies.Caption := lsMain.GetString('ButtonDefaultParams=Default');
  lblPies_Fill.Caption := lsMain.GetString('TitleLabel_Fill=Fill');
  SetColorComboButtonsHints(ccbPies_BackgroundColor);
  ccbPies_BackgroundColor.BoundLabel.Caption := LangStr_FillBackgroundColor;
  spedPies_FillTransp.BoundLabel.Caption := LangStr_FillTransparency;
  chPies_SolidFill.Caption := LangStr_FillSolid;
  hscbPies_HatchStyle.BoundLabel.Caption := LangStr_FillHatchStyle;
  SetColorComboButtonsHints(ccbPies_HatchColor);
  ccbPies_HatchColor.BoundLabel.Caption := LangStr_FillHatchColor;
  lblPies_Lines.Caption := lsMain.GetString('TitleLabel_Lines=Lines');
  SetColorComboButtonsHints(ccbPies_LineColor);
  ccbPies_LineColor.BoundLabel.Caption := LangStr_LineColor;
  spedPies_LineWidth.BoundLabel.Caption := LangStr_LineWidth;
  pscbPies_LineStyle.BoundLabel.Caption := LangStr_LineStyle;
  spedPies_LineTransp.BoundLabel.Caption := LangStr_LineTransparency;


  // Flip panel - Selected bins
  fpSelectedBins.Header.CaptionCollapsed := fpSelectedBins.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_SelectedBins.Caption := lsMain.GetString('ButtonDefaultParams=Default');
  lblSelectedBin_Fill.Caption := lsMain.GetString('TitleLabel_Fill=Fill');
  SetColorComboButtonsHints(ccbSelectedBin_BackgroundColor);
  ccbSelectedBin_BackgroundColor.BoundLabel.Caption := LangStr_FillBackgroundColor;
  spedSelectedBin_FillTransp.BoundLabel.Caption := LangStr_FillTransparency;
  chSelectedBin_SolidFill.Caption := LangStr_FillSolid;
  hscbSelectedBin_HatchStyle.BoundLabel.Caption := LangStr_FillHatchStyle;
  SetColorComboButtonsHints(ccbSelectedBin_HatchColor);
  ccbSelectedBin_HatchColor.BoundLabel.Caption := LangStr_FillHatchColor;
  lblSelectedBin_Lines.Caption := lsMain.GetString('TitleLabel_Lines=Lines');
  SetColorComboButtonsHints(ccbSelectedBin_LineColor);
  ccbSelectedBin_LineColor.BoundLabel.Caption := LangStr_LineColor;
  spedSelectedBin_LineWidth.BoundLabel.Caption := LangStr_LineWidth;
  pscbSelectedBin_LineStyle.BoundLabel.Caption := LangStr_LineStyle;
  spedSelectedBin_LineTransp.BoundLabel.Caption := LangStr_LineTransparency;


  // Flip panel - Title & description
  fpDesc.Header.CaptionCollapsed := fpDesc.Header.Caption + COLLAPSED_POSTFIX;
  sbtnDefaultParams_Title.Caption := lsMain.GetString('ButtonDefaultParams=Default');
  lblTitle.Caption := lsMain.GetString('TitleLabel_Title=Title');
  chTitle_Visible.Caption := LangStr_TextVisible;
  meTitle_Text.BoundLabel.Caption := LangStr_TextText;
  spedTitle_PosX.BoundLabel.Caption := LangStr_TextPosX;
  spedTitle_PosY.BoundLabel.Caption := LangStr_TextPosY;
  lblTitle_FontTitle.Caption := lsMain.GetString('TitleLabel_Font=Font');
  btnTitle_ChangeFont.Hint := lsMain.GetString('ChangeFont=Change font parameters...');
  FillFontParams(gpfp, FRoseDiagram.Title);
  lblTitle_FontStyle.Caption := gpfp.FontStyleStr;
  lblDesc.Caption := lsMain.GetString('TitleLabel_Description=Description');
  chDesc_Visible.Caption := LangStr_TextVisible;
  meDesc_Text.BoundLabel.Caption := LangStr_TextText;
  spedDesc_PosX.BoundLabel.Caption := LangStr_TextPosX;
  spedDesc_PosY.BoundLabel.Caption := LangStr_TextPosY;
  lblDesc_FontTitle.Caption := lsMain.GetString('TitleLabel_Font=Font');
  btnDesc_ChangeFont.Hint := lsMain.GetString('ChangeFont=Change font parameters...');
  FillFontParams(gpfp, FRoseDiagram.Description);
  lblDesc_FontStyle.Caption := gpfp.FontStyleStr;


  actSwitchTab_Diagram.Caption := lsMain.GetString('TabDiagram=Diagram');
  actSwitchTab_Metadata.Caption := lsMain.GetString('TabMetadata=Metadata');
  actSwitchTab_Report.Caption := lsMain.GetString('TabReport=Report');

  actExpandPanel_Main.Caption := fpMain.Header.Caption;
  actExpandPanel_Background.Caption := fpBackground.Header.Caption;
  actExpandPanel_Frame.Caption := fpFrame.Header.Caption;
  actExpandPanel_Circles.Caption := fpCircles.Header.Caption;
  actExpandPanel_Radii.Caption := fpRadii.Header.Caption;
  actExpandPanel_Axes.Caption := fpAxes.Header.Caption;
  actExpandPanel_Pies.Caption := fpPies.Header.Caption;
  actExpandPanel_SelectedBins.Caption := fpSelectedBins.Header.Caption;
  actExpandPanel_Description.Caption := fpDesc.Header.Caption;


  DrawDiagram;

end;
{$endregion SetLang}

procedure TFormMain.UpdateLangMenu;
var
  sl: TStringList;
  Item: TMenuItem;
  i, xInd: integer;
begin
  mnuLanguage.Clear;

  xInd := LangMgr.GetLangIndexByFileName(AppParams.LanguageIni);
  sl := TStringList.Create;
  try
    LangMgr.GetLanguageNames_EnglishAndNative(sl, ' - ');
    for i := 0 to sl.Count - 1 do
    begin
      Item := TMenuItem.Create(mnuLanguage);
      Item.Caption := sl[i];
      Item.Tag := i;
      if i = xInd then Item.Checked := True;
      Item.OnClick := ChangeLanguage;
      mnuLanguage.Add(Item);
    end;
  finally
    sl.Free;
  end;
end;

procedure TFormMain.ChangeLanguage(Sender: TObject);
var
  Item: TMenuItem;
  i, xInd: integer;
begin
  if not (Sender is TMenuItem) then Exit;
  Item := Sender as TMenuItem;
  LangMgr.SetActiveLanguageByIndex(Item.Tag);
  xInd := Item.Tag;
  AppParams.LanguageIni := LangMgr.GetLanguageFileNameByIndex(xInd);

  for i := 0 to mnuLanguage.Count - 1 do
  begin
    Item := mnuLanguage.Items[i];
    Item.Checked := i = xInd;
  end;

  SetLang;
  if Assigned(FormEditData) then FormEditData.SetLang;
  if Assigned(FormMeasurementConverter) then FormMeasurementConverter.SetLang;
  if Assigned(FormRandomDiagram) then FormRandomDiagram.SetLang;
  if Assigned(FormAbout) then FormAbout.SetLang;
end;

procedure TFormMain.actReloadCurrentLanguageFileExecute(Sender: TObject);
begin
  if LangMgr.ActiveLanguageIndex >= 0 then
  begin
    LangMgr.SetActiveLanguageByIndex(LangMgr.ActiveLanguageIndex);
    SetLang;
    if Assigned(FormEditData) then FormEditData.SetLang;
    if Assigned(FormMeasurementConverter) then FormMeasurementConverter.SetLang;
    if Assigned(FormRandomDiagram) then FormRandomDiagram.SetLang;
    if Assigned(FormAbout) then FormAbout.SetLang;
  end;
end;

{$endregion Languagae}



procedure TFormMain.RepaintDiagram(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  tmUpdateDiag.Restart;
end;

procedure TFormMain.RepaintDiagramAndModify(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  Modify;
  RepaintDiagram(Self);
end;

procedure TFormMain.Modify;
begin
  FModified := True;
  UpdateAppTitleAndCaption;
end;

procedure TFormMain.Unmodify;
begin
  FModified := False;
  UpdateAppTitleAndCaption;
end;

procedure TFormMain.tmUpdateDiagTimer(Sender: TObject);
begin
  //PrepareDataList;
  DrawDiagram;
  if PageList.ActivePage = spReport then UpdateReport;
  InitControls;
end;

procedure TFormMain.spedMarginMMChange(Sender: TObject);
var
  x, xWidth: integer;
begin
  x := spedMarginMM.Value;
  xWidth := spedRadiusMM.Value;
  // The sum of the margins must be smaller than the width of the entire diagram.
  if (x * 2) >= xWidth then spedMarginMM.Value := (xWidth div 2) - 1;
  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.tmStartTimer(Sender: TObject);
begin
  UpdateControlsFromRoseDiagramObject;
  {$IFNDEF DEBUG}
  if FCurrentFile = '' then actEditMeasurements.Execute;
  {$ENDIF}
end;

procedure TFormMain.actRepaintDiagramExecute(Sender: TObject);
begin
  sboxImg.Repaint;
  DrawDiagram;
end;

procedure TFormMain.actReport_CopySelectionExecute(Sender: TObject);
begin
  htvReport.CopyToClipboard;
end;



{$region ' INI - Load & Save settings '}
procedure TFormMain.SaveSettingsToIni;
var
  Ini: TJppMemIniFile;
  i: integer;
  Column: TVirtualTreeColumn;
  sID: string;

  procedure WriteFlipPanel(fp: TJppFlipPanel);
  begin
    Ini.WriteBool(fp.Name + '.Expanded', fp.Expanded);
  end;

begin
  Ini := TJppMemIniFile.Create(AppParams.IniFile, TEncoding.UTF8);
  try

    Ini.CurrentSection := INI_SECTION_MAIN;

    Ini.WriteFormPos(Self);

    Ini.WriteString('LanguageIni', ExtractFileName(AppParams.LanguageIni));
    Ini.WriteString('LastDir', dlgOpen.InitialDir);

    WriteFlipPanel(fpMain);
    WriteFlipPanel(fpBackground);
    WriteFlipPanel(fpFrame);
    WriteFlipPanel(fpCircles);
    WriteFlipPanel(fpRadii);
    WriteFlipPanel(fpAxes);
    WriteFlipPanel(fpPies);
    WriteFlipPanel(fpSelectedBins);
    WriteFlipPanel(fpDesc);
    WriteFlipPanel(fpShortStats);

    Ini.WriteBool('pnLeft_Visible', pnLeft.Visible);
    Ini.WriteInteger('pnLeft_Width', pnLeft.Width);

    Ini.WriteBool('pnRight_Visible', pnRight.Visible);


    Ini.CurrentSection := INI_SECTION_GRID_BINS;

    for i := 0 to vstBins.Header.Columns.Count - 1 do
    begin
      Column := vstBins.Header.Columns[i];
      sID := 'COL_' + itos(i);
      Ini.WriteInteger(sID, Column.Width);
    end;


    Ini.UpdateFile;

  finally
    Ini.Free;
  end;
end;

procedure TFormMain.LoadSettingsFromIni;
var
  Ini: TJppMemIniFile;
  i, x: integer;
  Column: TVirtualTreeColumn;
  s, sID: string;

  procedure ReadFlipPanel(fp: TJppFlipPanel);
  begin
    fp.Expanded := Ini.ReadBool(fp.Name + '.Expanded', fp.Expanded);
  end;

begin
  if not FileExists(AppParams.IniFile) then Exit;

  Ini := TJppMemIniFile.Create(AppParams.IniFile);
  try

    Ini.CurrentSection := INI_SECTION_MAIN;

    actEsc.Enabled := Ini.ReadBool('EscExit', False);

    Ini.ReadFormPos(Self);

    s := Ini.ReadString('LanguageIni', ExtractFileName(AppParams.LanguageIni));
    if s <> '' then
    begin
      s := AppParams.LangDir + PathDelim + s;
      if FileExists(s) then AppParams.LanguageIni := s;
    end;

    s := Ini.ReadString('LastDir', '');
    if (s = '') or (not DirectoryExists(s)) then s := ExtractFileDir(ParamStr(0));
    UpdateInitialDirs(s);

    ReadFlipPanel(fpMain);
    ReadFlipPanel(fpBackground);
    ReadFlipPanel(fpFrame);
    ReadFlipPanel(fpCircles);
    ReadFlipPanel(fpRadii);
    ReadFlipPanel(fpAxes);
    ReadFlipPanel(fpPies);
    ReadFlipPanel(fpSelectedBins);
    ReadFlipPanel(fpDesc);
    ReadFlipPanel(fpShortStats);

    pnLeft.Visible := Ini.ReadBool('pnLeft_Visible', pnLeft.Visible);

    x := Ini.ReadInteger('pnLeft_Width', pnLeft.Width);
    x := GetIntInRange(x, 50, 300);
    pnLeft.Width := x;

    splLeft.Visible := pnLeft.Visible;
    actShowHideLeftPanel.Checked := pnLeft.Visible;

    pnRight.Visible := Ini.ReadBool('pnRight_Visible', pnRight.Visible);
    actShowHideRightPanel.Checked := pnRight.Visible;


    Ini.CurrentSection := INI_SECTION_GRID_BINS;

    for i := 0 to vstBins.Header.Columns.Count - 1 do
    begin
      Column := vstBins.Header.Columns[i];
      sID := 'COL_' + itos(i);
      x := Ini.ReadInteger(sID, Column.Width);
      x := GetIntInRange(x, 25, 100);
      Column.Width := x;
    end;


  finally
    Ini.Free;
  end;
end;
{$endregion INI - Load & Save settings}



{$region ' vstBins related '}
procedure TFormMain.vstBins_Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  vd: PVstBinsData;
begin
  FRoseDiagram.BinDataList.UnselectAllBins;

  Node := vstBins.GetFirst;
  while Node <> nil do
  begin
    if not vstBins_GetNodeData(Node, vd) then Continue;
    if Node^.Selected then
      FRoseDiagram.BinDataList.SelectBin(vd^.BinIndex);
    Node := vstBins.GetNext(Node);
  end;

  vstBins_UpdateSelectionStats;

  DrawDiagram;
end;

procedure TFormMain.vstBins_UpdateSelectedRowsFromBinDataList;
var
  Node: PVirtualNode;
  vd: PVstBinsData;
  Index: integer;
  bSelected: Boolean;
begin
  vstBins.BeginUpdate;
  try

    vstBins.ClearSelection;

    Node := vstBins.GetFirst;
    while Node <> nil do
    begin
      if not vstBins_GetNodeData(Node, vd) then Continue;
      Index := vd^.BinIndex;
      bSelected := FRoseDiagram.BinDataList[Index].Selected;
      vstBins.Selected[Node] := bSelected;
      vd^.Selected := bSelected;
      Node := vstBins.GetNext(Node);
    end;

  finally
    vstBins.EndUpdate;
  end;

  vstBins_UpdateSelectionStats;
end;

procedure TFormMain.vstBins_UpdateSelectionStats;
var
  Node: PVirtualNode;
  vd: PVstBinsData;
  SelCount: integer;
  bSelected: Boolean;
  SelSum, SelPercent: Single;
begin
  SelSum := 0;
  SelPercent := 0;
  SelCount := 0;

  Node := vstBins.GetFirst;
  while Node <> nil do
  begin
    if not vstBins_GetNodeData(Node, vd) then Continue;

    if Node.Selected then
    begin
      Inc(SelCount);
      SelSum := SelSum + vd^.Count;
      SelPercent := SelPercent + vd^.Percent;
    end;

    Node := vstBins.GetNext(Node);
  end;


  dlblSelectedBinCount.RightCaption := itos(SelCount);
  dlblSelection.RightCaption :=
    {string(TStr.CharSigmaBig) + ' = ' +} InsertNumSep(ftos(SelSum, 2)) + ' / ' +
    ftos(SelPercent, 2) + '%';
end;

procedure TFormMain.vstBins_BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  clBg, clGrid: TColor;
  vd: PVstBinsData;
  R: TRect;
  bSelected: Boolean;
begin
  if not Assigned(Node) then Exit;
  clBg := clNone;
  bSelected := Node.Selected;
  R := CellRect;
  InflateRect(R, 1, 1);

  if vstBins.Focused then
    if bSelected then clGrid := vstBins.Colors.FocusedSelectionBorderColor else clGrid := vstBins.Colors.GridLineColor
  else
    if bSelected then clGrid := vstBins.Colors.UnfocusedSelectionBorderColor else clGrid := vstBins.Colors.GridLineColor;

  if Column = COL_NO then clBg := clBtnFace
  else if FRoseDiagram.BinDataList.Stats.MeasurementCount > 0 then //if Column = COL_COUNT then
  begin
    if not vstBins_GetNodeData(Node, vd) then Exit;
    if vd^.Count = FRoseDiagram.BinDataList.Stats.MaxMeasurementsInBin then clBg := 13499135;
    if vd^.Count = FRoseDiagram.BinDataList.Stats.MinMeasurementsInBin then clBg := 15594719;
  end;

  if bSelected then clGrid := Rozeta.Misc.Darker(clGrid, 20);

  if clBg <> clNone then
    with TargetCanvas do
    begin
      Brush.Color := clBg;
      Pen.Style := psClear;
      Rectangle(R);
    end;

  with TargetCanvas do
  begin
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    Pen.Color := clGrid;
    Rectangle(R);
  end;
end;

procedure TFormMain.vstBins_PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  clText: TColor;
  vd: PVstBinsData;
begin
  if vsSelected in Node^.States then Exit;
  clText := clNone;

  if (Column = COL_COUNT) or (Column = COL_PERCENT) then
  begin
    if not vstBins_GetNodeData(Node, vd) then Exit;
    if vd^.Count = 0 then clText := RGB3(200)
    else if vd^.Count = FRoseDiagram.BinDataList.Stats.MaxMeasurementsInBin then clText := clRed;
  end
  else if Column = COL_NO then clText := clGray;

  if clText <> clNone then TargetCanvas.Font.Color := clText;
end;

procedure TFormMain.vstBins_GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  vd: PVstBinsData;
begin
  CellText := '';
  if not vstBins_GetNodeData(Node, vd) then Exit;

  case Column of
    COL_NO: CellText := IntToStrEx(vd^.No);
    COL_BIN: CellText := vd^.RangeStr;
    COL_COUNT: CellText := itos(vd^.Count);
    COL_PERCENT: CellText := ftos(vd^.Percent, 2);
  end;

 end;

procedure TFormMain.vstBins_GetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TVstBinsData);
end;

function TFormMain.vstBins_GetNodeData(Node: PVirtualNode; var DataPtr: PVstBinsData): Boolean;
begin
  Result := False;
  if not Assigned(Node) then Exit;
  DataPtr := vstBins.GetNodeData(Node);
  Result := Assigned(DataPtr);
end;

procedure TFormMain.vstBins_FillGrid;
var
  i: integer;
  BinData: TBinData;
  vd: PVstBinsData;
  Node: PVirtualNode;
begin
  vstBins.BeginUpdate;
  try
    vstBins.Clear;

    for i := 0 to FRoseDiagram.BinDataList.Count - 1 do
    begin
      BinData := FRoseDiagram.BinDataList[i];

      Node := vstBins.AddChild(nil); // nil = parent
      if not vstBins_GetNodeData(Node, vd) then Exit;

      vd^.No := BinData.No;
      vd^.BinIndex := BinData.BinIndex;
      vd^.Count := BinData.MeasurementCount;
      vd^.BinStart := BinData.StartValue;
      vd^.BinEnd := BinData.EndValue;
      vd^.Percent := BinData.Percent;
      vd^.Selected := BinData.Selected;
    end;

  finally
    vstBins.EndUpdate;
  end;
end;

procedure TFormMain.vstBins_HeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if FUpdatingControls then Exit;
  if HitInfo.Button = mbRight then Exit;
  if vstBins.Header.SortDirection = sdAscending then vstBins.Header.SortDirection := sdDescending
  else vstBins.Header.SortDirection := sdAscending;
  vstBins.SortTree(HitInfo.Column, vstBins.Header.SortDirection);
end;

procedure TFormMain.vstBins_CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  vd1, vd2: PVstBinsData;

  function CompareIntegers(const x1, x2: integer): integer;
  begin
    if x1 < x2 then Result := -1
    else if x1 > x2 then Result := 1
    else Result := 0;
  end;

  function CompareSingle(const x1, x2: Single): integer;
  begin
    if x1 < x2 then Result := -1
    else if x1 > x2 then Result := 1
    else Result := 0;
  end;

begin
  if FUpdatingControls then Exit;
  if not vstBins_GetNodeData(Node1, vd1) then Exit;
  if not vstBins_GetNodeData(Node2, vd2) then Exit;

  case Column of

    // No
    0: Result := CompareIntegers(vd1^.No, vd2^.No);

    // Bin
    1: Result := CompareIntegers(vd1^.BinStart, vd2^.BinStart);

    // Count
    2: Result := CompareIntegers(vd1^.Count, vd2^.Count);

    // Percent
    3: Result := CompareSingle(vd1^.Percent, vd2^.Percent);

  end;
end;

{$endregion vstBins related}

procedure TFormMain.cbClassSizeChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  PrepareDataList;
  InitControls;
  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.cbDiagramTypeChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  InitControls;
  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.cbLinearMeasurementModeChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  PrepareDataList;
  InitControls;
  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.cbMeasurementTypeChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  PrepareDataList;
  InitControls;
  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.chCentralSymmetryChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  PrepareDataList;
  InitControls;
  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.chDesc_VisibleChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  InitControls;
  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.chTitle_VisibleChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  InitControls;
  RepaintDiagramAndModify(Self);
end;

{$region '  Loading JSON / presets  '}

procedure TFormMain.LoadDataFile(const FileName: string; const LoadMeasurements, LoadMetadata, UpdateBins: Boolean);
var
  rdf: TRoseDataFile;
  Options: TRoseDataFileOptions;
begin
  rdf := TRoseDataFile.Create(FRoseDiagram);
  try
    Options := {%H-}Options.SetAll;
    if not LoadMeasurements then Exclude(Options, rdfoMeasurements);
    if not LoadMetadata then Exclude(Options, rdfoMetadata);
    rdf.LoadFromFile(FileName, Options);
  finally
    rdf.Free;
  end;

  UpdateControlsFromRoseDiagramObject;

  if UpdateBins then
  begin
    PrepareDataList;
    vstBins_FillGrid;
  end;

  InitControls;
  tmUpdateDiag.Restart;
end;

function TFormMain.LoadPresetFile(const FileName: string; const UpdateBins: Boolean): Boolean;
var
  s, sMetaAuthor, sMetaSubject, sMetaDesc: string;
begin
  Result := False;

  if not FileExists(FileName) then
  begin
    s := lsMain.GetString('PresetFileNotExists=Unable to load settings from "%s".\nFile does not exist!');
    s := fixs(s, FileName);
    //raise Exception.Create(s);
    WinMsgError(s, AppParams.AppName, Handle);
    Exit;
  end;

  sMetaAuthor := meMeta_Author.Text;
  sMetaSubject := meMeta_Subject.Text;
  sMetaDesc := meMeta_Description.Text;
  try
    LoadDataFile(FileName, False, False, UpdateBins);
    Result := True;
  finally
    meMeta_Author.Text := sMetaAuthor;
    meMeta_Subject.Text := sMetaSubject;
    meMeta_Description.Text := sMetaDesc;
  end;
end;

procedure TFormMain.actDefaultParams_ALLExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultMain, True) then
  if LoadPresetFile(AppParams.PresetFile_DefaultFrame, False) then
  if LoadPresetFile(AppParams.PresetFile_DefaultBackground, False) then
  if LoadPresetFile(AppParams.PresetFile_DefaultCircles, False) then
  if LoadPresetFile(AppParams.PresetFile_DefaultRadii, False) then
  if LoadPresetFile(AppParams.PresetFile_DefaultAxes, False) then
  if LoadPresetFile(AppParams.PresetFile_DefaultPies, False) then
  if LoadPresetFile(AppParams.PresetFile_DefaultSelectedBins, False) then
  LoadPresetFile(AppParams.PresetFile_DefaultTitle, False);

  Modify;
end;

procedure TFormMain.actDefaultParams_MainExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultMain, True) then Modify;
end;

procedure TFormMain.actDefaultParams_FrameExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultFrame, False) then Modify;
end;

procedure TFormMain.actDefaultParams_BackgroundExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultBackground, False) then Modify;
end;

procedure TFormMain.actDefaultParams_CirclesExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultCircles, False) then Modify;
end;

procedure TFormMain.actDefaultParams_RadiiExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultRadii, False) then Modify;
end;

procedure TFormMain.actDefaultParams_AxesExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultAxes, False) then Modify;
end;

procedure TFormMain.actDefaultParams_PiesExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultPies, False) then Modify;
end;

procedure TFormMain.actDefaultParams_SelectedBinsExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultSelectedBins, False) then Modify;
end;

procedure TFormMain.actDefaultParams_TitleExecute(Sender: TObject);
begin
  if LoadPresetFile(AppParams.PresetFile_DefaultTitle, False) then Modify;
end;
{$endregion Loading JSON / presets}


{$region ' UpdateControlsFromRoseDiagramObject '}
procedure TFormMain.UpdateControlsFromRoseDiagramObject;
var
  bUC: Boolean;
  gpfp: TGPFontParams;
  x: integer;
begin
  bUC := FUpdatingControls;
  FUpdatingControls := True;
  try

    // Main options
    spedRadiusMM.Value := FRoseDiagram.RadiusMM;
    spedMarginMM.Value := FRoseDiagram.MarginMM;
    SetComboClassSizeValue(FRoseDiagram.ClassSize);

    if FRoseDiagram.DiagramType = rdtRose then x := 0 else x := 1;
    cbDiagramType.ItemIndex := x;

    chDrawInternalPolygonLines.Checked := FRoseDiagram.DrawInternalPolygonLines;

    if FRoseDiagram.MeasurementType = mt360 then x := 0 else x := 1;
    cbMeasurementType.ItemIndex := x;

    chCentralSymmetry.Checked := FRoseDiagram.CentralSymmetry;

    if FRoseDiagram.BinDataList.LinearMeasurementFixMode = lmfmNormalize then x := 0 else x := 1;
    cbLinearMeasurementMode.ItemIndex := x;


    // Background
    ccbBackground_Color.SelectedColor := FRoseDiagram.Background.Color;
    spedBackground_Transp.Value := FRoseDiagram.Background.Transparency;

    // Frame
    chFrame_Visible.Checked := FRoseDiagram.Frame.Visible;
    ccbFrame_Color.SelectedColor := FRoseDiagram.Frame.Color;
    spedFrame_Width.Value := FRoseDiagram.Frame.Width;
    pscbFrame_Style.Selected := FRoseDiagram.Frame.Style;
    spedFrame_Transp.Value := FRoseDiagram.Frame.Transparency;

    // Circles
    chCircles_Visible.Checked := FRoseDiagram.Circles.Visible;
    spedCircles_Count.Value := FRoseDiagram.CirclesCount;
    ccbCircles_Color.SelectedColor := FRoseDiagram.Circles.Color;
    spedCircles_Width.Value := FRoseDiagram.Circles.Width;
    pscbCircles_Style.Selected := FRoseDiagram.Circles.Style;
    spedCircles_Transp.Value := FRoseDiagram.Circles.Transparency;

    // Radii
    chRadii_Visible.Checked := FRoseDiagram.Radii.Visible;
    ccbRadii_Color.SelectedColor := FRoseDiagram.Radii.Color;
    spedRadii_Width.Value := FRoseDiagram.Radii.Width;
    pscbRadii_Style.Selected := FRoseDiagram.Radii.Style;
    spedRadii_Transp.Value := FRoseDiagram.Radii.Transparency;

    // Axes
    chAxes_Visible.Checked := FRoseDiagram.Axes.Line.Visible;
    ccbAxes_Color.SelectedColor := FRoseDiagram.Axes.Line.Color;
    spedAxes_Width.Value := FRoseDiagram.Axes.Line.Width;
    pscbAxes_Style.Selected := FRoseDiagram.Axes.Line.Style;
    spedAxes_Transp.Value := FRoseDiagram.Axes.Line.Transparency;
    chAxes_Description.Checked := FRoseDiagram.Axes.Text.Visible;
    if FRoseDiagram.Axes.CaptionType = actDegrees then x := 0 else x := 1;
    cbAxes_CaptionType.ItemIndex := x;

    FillFontParams(gpfp{%H-}, FRoseDiagram.Axes.Text.Font);
    lblAxes_Font.Caption := gpfp.InfoStr(False);
    lblAxes_FontStyle.Caption := gpfp.FontStyleStr;
    csAxes_FontColor.SelectedColor := gpfp.Color;
    csAxes_FontColor.AnchoredControls.UpdateAllControlsPos;

    chAxes_PercentageMarkers.Checked := FRoseDiagram.Axes.PercentageMarkers.Visible;
    FillFontParams(gpfp{%H-}, FRoseDiagram.Axes.PercentageMarkers.Font);
    lblAxes_PercentageMarkers_Font.Caption := gpfp.InfoStr(False);
    lblAxes_PercentageMarkers_FontStyle.Caption := gpfp.FontStyleStr;
    csAxes_PercentageMarkers_FontColor.SelectedColor := gpfp.Color;
    csAxes_PercentageMarkers_FontColor.AnchoredControls.UpdateAllControlsPos;

    ccbAxes_PercentageMarkers_Color.SelectedColor := FRoseDiagram.Axes.PercentageMarkers.MarkerColor;
    spedAxes_PercentageMarkers_Width.Value := FRoseDiagram.Axes.PercentageMarkers.MarkerWidth;
    spedAxes_PercentageMarkers_Transp.Value := FRoseDiagram.Axes.PercentageMarkers.MarkerTransparency;


    // Pies
    ccbPies_BackgroundColor.SelectedColor := FRoseDiagram.PieFill.Color;
    spedPies_FillTransp.Value := FRoseDiagram.PieFill.Transparency;
    chPies_SolidFill.Checked := FRoseDiagram.PieFill.SolidFill;
    hscbPies_HatchStyle.SelectedHatchStyle := TIGPHatchStyle(integer(FRoseDiagram.PieFill.HatchStyle));
    ccbPies_HatchColor.SelectedColor := FRoseDiagram.PieFill.HatchColor;
    ccbPies_LineColor.SelectedColor := FRoseDiagram.PieLine.Color;
    spedPies_LineWidth.Value := FRoseDiagram.PieLine.Width;
    pscbPies_LineStyle.Selected := FRoseDiagram.PieLine.Style;
    spedPies_LineTransp.Value := FRoseDiagram.PieLine.Transparency;

    // Selected bin
    ccbSelectedBin_BackgroundColor.SelectedColor := FRoseDiagram.SelectedBinFill.Color;
    spedSelectedBin_FillTransp.Value := FRoseDiagram.SelectedBinFill.Transparency;
    chSelectedBin_SolidFill.Checked := FRoseDiagram.SelectedBinFill.SolidFill;
    hscbSelectedBin_HatchStyle.SelectedHatchStyle := TIGPHatchStyle(integer(FRoseDiagram.SelectedBinFill.HatchStyle));
    ccbSelectedBin_HatchColor.SelectedColor := FRoseDiagram.SelectedBinFill.HatchColor;
    ccbSelectedBin_LineColor.SelectedColor := FRoseDiagram.SelectedBinLine.Color;
    spedSelectedBin_LineWidth.Value := FRoseDiagram.SelectedBinLine.Width;
    pscbSelectedBin_LineStyle.Selected := FRoseDiagram.SelectedBinLine.Style;
    spedSelectedBin_LineTransp.Value := FRoseDiagram.SelectedBinLine.Transparency;

    // Title
    chTitle_Visible.Checked := FRoseDiagram.Title.Visible;
    meTitle_Text.Text := FRoseDiagram.Title.Text;
    spedTitle_PosX.Value := FRoseDiagram.Title.PosX;
    spedTitle_PosY.Value := FRoseDiagram.Title.PosY;

    FillFontParams(gpfp{%H-}, FRoseDiagram.Title);
    lblTitle_Font.Caption := gpfp.InfoStr(False);
    lblTitle_FontStyle.Caption := gpfp.FontStyleStr;
    csTitle_FontColor.SelectedColor := gpfp.Color;
    csTitle_FontColor.AnchoredControls.UpdateAllControlsPos;

    // Description
    chDesc_Visible.Checked := FRoseDiagram.Description.Visible;
    meDesc_Text.Text := FRoseDiagram.Description.Text;
    spedDesc_PosX.Value := FRoseDiagram.Description.PosX;
    spedDesc_PosY.Value := FRoseDiagram.Description.PosY;

    FillFontParams(gpfp{%H-}, FRoseDiagram.Description);
    lblDesc_Font.Caption := gpfp.InfoStr(False);
    lblDesc_FontStyle.Caption := gpfp.FontStyleStr;
    csDesc_FontColor.SelectedColor := gpfp.Color;
    csDesc_FontColor.AnchoredControls.UpdateAllControlsPos;



    // Metadata
    meMeta_Subject.Text := FRoseDiagram.Metadata.Subject;
    meMeta_Author.Text := FRoseDiagram.Metadata.Author;
    meMeta_Description.Text := FRoseDiagram.Metadata.Description;


  finally
    FUpdatingControls := bUC;
  end;
end;
{$endregion UpdateControlsFromRoseDiagramObject}



procedure TFormMain.SetComboClassSizeValue(const ClassSize: Byte);
var
  i: integer;
  xb: Byte;
  s: string;
begin
  for i := 0 to cbClassSize.Items.Count - 1 do
  begin
    s := cbClassSize.Items[i];
    s := TrimFromEnd(s, TSpecialChars.Degree);
    if not TryStrToByte(s, xb) then Continue;
    if xb = ClassSize then
    begin
      cbClassSize.ItemIndex := i;
      Break;
    end;
  end;
end;

{$region '  Changing font params  '}
procedure TFormMain.btnAxes_ChangeFontClick(Sender: TObject);
var
  Params: TGPFontParams;
begin
  FillFontParams(Params{%H-}, FRoseDiagram.Axes.Text.Font);
  if not ExecuteGPFontDialog(Params, RD_MAX_FONT_SIZE) then Exit;
  ApplyFontParams(Params, FRoseDiagram.Axes.Text.Font);

  lblAxes_Font.Caption := Params.InfoStr(False);
  lblAxes_FontStyle.Caption := Params.FontStyleStr;
  csAxes_FontColor.SelectedColor := Params.Color;

  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.btnAxes_PercentageMarkers_ChangeFontClick(Sender: TObject);
var
  Params: TGPFontParams;
begin
  FillFontParams(Params{%H-}, FRoseDiagram.Axes.PercentageMarkers.Font);
  if not ExecuteGPFontDialog(Params, RD_MAX_FONT_SIZE) then Exit;
  ApplyFontParams(Params, FRoseDiagram.Axes.PercentageMarkers.Font);

  lblAxes_PercentageMarkers_Font.Caption := Params.InfoStr(False);
  lblAxes_PercentageMarkers_FontStyle.Caption := Params.FontStyleStr;
  csAxes_PercentageMarkers_FontColor.SelectedColor := Params.Color;

  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.btnTitle_ChangeFontClick(Sender: TObject);
var
  Params: TGPFontParams;
begin
  FillFontParams(Params{%H-}, FRoseDiagram.Title);
  if not ExecuteGPFontDialog(Params, RD_MAX_FONT_SIZE) then Exit;
  ApplyFontParams(Params, FRoseDiagram.Title);

  lblTitle_Font.Caption := Params.InfoStr(False);
  lblTitle_FontStyle.Caption := Params.FontStyleStr;
  csTitle_FontColor.SelectedColor := Params.Color;

  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.btnDesc_ChangeFontClick(Sender: TObject);
var
  Params: TGPFontParams;
begin
  FillFontParams(Params{%H-}, FRoseDiagram.Description);
  if not ExecuteGPFontDialog(Params, RD_MAX_FONT_SIZE) then Exit;
  ApplyFontParams(Params, FRoseDiagram.Description);

  lblDesc_Font.Caption := Params.InfoStr(False);
  lblDesc_FontStyle.Caption := Params.FontStyleStr;
  csDesc_FontColor.SelectedColor := Params.Color;

  RepaintDiagramAndModify(Self);
end;

procedure TFormMain.FillFontParams(var gpfp: TGPFontParams; RoseText: TRoseDiagramText);
begin
  gpfp.AssignParams(
    RoseText.Font.FontName,
    RoseText.Font.Size,
    RoseText.Font.Color,
    RoseText.Font.Transparency,
    GPFontStyleToFontStyles(RoseText.Font.Style)
  );
end;

procedure TFormMain.FillFontParams(var gpfp: TGPFontParams; RoseFont: TRoseDiagramFont);
begin
  gpfp.AssignParams(
    RoseFont.FontName,
    RoseFont.Size,
    RoseFont.Color,
    RoseFont.Transparency,
    GPFontStyleToFontStyles(RoseFont.Style)
  );
end;

procedure TFormMain.ApplyFontParams(const gpfp: TGPFontParams; RoseText: TRoseDiagramText);
begin
  RoseText.Font.SetProperties(gpfp.Name, gpfp.Size, gpfp.GetGPFontStyle, gpfp.Color, gpfp.Transparency);
end;

procedure TFormMain.ApplyFontParams(const gpfp: TGPFontParams; RoseFont: TRoseDiagramFont);
begin
  RoseFont.SetProperties(gpfp.Name, gpfp.Size, gpfp.GetGPFontStyle, gpfp.Color, gpfp.Transparency);
end;

{$endregion Changing font params}


{$region ' Expanding / collapsing panels '}
procedure TFormMain.ExpandOrCollapseFlipPanels(const bExpand: Boolean);
begin
  fpMain.Expanded := bExpand;
  fpBackground.Expanded := bExpand;
  fpFrame.Expanded := bExpand;
  fpCircles.Expanded := bExpand;
  fpRadii.Expanded := bExpand;
  fpAxes.Expanded := bExpand;
  fpPies.Expanded := bExpand;
  fpSelectedBins.Expanded := bExpand;
  fpDesc.Expanded := bExpand;
  if not pnRight.Visible then actShowHideRightPanel.Execute;
end;

procedure TFormMain.ExpandOnePanel(fp: TJppFlipPanel);
begin
  //if not pnRight.Visible then actShowHideRightPanel.Execute;
  ExpandOrCollapseFlipPanels(False);
  fp.Expanded := True;
end;

procedure TFormMain.actCollapsePanelsExecute(Sender: TObject);
begin
  ExpandOrCollapseFlipPanels(False);
end;

procedure TFormMain.actExpandPanelsExecute(Sender: TObject);
begin
  ExpandOrCollapseFlipPanels(True);
end;

procedure TFormMain.actExpandPanel_AxesExecute(Sender: TObject);
begin
  ExpandOnePanel(fpAxes);
end;

procedure TFormMain.actExpandPanel_BackgroundExecute(Sender: TObject);
begin
  ExpandOnePanel(fpBackground);
end;

procedure TFormMain.actExpandPanel_CirclesExecute(Sender: TObject);
begin
  ExpandOnePanel(fpCircles);
end;

procedure TFormMain.actExpandPanel_FrameExecute(Sender: TObject);
begin
  ExpandOnePanel(fpFrame);
end;

procedure TFormMain.actExpandPanel_PiesExecute(Sender: TObject);
begin
  ExpandOnePanel(fpPies);
end;

procedure TFormMain.actExpandPanel_RadiiExecute(Sender: TObject);
begin
  ExpandOnePanel(fpRadii);
end;

procedure TFormMain.actExpandPanel_SelectedBinsExecute(Sender: TObject);
begin
  ExpandOnePanel(fpSelectedBins);
end;

procedure TFormMain.actExpandPanel_MainExecute(Sender: TObject);
begin
  ExpandOnePanel(fpMain);
end;

procedure TFormMain.actExpandPanel_DescriptionExecute(Sender: TObject);
begin
  ExpandOnePanel(fpDesc);
end;
{$endregion Expanding / collapsing panels}

procedure TFormMain.actGoTo_HomePageExecute(Sender: TObject);
begin
  GoToUrl(AppParams.UrlApplication);
end;

procedure TFormMain.actGoTo_GithubExecute(Sender: TObject);
begin
  GoToUrl(AppParams.UrlGithub);
end;

procedure TFormMain.actGoTo_DonationExecute(Sender: TObject);
begin
  GoToUrl(AppParams.UrlDonation);
end;

procedure TFormMain.actShowHideLeftPanelExecute(Sender: TObject);
begin
  pnLeft.Visible := not pnLeft.Visible;
  actShowHideLeftPanel.Checked := pnLeft.Visible;
  splLeft.Visible := pnLeft.Visible;
end;

procedure TFormMain.actShowHideRightPanelExecute(Sender: TObject);
begin
  pnRight.Visible := not pnRight.Visible;
  actShowHideRightPanel.Checked := pnRight.Visible;
end;

procedure TFormMain.actSwitchTab_DiagramExecute(Sender: TObject);
begin
  Tabs.TabIndex := 0;
end;

procedure TFormMain.actSwitchTab_MetadataExecute(Sender: TObject);
begin
  Tabs.TabIndex := 1;
end;

procedure TFormMain.actSwitchTab_ReportExecute(Sender: TObject);
begin
  Tabs.TabIndex := 2;
end;

{$region ' Select / unselect bins '}
procedure TFormMain.actSelectAllBinsExecute(Sender: TObject);
begin
  FRoseDiagram.BinDataList.SelectAllBins;
  vstBins_UpdateSelectedRowsFromBinDataList;
  DrawDiagram;
end;

procedure TFormMain.actSelectMaxBinsExecute(Sender: TObject);
begin
  FRoseDiagram.BinDataList.UnselectAllBins;
  FRoseDiagram.BinDataList.SelectMaxBins;
  vstBins_UpdateSelectedRowsFromBinDataList;
  DrawDiagram;
end;

procedure TFormMain.actSelectMinBinsExecute(Sender: TObject);
begin
  FRoseDiagram.BinDataList.UnselectAllBins;
  FRoseDiagram.BinDataList.SelectMinBins;
  vstBins_UpdateSelectedRowsFromBinDataList;
  DrawDiagram;
end;

procedure TFormMain.actUnselectAllBinsExecute(Sender: TObject);
begin
  FRoseDiagram.BinDataList.UnselectAllBins;
  vstBins_UpdateSelectedRowsFromBinDataList;
  DrawDiagram;
end;



procedure TFormMain.actInvertSelectedBinsExecute(Sender: TObject);
begin
  FRoseDiagram.BinDataList.InvertSelectedBins;
  vstBins_UpdateSelectedRowsFromBinDataList;
  DrawDiagram;
end;
{$endregion Select / unselect bins}


procedure TFormMain.actShowDebugInfoExecute(Sender: TObject);
var
  s: string;
begin
  s :=
    'Application start time: ' + MsToTimeStr(AppDebugInfo.StartTimeMS, False) + ENDL +
    'Diagram draw count: ' + IntToStrEx(AppDebugInfo.DrawCount) + ENDL +
    'Last draw time: ' + MsToTimeStr(AppDebugInfo.LastDrawTime) + ENDL +
    'Report generation time: ' + MsToTimeStr(AppDebugInfo.ReportGenerationTime) + ENDL +
    '-----------------------------------------------------' + ENDL +
    FRoseDiagram.BinDataList.AsDebugStr
  ;
  ShowWinStr(s, 'Debug info', 500, 500, AppParams.MonospaceFontName);
end;

procedure TFormMain.actShowFormMeasurementConverterExecute(Sender: TObject);
begin
  FormMeasurementConverter.Show;
end;

procedure TFormMain.actRandomDiagramExecute(Sender: TObject);
begin
  FormRandomDiagram.Show;
end;

procedure TFormMain.sboxImgPaint(Sender: TObject);
begin
  DrawCheckerboard(sboxImg.Canvas, RGB3(240), RGB3(255));
end;

procedure TFormMain.actCopyDiagramToClipboardExecute(Sender: TObject);
begin
  Clipboard.Assign(img.Picture.Bitmap);
end;





procedure TFormMain.actEditMeasurementsExecute(Sender: TObject);
begin
  FormEditData.ReadGlobalData;
  FormEditData.Show;
end;

procedure TFormMain.TabsTabChanged(Sender: TObject);
var
  x: integer;
begin
  x := Tabs.TabIndex;
  if (x >= 0) and (x < PageList.PageCount) then PageList.ActivePageIndex := x;
  if x = 2 {Report tab} then UpdateReport;
end;




end.

