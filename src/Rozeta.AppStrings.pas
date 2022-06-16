unit Rozeta.AppStrings;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  2022.06
}

interface

uses
  SysUtils,
  JPL.LangMgr,
  Rozeta.Misc;


const
  LANG_INI_SECTION_MAIN = 'MAIN';
  LANG_INI_SECTION_EDIT_MEASUREMENTS = 'EditMeasurements';
  LANG_INI_SECTION_RANDOM_DIAGRAM = 'RandomDiagram';
  LANG_INI_SECTION_FONT_DIALOG = 'FontDialog';
  LANG_INI_SECTION_MEASUREMENT_CONVERTER = 'MeasurementConverter';
  LANG_INI_SECTION_ABOUT = 'About';


procedure InitLangMgr;
procedure PrepareModuleStrings_Main;
procedure PrepareModuleStrings_EditMeasurements;
procedure PrepareModuleStrings_RandomDiag;
procedure PrepareModuleStrings_FontDialog;
procedure PrepareModuleStrings_MeasurementConverter;
procedure PrepareModuleStrings_About;


var
  LangMgr: TLangMgr;
  lsMain: TLangSection;
  lsEditM: TLangSection;
  lsRandomDiag: TLangSection;
  lsFontDlg: TLangSection;
  lsConv: TLangSection;
  lsAbout: TLangSection;


implementation

uses
  Rozeta.FormMain, Rozeta.FormEditData, Rozeta.FormRandomDiagram, Rozeta.FormMeasurementConverter, Rozeta.FormGPFontDialog,
  Rozeta.FormAbout;


procedure InitLangMgr;
begin
  if not Assigned(LangMgr) then LangMgr := TLangMgr.Create;
  LangMgr.AddFilesFromDir(AppParams.LangDir);
end;


{$region '                              PrepareModuleStrings_Main                                  '}
procedure PrepareModuleStrings_Main;
begin
  if not Assigned(LangMgr) then InitLangMgr;
  if not Assigned(FormMain) then Exit;

  lsMain := LangMgr.AddSection(LANG_INI_SECTION_MAIN);

  with FormMain do
  begin

    lsMain.AddString('TabDiagram=Diagram');
    lsMain.AddString('TabMetadata=Metadata');
    lsMain.AddString('TabReport=Report');

    lsMain.AddString('ChangeColor=Change color...');
    lsMain.AddString('CopyColor=Copy color');
    lsMain.AddString('PasteColor=Paste color');
    lsMain.AddString('ChangeFont=Change font parameters...');
    lsMain.AddString('AlphaChannel=Alpha:');

    lsMain.AddString('TitleLabel_Bins=Bins');
    lsMain.AddString('TitleLabel_Lines=Lines');
    lsMain.AddString('TitleLabel_Fill=Fill');
    lsMain.AddString('TitleLabel_Title=Title');
    lsMain.AddString('TitleLabel_Description=Description');
    lsMain.AddString('TitleLabel_Captions=Captions');
    lsMain.AddString('TitleLabel_Font=Font');
    lsMain.AddString('TitleLabel_PercentageMarkers=Percentage markers');

    lsMain.AddString('Line_Visible=Visible');
    lsMain.AddString('Line_Color=Color:');
    lsMain.AddString('Line_Width=Width:');
    lsMain.AddString('Line_Style=Style:');
    lsMain.AddString('Line_Transparency=Transparency:');

    lsMain.AddString('Fill_BackgroundColor=Background color:');
    lsMain.AddString('Fill_Transparency=Transparency:');
    lsMain.AddString('Fill_Solid=Solid fill');
    lsMain.AddString('Fill_HatchStyle=Hatch style:');
    lsMain.AddString('Fill_HatchColor=Hatch color:');

    lsMain.AddString('Text_Visible=Visible');
    lsMain.AddString('Text_Text=Text:');
    lsMain.AddString('Text_PosX=X:');
    lsMain.AddString('Text_PosY=Y:');
    lsMain.AddString('Text_FontTransparencyShort=Transp.:');

    lsMain.AddString('FontStyle_Bold=Bold');
    lsMain.AddString('FontStyle_Italic=Italic');
    lsMain.AddString('FontStyle_Underline=Underline');
    lsMain.AddString('FontStyle_Strikeout=Strikeout');

    lsMain.AddString('ButtonDefaultParams=Default');

    lsMain.AddString('SaveQuery=The current diagram has been modified.\nDo you want to save the changes?');
    lsMain.AddString('PresetFileNotExists=Unable to load settings from "%s".\nFile does not exist!');

    lsMain.AddAction(actOpenTemplate);
    lsMain.AddAction(actSaveTemplate);

    lsMain.AddAction(actDefaultParams_ALL);
    lsMain.AddAction(actDefaultParams_Main);
    lsMain.AddAction(actDefaultParams_Background);
    lsMain.AddAction(actDefaultParams_Frame);
    lsMain.AddAction(actDefaultParams_Circles);
    lsMain.AddAction(actDefaultParams_Radii);
    lsMain.AddAction(actDefaultParams_Axes);
    lsMain.AddAction(actDefaultParams_Pies);
    lsMain.AddAction(actDefaultParams_SelectedBins);
    lsMain.AddAction(actDefaultParams_Title);


    lsMain.ac(fpMain).ap('Header.Caption');
    lsMain.ac(fpBackground).ap('Header.Caption');
    lsMain.ac(fpFrame).ap('Header.Caption');
    lsMain.ac(fpCircles).ap('Header.Caption');
    lsMain.ac(fpRadii).ap('Header.Caption');
    lsMain.ac(fpAxes).ap('Header.Caption');
    lsMain.ac(fpPies).ap('Header.Caption');
    lsMain.ac(fpSelectedBins).ap('Header.Caption');
    lsMain.ac(fpDesc).ap('Header.Caption');
    lsMain.ac(fpShortStats).ap('Header.Caption');


    lsMain.AddAction(actCopyDiagramToClipboard);
    lsMain.AddAction(actRepaintDiagram);
    lsMain.AddAction(actReport_CopySelection);

    lsMain.AddMenuItem(mnuLanguage);


    // Flip panel - Main
    lsMain.AddComponentWithBoundLabel(spedRadiusMM);
    lsMain.AddComponentWithBoundLabel(spedMarginMM);
    lsMain.AddComponentWithBoundLabel(cbClassSize);
    lsMain.AddComponentWithBoundLabel(cbDiagramType);
    lsMain.AddString('DiagramType_Rose=Rose');
    lsMain.AddString('DiagramType_Polygon=Polygon');
    lsMain.acc(chDrawInternalPolygonLines);
    lsMain.AddComponentWithBoundLabel(cbMeasurementType);
    lsMain.AddString('MeasurementType_Azimuths=Azimuths (0°-360°)');
    lsMain.AddString('MeasurementType_Strikes=Strikes (0°-180°)');
    lsMain.acc(chCentralSymmetry);
    lsMain.AddComponentWithBoundLabel(cbLinearMeasurementMode);
    lsMain.AddString('LinearMeasurementMode_Normalize180=Normalize to 180');
    lsMain.AddString('LinearMeasurementMode_Ignore=Ignore');

    // Flip panel - Circles
    lsMain.AddComponentWithBoundLabel(spedCircles_Count);

    // Flip panel - Axes
    lsMain.AddComponentWithBoundLabel(cbAxes_CaptionType);
    lsMain.AddComponentWithBoundLabel(ccbAxes_PercentageMarkers_Color);
    lsMain.AddComponentWithBoundLabel(spedAxes_PercentageMarkers_Width);
    lsMain.AddComponentWithBoundLabel(spedAxes_PercentageMarkers_Transp);

    // Bins
    lsMain.AddAction(actSelectMaxBins);
    lsMain.AddAction(actSelectMinBins);
    lsMain.AddAction(actSelectAllBins);
    lsMain.AddAction(actUnselectAllBins);
    lsMain.AddAction(actInvertSelectedBins);
    lsMain.AddString('GridBins_ColNo=No');
    lsMain.AddString('GridBins_ColBin=Bin');
    lsMain.AddString('GridBins_ColCount=Count');
    lsMain.acch(dlblSelectedBinCount);
    lsMain.acch(dlblSelection);
    lsMain.acch(dlblMeasurements);
    lsMain.acch(dlblBinCount);
    lsMain.acch(dlblSumOfMeasurements);
    lsMain.acch(dlblMeasurementsPerBin);
    lsMain.acch(dlblBinMax);
    lsMain.acch(dlblBinMin);
    lsMain.AddString('dlblBinMinMax_BinsSuffix=bins');
    lsMain.acch(dlblRadius);
    lsMain.acch(dlblMean);
    lsMain.acch(dlblStdDev);

    // Menu - File
    lsMain.AddMenuItem(mnuFile);
    lsMain.AddAction(actOpen);
    lsMain.AddAction(actSave);
    lsMain.AddAction(actSaveAs);
    lsMain.AddAction(actSaveReport);
    lsMain.AddAction(actSavePng);
    lsMain.AddAction(actEditMeasurements);
    lsMain.AddAction(actCloseFile);
    lsMain.AddAction(actExit);

    // Menu - Tabs
    lsMain.AddMenuItem(mnuTabs);

    // Menu - Tools
    lsMain.AddMenuItem(mnuTools);
    lsMain.AddAction(actShowFormMeasurementConverter);
    lsMain.AddAction(actRandomDiagram);

    // Menu - Panels
    lsMain.AddMenuItem(mnuPanels);
    lsMain.AddAction(actShowHideLeftPanel);
    lsMain.AddAction(actShowHideRightPanel);
    lsMain.AddAction(actExpandPanels);
    lsMain.AddAction(actCollapsePanels);

    // Menu - Presets
    lsMain.AddMenuItem(mnuPresets);

    // Menu - Help
    lsMain.AddMenuItem(mnuHelp);
    lsMain.AddAction(actAbout);
    lsMain.AddAction(actGoTo_HomePage);
    lsMain.AddAction(actGoTo_Github);
    lsMain.AddAction(actGoTo_Donation);

    // Metadata
    lsMain.AddComponentWithBoundLabel(meMeta_Subject);
    lsMain.AddComponentWithBoundLabel(meMeta_Author);
    lsMain.AddComponentWithBoundLabel(meMeta_Description);

    // Report
    lsMain.AddString('Report_HtmlLangID=en');
    lsMain.AddString('Report_File_Title=File info');
    lsMain.AddString('Report_File_Name=File:');
    lsMain.AddString('Report_File_Size=Size:');
    lsMain.AddString('Report_File_Created=Created:');
    lsMain.AddString('Report_File_Modified=Modifed:');
    lsMain.AddString('Report_Metadata_Title=Metadata');
    lsMain.AddString('Report_Metadata_Subject=Subject');
    lsMain.AddString('Report_Metadata_Author=Author');
    lsMain.AddString('Report_Metadata_Description=Description');
    lsMain.AddString('Report_Stats_Title=Stats');
    lsMain.AddString('Report_Stats_NumberOfMeasurements=Number of measurements:');
    lsMain.AddString('Report_Stats_NumberOfBins=Number of bins:');
    lsMain.AddString('Report_Stats_SumOfMeasurements=Sum of measurements:');
    lsMain.AddString('Report_Stats_MeasurementsPerBin=Measurements per bin:');
    lsMain.AddString('Report_Stats_MaxInBin=Maximum number of measurements in the bin(s):');
    lsMain.AddString('Report_Stats_BinsWithMax=Number of bins with the maximum number of measurements:');
    lsMain.AddString('Report_Stats_MinInBin=Minimum number of measurements in the bin(s):');
    lsMain.AddString('Report_Stats_BinsWithMin=Number of bins with the minimum number of measurements:');
    lsMain.AddString('Report_Stats_Radius=Radius:');
    lsMain.AddString('Report_Stats_Average=Average measurement:');
    lsMain.AddString('Report_Stats_StdDev=Standard deviation:');
    lsMain.AddString('Report_Bins_Title=Bins');
    lsMain.AddString('Report_Bins_No=No');
    lsMain.AddString('Report_Bins_Range=Range');
    lsMain.AddString('Report_Bins_Count=Count');
    lsMain.AddString('Report_Bins_Percent=Percent');
    lsMain.AddString('Report_Bins_Average=Average');
    lsMain.AddString('Report_Bins_StdDev=Std. dev.');
    lsMain.AddString('Report_Bins_Measurements=Measurements');

  end;

end;
{$endregion PrepareModuleStrings_Main}


{$region '            PrepareModuleStrings_EditMeasurements               '}
procedure PrepareModuleStrings_EditMeasurements;
begin
  if not Assigned(LangMgr) then InitLangMgr;
  if not Assigned(FormEditData) then Exit;

  lsEditM := LangMgr.AddSection(LANG_INI_SECTION_EDIT_MEASUREMENTS);

  with FormEditData do
  begin

    lsEditM.AddString('Caption', 'Measurements');
    lsEditM.AddAction(actUpdateDiagram);
    lsEditM.acc(lblInfo1);
    lsEditM.acc(lblInfo2);

  end;
end;
{$endregion PrepareModuleStrings_EditMeasurements}


{$region '             PrepareModuleStrings_RandomDiag             '}
procedure PrepareModuleStrings_RandomDiag;
begin
  if not Assigned(LangMgr) then InitLangMgr;
  if not Assigned(FormRandomDiagram) then Exit;

  lsRandomDiag := LangMgr.AddSection(LANG_INI_SECTION_RANDOM_DIAGRAM);

  with FormRandomDiagram do
  begin

    lsRandomDiag.AddString('Caption', 'Random diagram');
    lsRandomDiag.AddComponentWithBoundLabel(spedCount);
    lsRandomDiag.AddComponentWithBoundLabel(spedMin);
    lsRandomDiag.AddComponentWithBoundLabel(spedMax);
    lsRandomDiag.acc(chFloat);
    lsRandomDiag.AddAction(actOk);
    lsRandomDiag.AddAction(actClose);
    lsRandomDiag.AddString('Error_MinGreatherThanMax=The maximum value of the measurement must be greater than the minimum value!');

  end;
end;
{$endregion PrepareModuleStrings_RandomDiag}


{$region '            PrepareModuleStrings_MeasurementConverter           '}
procedure PrepareModuleStrings_MeasurementConverter;
begin
  if not Assigned(LangMgr) then InitLangMgr;
  if not Assigned(FormMeasurementConverter) then Exit;

  lsConv := LangMgr.AddSection(LANG_INI_SECTION_MEASUREMENT_CONVERTER);

  with FormMeasurementConverter do
  begin

    lsConv.AddString('Caption=Angular measurement converter');
    lsConv.AddLabel(lblSource);
    lsConv.AddLabel(lblResult);
    lsConv.AddLabel(lblConversionType);
    lsConv.AddComponentWithBoundLabel(spedRound).ap('RightLabel.Caption');
    lsConv.AddComponentWithBoundLabel(cbDecimalSeparator);
    lsConv.AddAction(actConvert);

  end;
end;
{$endregion PrepareModuleStrings_MeasurementConverter}


{$region '            PrepareModuleStrings_FontDialog               '}
procedure PrepareModuleStrings_FontDialog;
begin
  if not Assigned(LangMgr) then InitLangMgr;

  lsFontDlg := LangMgr.AddSection(LANG_INI_SECTION_FONT_DIALOG);

  // UWAGA: Formularz tworzony dynamicznie! Nie można używać komponentów!

  lsFontDlg.AddString('Caption=Font');
  lsFontDlg.AddString('cbFontName.BoundLabel.Caption=Font:');
  lsFontDlg.AddString('spedSize.BoundLabel.Caption=Size:');
  lsFontDlg.AddString('ccbColor.BoundLabel.Caption=Color:');
  lsFontDlg.AddString('gbFontStyle.Caption=Style');
  lsFontDlg.AddString('edExampleText.BoundLabel.Caption=Example text:');
  lsFontDlg.AddString('spedTransparency.BoundLabel.Caption=Transparency:');
  lsFontDlg.AddString('lblPreview.Caption=Preview:');
  lsFontDlg.AddString('btnOK.Caption=OK');
  lsFontDlg.AddString('btnCancel.Caption=Cancel');

end;
{$endregion PrepareModuleStrings_FontDialog}


{$region '            PrepareModuleStrings_About                 '}
procedure PrepareModuleStrings_About;
begin
  if not Assigned(LangMgr) then InitLangMgr;
  if not Assigned(FormAbout) then Exit;

  lsAbout := LangMgr.AddSection(LANG_INI_SECTION_ABOUT);

  with FormAbout do
  begin

    lsAbout.AddString('Caption', 'About');
    lsAbout.AddString('Donation', 'DONATION');

  end;
end;
{$endregion PrepareModuleStrings_About}



initialization

  lsMain := nil;
  lsEditM := nil;
  lsRandomDiag := nil;
  lsFontDlg := nil;
  lsConv := nil;
  lsAbout := nil;

end.


