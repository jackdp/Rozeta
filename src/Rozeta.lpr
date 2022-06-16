program RoseDiagram;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  2022.06
}


{$mode objfpc}{$H+}


uses
  Interfaces,
  Forms, SysUtils,
  JPL.TimeLogger,
  Rozeta.FormMain, Rozeta.FormEditData, Rozeta.FormRandomDiagram, Rozeta.FormMeasurementConverter,
  Rozeta.AppStrings, Rozeta.Misc, Rozeta.FormAbout;

{$R *.res}

begin
  TTimeLogger.StartLog_2;
  AppDebugInfo.DrawCount := 0;

  {$IF DECLARED(UseHeapTrace)}
	GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.2.0
  {$ENDIF}

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;

  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormEditData, FormEditData);
  Application.CreateForm(TFormRandomDiagram, FormRandomDiagram);
  Application.CreateForm(TFormMeasurementConverter, FormMeasurementConverter);
  Application.CreateForm(TFormAbout, FormAbout);

  ///////////////////////////////////////////// LANGUAGE ////////////////////////////////////////////////
  PrepareModuleStrings_FontDialog; // <-- This must be called *before* LangMgr.SetActiveLanguageByIniFileName
  if Assigned(LangMgr) and (FileExists(AppParams.LanguageIni)) then LangMgr.SetActiveLanguageByIniFileName(AppParams.LanguageIni);
  FormMain.SetLang;
  if Assigned(FormEditData) then FormEditData.SetLang;
  if Assigned(FormAbout) then FormAbout.SetLang;
  if Assigned(FormRandomDiagram) then FormRandomDiagram.SetLang;
  if Assigned(FormMeasurementConverter) then FormMeasurementConverter.SetLang;
  ///////////////////////////////////////////////////////////////////////////////

  TTimeLogger.EndLog_2;
  AppDebugInfo.StartTimeMS := TTimeLogger.ElapsedTime_2;

  Application.Run;
end.

