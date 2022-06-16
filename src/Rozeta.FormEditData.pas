unit Rozeta.FormEditData;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  2022.06
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList,
  JPL.Conversion, JPP.SimplePanel, JPP.BasicSpeedButton, JPP.Labels,
  SynEdit, Rozeta.AppStrings;

type
  TFormEditData = class(TForm)
    actEsc: TAction;
    actUpdateDiagram: TAction;
    Actions: TActionList;
    sbtnUpdateDiagram: TJppBasicSpeedButton;
    lblInfo1: TJppLabel;
    lblInfo2: TJppLabel;
    pnTop: TJppSimplePanel;
    seData: TSynEdit;
    procedure actEscExecute(Sender: TObject);
    procedure actUpdateDiagramExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PrepareControls;
    procedure ReadGlobalData;
    procedure SetLang;
  private

  public

  end;


var
  FormEditData: TFormEditData;


implementation

uses
  Rozeta.FormMain, Rozeta.Misc, JPL.RoseDiag.BinDataList;


{$R *.lfm}



procedure TFormEditData.FormCreate(Sender: TObject);
begin
  Caption := 'Measurements';
  PrepareControls;
  PrepareModuleStrings_EditMeasurements;
  SetLang;
end;

procedure TFormEditData.PrepareControls;
begin
  seData.Font.Name := AppParams.MonospaceFontName;
  seData.Align := alClient;
end;

procedure TFormEditData.SetLang;
begin
  if not Assigned(lsEditM) then Exit;

  Caption := lsEditM.GetString('Caption=Measurements');

  sbtnUpdateDiagram.Repaint;

end;

procedure TFormEditData.actUpdateDiagramExecute(Sender: TObject);
var
  i: integer;
  Line: string;
  x: Single;
  Arr: TMeasurementArray;
begin
  SetLength(Arr{%H-}, 0);
  for i := 0 to seData.Lines.Count - 1 do
  begin
    Line := Trim(seData.Lines[i]);
    if not TryStoF(Line, x) then Continue;
    SetLength(Arr, Length(Arr) + 1);
    Arr[High(Arr)] := x;
  end;

  FormMain.RoseDiagram.BinDataList.ProcessMeasurementArray(Arr);
  FormMain.PrepareDataList;
  FormMain.RepaintDiagram(Self);
end;



procedure TFormEditData.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormEditData.ReadGlobalData;
var
  i: integer;
  Arr: TMeasurementArray;
begin
  Arr := FormMain.RoseDiagram.BinDataList.Measurements;
  seData.Clear;
  seData.Modified := False;
  seData.BeginUpdate;
  try
    for i := 0 to High(Arr) do
      seData.Lines.Add(ftos(Arr[i]));
  finally
    seData.EndUpdate;
  end;
end;



end.

