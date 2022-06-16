unit Rozeta.FormRandomDiagram;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls, JPLazSpinEdit, JPL.Dialogs, JPP.CheckBox, Rozeta.AppStrings;

type
  TFormRandomDiagram = class(TForm)
    actOk: TAction;
    actClose: TAction;
    Actions: TActionList;
    btnOK: TButton;
    btnClose: TButton;
    chFloat: TJppCheckBox;
    spedMin: TJPLazSpinEdit;
    spedMax: TJPLazSpinEdit;
    spedCount: TJPLazSpinEdit;
    procedure actCloseExecute(Sender: TObject);
    procedure actOkExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SetLang;
  private

  public

  end;

var
  FormRandomDiagram: TFormRandomDiagram;

implementation

uses
  Rozeta.FormMain, JPL.RoseDiag.BinDataList, Rozeta.FormEditData;

{$R *.lfm}



procedure TFormRandomDiagram.FormCreate(Sender: TObject);
begin
  PrepareModuleStrings_RandomDiag;
  Caption := 'Random diagram';
end;

procedure TFormRandomDiagram.SetLang;
begin
  if not Assigned(lsRandomDiag) then Exit;
  Caption := lsRandomDiag.GetString('Caption=Random diagram');
end;

procedure TFormRandomDiagram.actOkExecute(Sender: TObject);
var
  Arr: TMeasurementArray;
  Count, Min, Max: Word;
  s: string;
begin
  Count := spedCount.Value;
  Min := spedMin.Value;
  Max := spedMax.Value;

  if Min > Max then
  begin
    if Assigned(lsRandomDiag) then
      s := lsRandomDiag.GetString('Error_MinGreatherThanMax=The maximum value of the measurement must be greater than the minimum value!')
    else
      s := 'The maximum value of the measurement must be greater than the minimum value!';
    MsgError(s);
    Exit;
  end;

  FillMeasurementArrayWithRandomValues(Arr{%H-}, Count, Min, Max, chFloat.Checked);
  FormMain.RoseDiagram.BinDataList.ProcessMeasurementArray(Arr);
  FormMain.PrepareDataList;
  FormMain.RepaintDiagram(Self);
  FormMain.Modify;

  if FormEditData.Visible then FormEditData.ReadGlobalData;
end;

procedure TFormRandomDiagram.actCloseExecute(Sender: TObject);
begin
  Close;
end;

end.

