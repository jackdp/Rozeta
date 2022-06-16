unit Rozeta.FormMeasurementConverter;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, ExtCtrls, StdCtrls, JPLazSpinEdit, SynEdit,
  JPL.Strings, JPL.Conversion, JPL.Units,
  JPP.SimplePanel, JPP.RadioButton,
  JPP.Labels, JPP.ComboBox,
  Rozeta.Misc, Rozeta.AppStrings;

type
  TFormMeasurementConverter = class(TForm)
    actEsc: TAction;
    actConvert: TAction;
    Actions: TActionList;
    btnConvert: TButton;
    cbDecimalSeparator: TJppComboBox;
    spedRound: TJPLazSpinEdit;
    rbGradToDeg: TJppRadioButton;
    rbGradToRad: TJppRadioButton;
    rbRadToDeg: TJppRadioButton;
    rbRadToGrad: TJppRadioButton;
    pnConversionType: TJppSimplePanel;
    lblConversionType: TJppLabel;
    lblSource: TJppShadowLabel;
    lblResult: TJppShadowLabel;
    pnResult: TJppSimplePanel;
    pnSource: TJppSimplePanel;
    pnBottom: TJppSimplePanel;
    pnMain: TJppSimplePanel;
    rbDegToGrad: TJppRadioButton;
    rbDegToRad: TJppRadioButton;
    seSource: TSynEdit;
    seResult: TSynEdit;
    splMain: TSplitter;
    procedure actConvertExecute(Sender: TObject);
    procedure actEscExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PrepareControls;
    procedure SetLang;
  private
  public

  end;


var
  FormMeasurementConverter: TFormMeasurementConverter;


implementation

uses
  {%H-}Rozeta.FormMain;


{$R *.lfm}


procedure TFormMeasurementConverter.FormCreate(Sender: TObject);
begin
  Caption := 'Angular measurement converter';
  Constraints.MinWidth := Width;
  Constraints.MinHeight := 400;
  PrepareControls;
  PrepareModuleStrings_MeasurementConverter;
end;

procedure TFormMeasurementConverter.PrepareControls;
begin
  pnMain.Align := alClient;
  seSource.Align := alClient;
  pnResult.Align := alClient;
  seResult.Align := alClient;

  seSource.Font.Name := AppParams.MonospaceFontName;
  seResult.Font.Name := AppParams.MonospaceFontName;
end;

procedure TFormMeasurementConverter.SetLang;
begin
  Caption := lsConv.GetString('Caption=Angular measurement converter');
end;

procedure TFormMeasurementConverter.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMeasurementConverter.actConvertExecute(Sender: TObject);

  function GetNumStr(s: string): string;
  var
    xSrc, xResult: Double;
    DecSep: Char;
  begin
    DecSep := FormatSettings.DecimalSeparator;
    s := ReplaceFirst(s, '.', DecSep);
    s := ReplaceFirst(s, ',', DecSep);
    Result := s;
    xSrc := 0;
    xResult := 0;

    try
      xSrc := StrToFloat(s);

      if rbDegToGrad.Checked then xResult := DegToGrad(xSrc)
      else if rbDegToRad.Checked then xResult := DegToRad(xSrc)

      else if rbGradToDeg.Checked then xResult := GradToDeg(xSrc)
      else if rbGradToRad.Checked then xResult := GradToRad(xSrc)

      else if rbRadToDeg.Checked then xResult := RadToDeg(xSrc)
      else if rbRadToGrad.Checked then xResult := RadToGrad(xSrc);
    except
      xResult := xSrc;
    end;

    Result := ftos(xResult, spedRound.Value);
    Result := ReplaceFirst(Result, DecSep, cbDecimalSeparator.Text);
  end;

type
  TState = (stNumber, stOther);
var
  slSource, slResult: TStringList;
  i, x: integer;
  SrcLine, ResLine, sNum: string;
  c: Char;
  State: TState;
begin
  seResult.Clear;
  if Trim(seSource.Text) = '' then Exit;

  slSource := TStringList.Create;
  slResult := TStringList.Create;
  try
    slSource.Assign(seSource.Lines);

    for i := 0 to slSource.Count - 1 do
    begin
      SrcLine := slSource[i];
      ResLine := '';
      State := stOther;
      sNum := '';

      for x := 1 to Length(SrcLine) do
      begin
        c := SrcLine[x];

        if CharInSet(c, ['0'..'9', ',', '.']) then State := stNumber
        else State := stOther;

        case State of

          stOther:
            begin
              if sNum = '' then ResLine := ResLine + c
              else
              begin
                ResLine := ResLine + GetNumStr(sNum) + c;
                sNum := '';
              end;
            end;

          stNumber: sNum := sNum + c;

        end;

      end; // for x

      if sNum <> '' then ResLine := ResLine + GetNumStr(sNum);

      slResult.Add(ResLine);

    end; // for i


    seResult.Lines.Assign(slResult);

  finally
    slSource.Free;
    slResult.Free;
  end;

end;

end.

